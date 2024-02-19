library(xgboost)
library(SHAPforxgboost)
library(ggExtra)
library(uwot)
library(glue)
library(pbapply)
library(sjmisc)
library(genieclust)

read_multiple_files <- function(directory,type) {
  files <- (gcs_list_objects(prefix = directory)  %>% filter(size!='0 bytes'))$name
  files=files
  if (type=='Parquet') {
    df <- data.table::rbindlist(future_lapply(files, function(x) read_parquet(gcs_get_object(x))))}
  else {
    df <- data.table::rbindlist(future_lapply(files, function(x) gcs_get_object(x)))
  }
  # system(glue('umount {path}'))
}

apply_vocab_files <- function(directory, df) {
  d <- gcs_list_objects(prefix = directory)
  d <- d %>% filter(size!='0 bytes')
  d <- d$name
  d2 <- gsub(paste0(directory, "vocab__"), "", d)
  for (i in 1:length(d)) {
    print(i)
    tmp <- read_csv(gcs_get_object(d[i]), col_names = F)
    old <- colnames(df %>% select(matches(gsub("_", "[._]", d2[i]))) %>% select(1:nrow(tmp)))
    new <- paste0(d2[i], "_", tmp[, 1, drop = T])
    new <- gsub(" ", "_", new)
    data.table::setnames(df, old, new)
  }
}

get_map <- function(df, n_neighbors = 20, metric = "euclidean",session, ...) {
  progressSweetAlert(session = session, id = 'progressCluster', title = 'Started mapping', display_pct = F, value = 30,total = 100,status = 'success')
  N <- ncol(df)
  embedding <-
    umap(
      df,
      verbose = ifelse(nrow(df) < 100, F, T),
      n_neighbors = max(2, min(n_neighbors, nrow(df) - 1)), metric = metric,
      n_threads = 8, # pca_center = T,pca = min(ncol(df),50),
      n_sgd_threads = 8, # scale = 'Z',
      min_dist = 0,
      init_sdev = 1e-4, init = "random", # set_op_mix_ratio = 0.5,
      spread = 0.3, approx_pow = F,
      n_components = max(2, ceiling(log2(N))), nn_method = "annoy",
      ...
    )
  closeSweetAlert(session = session)
  sendSweetAlert(session = session, title = 'Done mapping!', type = 'success')
  return(embedding)
}

get_clusters <- function(df, num_cluster = 10,session=session) {
  progressSweetAlert(session = session, id = 'progressCluster2', title = 'Started clustering', display_pct = F, value = 50,total = 100,status = 'success')
  clus2 <- genie(df, k = num_cluster, detect_noise = F, gini_threshold = .3)
  cl <- data.frame(cluster = clus2)
  closeSweetAlert(session = session)
  sendSweetAlert(session = session, title = 'Done clustering!', type = 'success')
  return(cl)
}

shap.decision.tree.plot <- function(shap_contrib, ids = 1:nrow(shap_contrib), top_n = ncol(shap_contrib)) {
    as_tibble(shap_contrib[ids, , drop = F]) %>%
    select(order(apply(abs(shap_contrib[ids, , drop = F]), 2, sum), decreasing = T)) %>%
    select(BIAS, everything()) %>%
    select(1:top_n) %>%
    pivot_longer(everything()) %>%
    mutate(group = rep(ids, each = n() / length(ids))) %>%
    group_by(group) %>%
    mutate(value_cum = cumsum(value)) %>%
    dplyr::slice(n():1) %>%
    ggplot() +
    geom_line(aes(as_factor(name), value_cum, group = group, col = value), alpha = 1 / log1p(100 * length(ids)), size = 1.5) +
    coord_flip() +
    # geom_hline(yintercept = 0) +
    labs(title = "SHAP decision plot", x = "Variable name", y = "Model output", col = "Marginals") +
    scale_color_gradient(low = "#FFCC33", high = "#6600CC", guide = guide_colorbar(barwidth = 10, barheight = 0.3)) +
    theme(legend.position = "bottom")
}

shap.individual.waterfall <- function(shap_contrib, id, top_n = 10, df) {
  shap_contrib <- shap_contrib %>%
    as_tibble() %>%
    dplyr::slice(id)
  s <- as_tibble(shap_contrib) %>%
    select(order(apply(abs(shap_contrib), 2, sum), decreasing = T)) %>%
    select(BIAS, everything()) %>%
    pivot_longer(everything()) %>%
    mutate(name = c(name[1:top_n], rep("OTHER", n() - top_n))) %>%
    group_by(name) %>%
    summarise(
      value = sum(value),
      .groups = "drop"
    ) %>%
    arrange(desc(abs(value))) %>%
    mutate(value_cum = cumsum(value)) %>%
    mutate(start = lag(value_cum,
      default = 0
    ), ids = 1:n())
  s$name[-which(s$name %in% c("BIAS", "OTHER"))] <- paste0(s$name[-which(s$name %in% c("BIAS", "OTHER"))], "=", df[id, ] %>%
    select(s$name[-which(s$name %in% c("BIAS", "OTHER"))]))
  s %>% ggplot() +
    geom_rect(aes(
      reorder(name, -abs(value)),
      xmin = ids - 0.45, xmax = ids + 0.45, ymin = start, ymax = value_cum,
      fill = value >= 0
    ), show.legend = F) + geom_hline(yintercept=0) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")) +
    scale_fill_viridis_d() +
    xlab("Variable Name") +
    ylab("Log odds") +
    ggtitle(paste("Id:", id, "SHAP:", round(
      s$value_cum[nrow(s)],
      3
    )))
}

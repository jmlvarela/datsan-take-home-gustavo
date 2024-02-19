# This is a SHAP dashboard for XGBoost models.

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(future)
library(future.apply)
plan(multicore)
source("functions.R")

ui <- dashboardPage(
  dashboardHeader(title = "Model Explainer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Load Model", tabName = "loader", icon = icon("file")),
      menuItem("Explainer", tabName = "explainer", icon = icon("dashboard")),
      menuItem("Experimental", tabName = "advanced", icon = icon("plus")),
      numericInput("nrows", "Maximum number of rows to consider", value = 10000, min = 1, step = 5000),
      checkboxInput('no_missing', label='Remove missing features', value = F),
      sliderInput("num_cluster", label = "Select number of clusters", min = 2, max = 10, value = 10, step = 1),
      actionButton("go_clusters", "Start Clustering!")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "loader",
        fluidRow(
            
          box(title = "Variable Importance", plotOutput("summary_plot", width ="100%", height = 720)),
          useSweetAlert(),
          actionButton(inputId = "go", "Go!", class = "btn btn-success")
          
          
        )
      ),
      tabItem(
        tabName = "explainer",
        fluidRow(
          box(
            title = "Choose data element",
            numericInput("id", "Choose id number", value = 1, min = 1, label = "Choose id number"),
            plotOutput("waterfall", height = 720)
          ),
          box(
            title = "Select Variables to Display",
            selectInput("main_var", choices = "", label = "Main Variable", multiple = F),
            checkboxInput("log10_1", label = "Log scale x axis", value = F),
            box(
              selectInput("interaction_var", choices = "", label = "Interaction Variable", multiple = F),
              checkboxInput("include_interaction", label = "Include in graph", value = F)
            ),
            box(
              selectInput("colour_var", choices = "auto", label = "Colour by", multiple = F),
              checkboxInput("include_colour", label = "Include in graph", value = T)
            ),
            plotOutput("interaction_plot")
          ),
        )
      ),
      tabItem(
        tabName = "advanced",
        fluidRow(
          column(
            width = 6,

            box(
              title = "SHAP Clusters", width = NULL,
              plotOutput("shap_cluster")
            ),
            box(
              title = "Cluster SHAP Mean", width = NULL,
              tableOutput("mean_cluster_shap")
            )
          ),
          uiOutput("decisiontrees"),
          box(
            title = "Dependence plot",
            selectInput("main_var_2", choices = "", label = "Select Variable", multiple = F),
            checkboxInput("log10_2", label = "Log scale x axis", value = F),
            plotOutput("interaction_plot_cluster")
          )

          # ,
        )
      )
    )
  )
)


server <- function(input, output, session) {
  observe({
    query <- parseQueryString(session$clientData$url_search)
    l=c('model_endpoint','data_folder','vocab_folder')#,'file_type','go')
    for (name in l) 
      {
        if (!is.null(query[[name]])) 
        {
          updateTextInput(session = getDefaultReactiveDomain(),name, value = query[[name]])
        }
      }
    if (!is.null(query[['file_type']])) {
      updateSliderTextInput(session,'file_type',selected=query[['file_type']])
      }
    
  })
  
  observeEvent(
    {
      input$go
      # req(input$bucket,input$model_endpoint,input$data_folder,input$nrows)
    },
    {

      # output$selection <- renderPrint(paste(choice_model,choice_data,sep='\n'))
      model <- reactive({
        model <- xgb.load("model.json")
      })
      progressSweetAlert(session = session, id = 'progress', title = 'Loading', display_pct = F, value = 30,total = 100,status = 'success')


      #updateProgressBar(session = session, id= 'progress', title = 'Loading', value=50,total = 100)
      df2 <- reactive({
        df <- data.table::fread("newdata.csv")
        df <- df %>%
          as_tibble() #%>% select(-Response)
        return(df)
      })
      updateProgressBar(session = session, id= 'progress', title='Loading',value=50,total = 100)

      
      closeSweetAlert(session = session)
      sendSweetAlert(session = session, title = 'Data loaded!', type = 'success')


      df <- reactive({
        input$go_clusters
        tmp <- df2()[1:min(isolate(input$nrows), nrow(df2())), ]
        if(input$no_missing){tmp[tmp==-999]=NA}
        return(tmp)
      })

      updateNumericInput(session, inputId = "nrows", max = nrow(df2()))
      updateNumericInput(session, inputId = "id", label = paste("Choose id number between 1 and", input$nrows), max = nrow(df()))

      shap_contrib <- reactive({
        predict=predict(object = model(), newdata = df() %>% select(-contains("Response")) %>% as.matrix(), predcontrib = T)
        return(predict)
      }) # %>% bindCache(model(),df())

      shap_long <- reactive({
        #print("shap_long")
        p <- shap.prep(shap_contrib = shap_contrib() %>% as_tibble() %>% select(-BIAS), X_train = df() %>% select(-contains("Response")))
        if(input$no_missing) {p <- p[rfvalue!=-999]}
        vars <- p[,mean(abs(value)),variable]
        vars <- arrange(vars,desc(V1))
        vars <- vars[1:15,variable]
        p <- p[variable %in% vars]
        p$variable=factor(p$variable) #eliminates unwanted variable names in the factor levels
        return(p)
        })



      output$summary_plot <- renderPlot(expr = {
        shap.plot.summary(data_long = shap_long(), dilute = 5,) + ggtitle("Top Variables") + theme(text = element_text(size = 16))
      })  #%>% bindCache(df(),shap_long())

      updateSelectInput(session, "main_var", choices = unique(shap_long()$variable))
      updateSelectInput(session, "main_var_2", choices = unique(shap_long()$variable))

      updateSelectInput(session, "interaction_var", choices = unique(shap_long()$variable))

      updateSelectInput(session, "colour_var", choices = list(auto='auto',vars=unique(shap_long()$variable)))
      #data_int=reactive({shap.prep.interaction(xgb_model = model(),X_train = df() %>% select(-contains('Response')) %>% as.matrix)})
      output$interaction_plot <- renderPlot({
        shap.plot.dependence(data_long = shap_long(), x = input$main_var, y = if (input$include_interaction) {
          input$interaction_var
        }, 
        #data_int= if(input$include_interaction) {data_int()},
        color_feature = if (input$include_colour) {
          input$colour_var
        }, smooth = F,jitter_width = 0.1,size=2) + {
          if (input$log10_1) scale_x_continuous(trans='log1p')
        }
      })  #%>% bindCache(shap_long(),input$main_var,input$include_interaction,input$interaction_var, input$include_colour, input$colour_var, input$log10_1)

      output$waterfall <- renderPlot({
        shap.individual.waterfall(shap_contrib = shap_contrib(), id = as.numeric(input$id), df = df())
      })


      observeEvent(input$go_clusters, {
        #   get_map_clus <- isolate(reactive({map_cluster(df=shap_contrib(),n_neighbors =16, metric='correlation', session=session, num_cluster=input$num_cluster)}))
        shap_map <- reactive({
           get_map(df = shap_contrib(), n_neighbors = 16, metric = "euclidean",session=session)
         }) # %>% bindCache(shap_contrib())

        #shap_map <- map_clus()[[1]]
        #clusters <- map_clus()[[2]]
         clusters <- reactive({
           get_clusters(df = shap_map(), num_cluster = input$num_cluster,session = session)
         }) #%>%  bindCache(shap_map(),input$num_cluster)

        if (!is_empty(shap_map())) {lapply(1:input$num_cluster, function(i) {
          output[[paste0("trees", i)]] <- renderPlot((shap.decision.tree.plot(shap_contrib = shap_contrib(), ids = which(clusters()$cluster == i), top_n = 10))) #%>% bindCache(shap_contrib(),clusters(),input$decisiontrees)
        })

        output$decisiontrees <- renderUI({
          do.call(tabBox, c(id = "decisiontrees", lapply(1:input$num_cluster, function(i) {
            tabPanel(title = paste0("Cluster_", i), plotOutput(paste0("trees", i)))
          })))
        })

        output$shap_cluster <- renderPlot({(
          shap_map() %>%
            as_tibble() %>%
            ggplot() +
            geom_point(aes(V1, V2, col = as.factor(clusters()$cluster), size = ifelse(paste0("Cluster_", clusters()$cluster) == input$decisiontrees, 2, 0.5)), alpha = 1 / log1p(1 * nrow(df())), show.legend = F)
        )}) #+labs(col='Cluster',size='Selected')})
      }
        output$mean_cluster_shap <- renderTable(
          {
            shap_contrib() %>%
              as_tibble() %>% # dplyr::slice(1:min(10000,nrow(shap_contrib()))) %>%
              rowwise() %>%
              mutate(sumVar = sum(c_across())) %>%
              ungroup() %>%
              mutate(cluster = clusters()$cluster) %>%
              group_by(cluster) %>%
              summarise(meanSHAP = mean(sumVar), pop_pct = n() / nrow(df()))
          },
          #options = list(dom = "t"),
          rownames = F, server=F
        )

        output$interaction_plot_cluster <- renderPlot({

            (
          shap.plot.dependence(data_long = shap_long(), x = input$main_var_2, smooth = F, jitter_width=0.1) +
            {
              if (input$log10_2) scale_x_continuous(trans = 'log1p')
            } + geom_jitter(aes(col = ifelse(paste0("Cluster_", clusters()$cluster) == input$decisiontrees, 1, 0), size = ifelse(paste0("Cluster_", clusters()$cluster) == input$decisiontrees, 2, 0.5)), width=0.1, alpha = 1 / log1p(1 * nrow(df()))) +
            guides(size = F)
        )})
      })
    }
  )
}

shinyApp(ui, server)

# Take-home Assignment

You are a Data Scientist working in Anti-Financial Crime and you have been tasked with building a prototype model for identifying suspicious transaction activity within your financial institution. You are provided two files for the task:

- _`party.csv`_ - which contains information about the bank customers and whether their activity has been considered suspicious.
- _`tx.csv`_ - a list of transactions

From these data you are to perform your analysis using any approach you like and address the questions below in a short report (max 1000 words).

When we meet to discuss your report we will use those questions as guides for the interview so be prepared to explain your choices in detail. You will also note that a fraction of _`suspicious_label`_ are missing data. We encourage you to treat these as your test set and we will also discuss your models performance based on the true value of those labels.

Note that the data are simulated and their properties are based on publicly available test data.

## Questions and guidance
1. You need to combine data across the files. How have you chosen to do so and why?
2. What modelling approaches have you considered and why did you choose your final model? How is that model parameterised and how should one interpret the predictions?
3. There are fewer  suspicious than non-suspicious labels. Does this affect your analyses and, if so, what steps have you taken to address this effect?
4. Are you concerned about your model overfitting and have you taken any steps to avoid overfitting?
5. If you wished to continue your analyses and modelling, what would be your next steps or experiments?
6. If your prototype needed to be put into production what additional steps or tasks would be required? Consider both the data science and software engineering perspectives when addressing this question. 
7. Are there any ethical concerns related to using these data? 


## Submission instructions
Please create a branch named _`submission`_, and commit and push:
1) your report addressing the guidance and questions above (in any format you see fit)
2) your code (in any language you see fit)
3) your predictions for the test set. Call the file _`test-predictions.csv`_ with the columns _`id`_ and _`score`_.

When your are happy with your submission, please create a pull request.

## Data dictionary

**party.csv:**

    - id = customer id
    - type = customer type, e.g. private person, sole proprietor, or business corporation
    - age = age of customer (person or corporation)
    - postal_code = area of residence for private persons and registration for corporations
    - suspicious_label = customers that have been found to be suspicious

**tx.csv:**

    - id = customer id
    - tx = transaction amounts
    - date = date of transaction

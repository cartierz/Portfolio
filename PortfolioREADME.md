# Portfolio
Work Samples

## Author

**Cartier Zhi**

- Email: cartier.jen.zhi@gmail.com

## Description
This repo is dedicated to work samples relating to data science and analytics completed. It contains zip files with R and Python code for different projects. 
Each zip contains the necessary files needed for each project. 

### Portfolio Structure 
    ├── fda_at3_datacleaning.zip - data cleaning and use of ovun.sample for unbalanced classes
    ├── MLAA_AT2.zip - gradient boosted machine ML technique for classification modelling of credit risk (team: Jean Koh and Navid Mehnati)
    ├── STDS_AT1_Rvignette_2021.Rmd - Guided Visualisations of Numeric Data in Exploratory Data Analysis: featuring ggplot (in RMarkdown)
    ├── Guided Visualisations ggplot.html - Guided Visualisations of Numeric Data in Exploratory Data Analysis: featuring ggplot (as a html file)
    ├── currency_converter_program_at1_dsp.zip - currency converter program (see below for more details)
    ├── MLAA_AT1B_classification_model_repurchase.zip - logistic regression classification model for customer repurchase data
    ├── MLAA_text_mining.zip - analyses corpus (docs) to gain insights into the content and themes within

### Currency Converter
The currency converter is able to return the exchange rate between two currency codes inputted into the terminal as follows:
`python3 main.py AUD EUR`,
`pipenv run python3 main.py AUD EUR`,
It is able to return any currency that is available from the open-source API: https://www.frankfurter.app/.
Frankfurter API tracks foreign exchange references rates published by the European Central Bank. The data refreshes around 16:00 CET every working day.

#### Packages

- requests
- datetime

#### Currency Converter Structure

    ├── api.py             <- Calling the specified API endpoints
    ├── currency.py        <- Check input validity, store results & format final outputs
    ├── main.py            <- Main script for currency code inputs & displaying results
    ├── Pipfile            <- Required language version, packages and package source
    ├── Pipfile.lock       <- Details of the environment with package and version details
    ├── README.md          <- Details about files in directory, with student's details
    ├── test_api.py        <- Testing code from api.py
    └── test_currency.py   <- Testing code from currency.py

# Fame & Partners Analytics
## eCommerce Performance Dashboard: `/ecommerce-performance/`
Here is a Shiny Application hosted at https://fameandpartners.shinyapps.io/ecommerce-performance/

[Shiny](https://shiny.rstudio.com/) is a framework for building interactive dashboards in R.  

#### Run development server
`cd ecommerce-performance`

Add your copy of the `fp_init.R` file that manages the database connection

`$ R`

`>shiny::runApp()`

#### Deploy app to shinyapps.io
Get a login to Fame & Partners' account and follow these [instructions](https://shiny.rstudio.com/articles/shinyapps.html) to configure your rsconnect folder.  Once your account's credentials are configured, run

`$ R`

`>rsconnect::deployApp()`

## Data Warehouse: `/dw/`
### Requirements
- `python 3.6.1`
- `pip 9.0.1 (python 3.6)`
- `R 3.4.2`

To install python dependencies run
`$ cd dw`
`$ pip install -r requirements.txt`

To install R dependencies run
`$ cd dw/Rscripts`
`$ Rscript dependencies.R`

`/dw/` manages F&P's Data Warehouse.  It uses both R and Python to run ETL jobs.  It relies on [feather](https://github.com/wesm/feather) to exchange Data Frames between R and Python in the data pipelines.  The entry point to these ETL jobs are bash scripts, so to run the hourly ETL job is this command

`$ sh hourly.sh`

or to run the culling algorithm is this command

`$ sh culling.sh`

## Analysis Presentations: `/analyses/`
These are scripts written for analysis presentations.  These scripts use ggplot2 heavily and were written to be run locally and generate files for analysis presentations.

## Ad-Hoc R/Python ETL Scripts: `/etl/`
Here are ETL scripts written for ad-hoc report requests or to manually update CSV files in `/ecommerce-performance/static-data/`.  All of these scripts were written to be run locally.  And most should be thought of as scratch paper.  None of these scripts should be run in production.

## Ad-Hoc SQL Queries: `/misc-sql/`
Here are SQL queries written for ad-hoc reports or quick test and experiments. It is essentially SQL scratch paper. Nothing in this folder should be used for any production or mission-critical applications.

## Legacy Repeat Customers Dashboard: `/repeat-customers/`
This is an old Shiny dashboard that has been archived on shinyapps.io and is not used anymore.

## "Static" Data Folders
- `/ecommerce-performance/static-data/`
- `dw/Rscripts/static-data/`

Both of these `static-data` folders are lookup tables that either shiny or the data warehouse needs to reference.

A copy of the Static Data folder can be found in [here in dropbox](https://www.dropbox.com/sh/837pif44bgtiij4/AADA_hOPiT5xZBDf7jn7GMz9a?dl=0)

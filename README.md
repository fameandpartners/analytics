# Fame & Partners Analytics

## eCommerce Performance Dashboard: `/ecommerce-performance/`
Here is a Shiny Application hosted at https://fameandpartners.shinyapps.io/ecommerce-performance/

[Shiny](https://shiny.rstudio.com/) is a framework for building interactive dashboards in R.  

`cd ecommerce-performance`

Add your copy of the `fp_init.R` file that manages the database connection

#### Run development server
`$ R`

`>shiny::runApp()`

To deploy this app to shinnyapps.io first get a login to Fame & Partners' account and follow these [instructions](https://shiny.rstudio.com/articles/shinyapps.html) to configure your rsconnect folder.  Once your account's credentials are configured, run

#### Deploy app to shinyapps.io
`$ R`

`>rsconnect::deployApp()`

## Data Warehouse: `/dw/`
This manages F&P's Data Warehouse.  It uses both R and Python to run ETL jobs.  It relies on [feather](https://github.com/wesm/feather) to exchange Data Frames between R and Python in the data pipelines.  The entry point to these ETL jobs are bash scripts, so run the hourly ETL job run

`$ sh hourly.sh`

## Analysis Presentations: `/analyses/`
These are scripts written for analysis presentations.  These scripts use ggplot2 heavily and were written to be run locally and generate files for analysis presentations.

## Ad-Hoc R/Python ETL Scripts: `/etl/`
Here are ETL scripts written for ad-hoc report requests or to manually update CSV files in `/ecommerce-performance/static-data/`.  All of these scripts were written to be run locally.  And most should be thought of as scratch paper.  None of these scripts should be run in production.

## Ad-Hoc SQL Queries: `/misc-sql/`
Here are SQL queries written for ad-hoc reports or quick test and experiments. It is essentially SQL scratch paper. Nothing in this folder should be used for any production or mission-critical applications.

## Legacy Repeat Customers Dashboard: `/repeat-customers/`
This is an old Shiny dashboard that has been archived on shinyapps.io and is not used anymore.

<!-- README.md is generated from README.Rmd. Please edit that file -->
Shiny chart builder
===================

v0.1

This shiny app means to be a system for basic reporting in the style of most Business Intelligence tools, you can create a report, and then share it with the bookmark, without knowing any SQL or R.

This package heavily relies on dplyr for database abstraction, it theoretically works with any dplyr-compatible database, but may require some tuning for some of the databases.

The flow
--------

The way you should use this app is to build your chart with the `Sample mode`, and when you have the visualization you want, you untick the sample mode, which goes to the database to fetch the complete dataset you need. The app does some tricks with dplyr to avoid over-querying data.

![Scatterplot](images/scatter.gif)

![Linechart](images/line.gif)

Configuration
-------------

The app has to be configured by placing 2 files on the root of the project: config.R and update\_samples.R. Example files using sqlite have been provided in the examples folder.

Before using the shiny app, you have to execute the script "update\_samples.R", which will download samples of all tables (Might take a while on big databases). If you only want to query a subset of your tables, modify this script so it only finds those tables.

This script should be reran ocasionally, depending on how much your database changes, maybe daily, weekly or monthly, use your job scheduler or cron to do execute this script.

Also, the stack overflows easily because of the level of recursion used, on the server or machine where you deploy this, you should allow for big stack sizes, i've tried and it worked fine with the unlimited setting in my experience.

    ulimit -s unlimited

Missing features
----------------

This is a very preliminar release, a lot of things may be missing, pull requests are welcome!

Some examples of missing features:

-   Some advanced settings to control appearance
-   Bar charts
-   Histograms
-   Faceting?
-   More database examples
-   Bookmarking charts (It fails with the default bookmarker)

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

library(shiny)
# server <- (Sys.info()["nodename"] == "Lucky1304")
# server <- FALSE
shiny::runApp('app', launch.browser=TRUE, port=6149)
# To stop the Shiny server, click the red stop sign in the 'Console window.


# Deploy to shinyapps.io if the change is stable.  See https://github.com/rstudio/shinyapps/blob/master/guide/guide.md
# shinyapps::deployApp()

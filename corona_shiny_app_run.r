library(shiny)
runApp("corona_shiny")


# Deploy the app to shinyapp.io
library(rsconnect)
deployApp("corona_shiny")
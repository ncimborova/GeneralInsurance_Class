install.packages("shiny")
install.packages("dplyr")
install.packages("ggplot2")

library(shiny)
library(dplyr)
library(ggplot2)

data <- read.csv("./Data/lesson2_KPI.csv") %>% filter_all(all_vars(!is.na(.)))

ui <- fluidPage(
  titlePanel("Plot"),
  selectInput("select", label = NULL, 
              choices = list("Region", "Unit", "Segment", "Business", "Year")),
  plotOutput(outputId = "Graph"))


server <- function(input, output)
{
  output$lesson3_KPI_multidim_UWR_graph <- renderPlot({
    ggplot(data = data, 
           mapping = aes_string(x = "Premium", y = "Expenses", colour = input$select)) +
      geom_point(aes_string(x = data$Premium, y = data$Expenses, colour = input$select)) +
      geom_smooth(aes_string(x = data$Premium, y = data$Expenses, colour = input$select), 
                  method = loess, se = FALSE)
  })
}

shinyApp(ui, server)

#Unit7 obsahuje najrizikovejšie podniky
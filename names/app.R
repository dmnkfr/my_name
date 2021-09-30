#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


unique_names = names %>% drop_na() %>% select(name) %>% distinct() %>%  arrange(name)

ui = fluidPage(
    # Application title
        titlePanel("Given names in Austria between 1984 and 2020"),
        
        sidebarLayout(
            sidebarPanel(
                selectInput("name", "Name:", choices = unique_names, selected = "Dominik"),
                selectInput("measure", "Measure:", choices = c("Rank", "Absolute", "Percent"), selected = "Absolute")

                        ),
        
            # Show a plot of the generated distribution
            mainPanel(
               plotOutput("name_plot")
                    )
    
                    )
  )
  
server = function(input, output) {
        output$name_plot = renderPlot({
            
            input_name = input$name
            input_measure = tolower(input$measure)

        names %>%
            drop_na() %>% 
            filter(name == input_name) %>% 
                ggplot(aes(x = year, y = get(input_measure))) +
                    geom_line(color = my_red, size = 1.5) +
                    labs(title = paste0("Popularity of the name ", input_name, " \nin Austria per year"), 
                         y = input_measure, 
                         x = "Year") +
                    xlim(1980,2020) +
                    theme_ipsum() 
              
      
    })
  }

shinyApp(ui, server)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(bslib)
library(hrbrthemes)
hrbrthemes::import_roboto_condensed()

my_red = "#DC2F1E"

data = readRDS("all_name_data.RDS")
unique_names = readRDS("unique_names.RDS") %>% arrange(name)

ui = fluidPage(
        theme = bs_theme(
                  bg = "#252933", 
                  fg = "white", 
                  primary = "white"
    ),
    # Application title
        titlePanel("Popularity of names in Austria between 1984 and 2020"),
        
        sidebarLayout(
            sidebarPanel(
                selectInput("name", "Name:", choices = unique_names, selected = "Dominik"),
                selectInput("measure", "Measure:", choices = c("Rank", "Absolute", "Percent"), selected = "Absolute")

                        ),
        
            # Show a plot of the generated distribution
            mainPanel(
               plotOutput("name_plot"),              
               HTML(
                 paste(
                  h6("Note: Only names that were among the top 120 names in any year between 1984 and 2020 are in the list."),'<br/>',
                  '<a href="https://www.statistik.at/web_de/statistiken/menschen_und_gesellschaft/bevoelkerung/geborene/vornamen/index.html">Data from Statistik Austria.</a>', '<br/>',
                  '<a href="https://dmnkfr.github.io/my_name/">For more info, visit https://dmnkfr.github.io/my_name/</a>', '<br/>',
                  '<a href="https://dmnkfr.netlify.app/">For questions or feedback, please do not hesitate to contact me via https://dmnkfr.netlify.app/</a>', '<br/>'
                      )
                  )
                    )
                    )
  )
  
server = function(input, output) {
        output$name_plot = renderPlot({
            
            input_name = input$name
            input_measure = tolower(input$measure)
            count = data %>% filter(name == input_name) %>% nrow()
        
        if (count == 1){
        
          data %>%
            drop_na() %>% 
            filter(name == input_name) %>% 
                ggplot(aes(x = year, y = get(input_measure))) +
                    geom_point(color = my_red, size = 6) +
                    labs(title = paste0("Popularity of the name ", input_name, " in Austria per year"),
                         y = input_measure, 
                         x = "Year") +
                    xlim(1984,2020) +
                    theme_ft_rc()
          
        } else {
          data %>%
            drop_na() %>% 
            filter(name == input_name) %>% 
                ggplot(aes(x = year, y = get(input_measure))) +
                    geom_line(color = my_red, size = 1.5) +
                    labs(title = paste0("Popularity of the name ", input_name, " in Austria per year"),
                         y = input_measure, 
                         x = "Year") +
                    xlim(1984,2020) +
                    theme_ft_rc()
        }
    })
  }

shinyApp(ui, server)
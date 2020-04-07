#
library(shiny)
library(tidyverse)

shinyServer(function(input, output) {
    random_data <- reactive({ 
        tibble(
            x = 1:input$points,
            y = x + rnorm(input$points, input$mean, input$stddev)
        )
    })
    
    
    output$cars_plot <- renderPlot({
        p <- random_data() %>%
            ggplot(aes(x, y)) +
            geom_point() +
            geom_smooth(method = 'lm') +
            geom_abline(intercept = 0, slope = 1, colour = 'red') +
            labs(
                x = 'X Axis',
                y = 'Y',
                colour = '# Cylinders'
            )
        
        return(p)
    })

})

    #
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Linear Regression Analysis"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h1('Documentation'),
            p('This application helps you to see how changing the number of points, and
            the mean and standard deviation of the random error component changes a linear regression.
            
            Use the sliders below to change these values. The graph on the right shows the points,
            the fitted curve (in black), the confidence interval (in gray), and the true f(x).'),
            
            h1('Controls'),
            sliderInput('points', "Number of points:", 0, 2000, value = 100, step = 100),
            sliderInput('mean', "Mean:", 0, 100, value = 10, step = 10),
            sliderInput('stddev', "Standard Deviation:", 0, 1000, value = 100, step = 10)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("cars_plot")
        )
    )
))

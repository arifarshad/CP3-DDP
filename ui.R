

library(shiny)
shinyUI(fluidPage(
  titlePanel("Wine Quality Predictor"),
  sidebarLayout(
    sidebarPanel(
      h5("Move the Slider!"),
      submitButton("Apply Changes"),
      sliderInput("slider1", "Fixed Acidity",
                  3.8, 15.9, value=10.05),
      sliderInput("slider2", "Volatile Acidity", 
                  0.085, 1.58, 0.8325),
      sliderInput("slider3", "Citric Acid",
                  0, 1.23, 0.615),
      sliderInput("slider4", "Residual Sugar",
                  0.6, 31.6, 4.4),
      sliderInput("slider5", "Chlorides",
                  0.012, 0.611, 0.3115),
      sliderInput("slider6", "Free Sulfur Dioxide",
                  1.0, 122.5, 61.75),
      sliderInput("slider7", "Total Sulfur Dioxide",
                  6,294,150),
      sliderInput("slider8", "Density",
                  0.98711, 1.01030, 0.998705),
      sliderInput("slider9", "pH",
                  2.74, 4.01, 3.375),
      sliderInput("slider10", "Sulphates", 
                  0.26, 2, 1.13),
      sliderInput("slider11", "Alcohol", 
                  8.0, 14.9, 11.45)
      
      
    ),
    mainPanel(
      
      h5("Documentation: The App is used to predict wine quality.  You can also view the distribution of predictors by wine type.  Use the boxplots to help you adjust the levels to get better wine.  You will find that making virtual wine is about as easy as making regular wine.  Ratings are between 1 (lowest) and 9 (highest)."),
      h3("Wine Quality"),
      textOutput("text"),
 
    # Show a plot of the  distribution

      selectInput("variable", "Variable:",
                  c("Fixed.Acidity", 
                    "Volatile.Acidity",
                    "Citric.Acid",
                    "Residual.Sugar",
                    "Chlorides",
                    "Free.Sulfur.Dioxide",
                    "Total.Sulfur.Dioxide",
                    "Density", "pH",
                    "Sulphates", "Alcohol"), selected="Alcohol"),
      submitButton("Choose to display!"),          
      
      plotOutput("plot")
 
      )
    )
))

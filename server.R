
# shiny functions
library(shiny)



shinyServer(function(input, output) {
  ## download
  red <- "http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
  white <- "http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
  download.file(red, destfile="./winequality-red.csv")
  download.file(white, destfile="./winequality-white.csv")
  
  ## wine data
  red <- read.csv("winequality-red.csv", sep=";")
  white <- read.csv("winequality-white.csv", sep=";")
  
  library(caret)
  white_intrain <- createDataPartition(y=white$quality,
                                       p = 0.3, list=FALSE)
  
  training_white <- white[white_intrain,]
  na.omit(training_white)
  
  
  red$quality <- droplevels(as.factor(red$quality))
  training_white$quality <- droplevels(as.factor(training_white$quality))
  
  
  names <- colnames(red)
  df <- rbind(red, training_white)
  colnames(df) <- names
  df$quality <- as.factor(df$quality)
  na.omit(df)
  red$kind <- rep("red", times=nrow(red))
  training_white$kind <- rep("white", times=nrow(training_white))

  
  
  
  
  

  
  set.seed(999999)
  
  ## fit model
  library(kernlab)
  fit <- ksvm(quality~., data=df)
  

  
  
  
  
   pq <- reactive({
    one <- input$slider1
    two <- input$slider2
    three <- input$slider3
    four <- input$slider4
    five <- input$slider5
    six <- input$slider6
    seven <- input$slider7
    eight <- input$slider8
    nine <- input$slider9
    ten <- input$slider10
    eleven <- input$slider11
    
    
 
  predict(fit, newdata=data.frame(fixed.acidity=one,
                                    volatile.acidity=two,
                                    citric.acid=three,
                                    residual.sugar=four,
                                    chlorides=five,
                                    free.sulfur.dioxide=six,
                                    total.sulfur.dioxide=seven,
                                    density=eight,
                                    pH=nine,
                                    sulphates=ten,
                                    alcohol=eleven))
  })
  output$text <- renderText({
    pq()

    })
  red$kind <- rep("red", times=nrow(red))
  training_white$kind <- rep("white", times=nrow(training_white))
  df2 <- rbind(red, training_white)
  df2$kind <- as.factor(df2$kind)
  df2$quality <- as.factor(df2$quality)
  colnames(df2) <- c("Fixed.Acidity", 
                     "Volatile.Acidity",
                     "Citric.Acid",
                     "Residual.Sugar",
                     "Chlorides",
                     "Free.Sulfur.Dioxide",
                     "Total.Sulfur.Dioxide",
                     "Density", "pH",
                     "Sulphates", "Alcohol",
                     "Quality", "Kind")
  library(ggplot2)
  
  
  output$plot <- renderPlot({
    f <- input$variable
    if(f=="Fixed.Acidity"){
      ggplot(data=df2, aes(x=Quality, y=Fixed.Acidity,fill=Kind)) + geom_boxplot()
    } else if(f=="Volatile.Acidity"){
      ggplot(data=df2, aes(x=Quality, y=Volatile.Acidity,fill=Kind)) + geom_boxplot()
    } else if(f=="Citric.Acid"){
      ggplot(data=df2, aes(x=Quality, y=Citric.Acid,fill=Kind)) + geom_boxplot()
    } else if(f=="Residual.Sugar"){
      ggplot(data=df2, aes(x=Quality, y=Residual.Sugar,fill=Kind)) + geom_boxplot()
    } else if(f=="Chlorides"){
      ggplot(data=df2, aes(x=Quality, y=Chlorides,fill=Kind)) + geom_boxplot()
    } else if(f=="Free.Sulfur.Dioxide"){
      ggplot(data=df2, aes(x=Quality, y=Free.Sulfur.Dioxide, fill=Kind)) + geom_boxplot()
    } else if(f=="Total.Sulfur.Dioxide"){
      ggplot(data=df2, aes(x=Quality, y=Total.Sulfur.Dioxide, fill=Kind)) + geom_boxplot()
    } else if(f=="Density"){
      ggplot(data=df2, aes(x=Quality, y=Density,fill=Kind)) + geom_boxplot()
    } else if(f=="pH"){
      ggplot(data=df2, aes(x=Quality, y=pH,fill=Kind)) + geom_boxplot()
    } else if(f=="Sulphates"){
      ggplot(data=df2, aes(x=Quality, y=Sulphates,fill=Kind)) + geom_boxplot()
    } else if(f=="Alcohol"){
      ggplot(data=df2, aes(x=Quality, y=Alcohol,fill=Kind)) + geom_boxplot()
    }

  })


})





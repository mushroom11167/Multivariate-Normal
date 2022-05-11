library(shiny)
library(mvtnorm)
library(ggplot2)



ui <- fluidPage(
  
  # Application title
  titlePanel("Multivariate Normal"),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("k",
                  "Class:",
                  min = 3,
                  max = 6,
                  value = 3),
      
      sliderInput("p",
                  "p:",
                  min = 2,
                  max = 6,
                  value = 3),
      
      sliderInput("my_range",
                  "Select range:(p)",
                  min = 1,
                  max = 10,
                  value = 1),
      radioButtons("balance", "Balance or not:",
                  c("TRUE"=TRUE,"FALSE"=FALSE)),
      textInput("balance1", "Enter a vector", "100,100,100")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput('scatterPlot')
    )
  )
)

server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    
    if(input$balance==TRUE){
      n <- rep(100,input$k)
    }else{
      n <- as.numeric(unlist(strsplit(input$balance1,",")))
    }
    
    
    df <- data.frame()
    mymean <- c()
    
    for(i in 1:input$k){
      
      my_mean <- round(
        runif(input$p, -input$my_range, input$my_range), #setting mean's range
        3)
      
      mymaen <- rbind(mymean, my_mean)
      
      y <- rmvnorm(n[i], mean = my_mean)
      
      
      df <- rbind(df, y)
    }
    
    from <- 1
    if(input$balance == TRUE){
      for(i in 1:input$k){
        
        to <- i*100 #
        df[from:to,input$p+1] <- i
        from <- to+1
      }
    }else{
      for(i in 1:input$k){
        
        to <- sum(n[1:i]) 
        df[from:to,input$p+1] <- i
        from <- sum(n[1:i])+1
      }
    }

    
    
    df_pca <- prcomp(formula = ~.,
                     data    = df[,-input$p+1],
                     scale.  = TRUE)
    
    df[,input$p+1] <- as.factor(df[,input$p+1])
    qplot(df_pca$x[, 1], df_pca$x[, 2], color=df[,input$p+1], main="PCA")+ theme_bw()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

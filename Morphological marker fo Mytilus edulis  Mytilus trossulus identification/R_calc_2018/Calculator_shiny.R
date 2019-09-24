library(gridExtra)
library(ggplot2)

# Функция для вычисления P_MT по P_T

calculator1 <- function(a, b, c, d, P_T_in){
  P_T <- (a+c)/(a+b+c+d)
  P_T_ME <- c/(c+d)
  P_T_MT <- a/(a+b)
  P_MT <- (P_T_in - P_T_ME)/(P_T_MT - P_T_ME)
  P_MT
}



calculator2 <- function(a, b, c, d, P_T_in){
  P_T <- (a+c)/(a+b+c+d)
  P_E <- (b+d)/(a+b+c+d)
  P_MT_T <- a/(a+c)
  P_MT_E <- b/(b+d)
  
  P_MT <- P_MT_T * P_T_in + P_MT_E * P_E
  P_MT
}


calculator3 <- function(a, b, c, d, P_T_in){
  P_T <- (a+c)/(a+b+c+d)
  P_E <- (b+d)/(a+b+c+d)
  
  P_MT.E <- b/(a+b+c+d)
  P_ME.T <- c/(a+b+c+d)
  
  
  P_MT <- P_T_in + P_MT.E - P_ME.T
  P_MT
}





P_MT_predictions <- data.frame(P_T = seq(0, 1, 0.01))


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Калькуляторы"),
  
  sidebarLayout(
    sidebarPanel(
      #Задаем параметр a
      sliderInput(inputId = "a",
                  label = "MT with T-morphotype (a):",
                  min = 0,
                  max = 1000,
                  step = 1,
                  value = 500), #То с чего начинается
      
      sliderInput(inputId = "b",
                  label = "MT with E-morphotype (b):",
                  min = 0,
                  max = 1000,
                  step = 1,
                  value = 0), #То с чего начинается
      
      
      sliderInput(inputId = "c",
                  label = "ME with T-morphotype (c):",
                  min = 0,
                  max = 1000,
                  step = 1,
                  value =0), 
      
      sliderInput(inputId = "d",
                  label = "ME with E-morphotype (d):",
                  min = 0,
                  max = 1000,
                  step = 1,
                  value = 500) #То с чего начинается
      
      
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("my_graph")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$my_graph <- renderPlot({
    
    #Здесь может быть код 

    P_MT_predictions$P_MT_calc1 <- calculator1(a=input$a, b=input$b, c=input$c, d=input$d, P_T_in = P_MT_predictions$P_T )

    P_MT_predictions$P_MT_calc2 <- calculator2(a=input$a, b=input$b, c=input$c, d=input$d, P_T_in = P_MT_predictions$P_T )
  
    P_MT_predictions$P_MT_calc3 <- calculator3(a=input$a, b=input$b, c=input$c, d=input$d, P_T_in = P_MT_predictions$P_T )
    
    #Функция рисующая график 

    ggplot(P_MT_predictions, aes(x = P_T)) + 
      geom_line(aes(y = P_MT_calc1), color = "black") + 
      geom_line(aes(y = P_MT_calc2), color = "blue") + 
      geom_line(aes(y = P_MT_calc3), color = "gray") + 
      ylim(0,1)
        
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

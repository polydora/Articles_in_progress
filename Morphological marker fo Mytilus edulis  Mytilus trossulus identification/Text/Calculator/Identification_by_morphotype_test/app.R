
library(gridExtra)
library(ggplot2)
library(shiny)
library(ggrepel)



# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Mytilus trossulus and Mytilus edulis identification by morphotype-test"),
  
  sidebarLayout(
    sidebarPanel(
            
      #
      sliderInput(inputId = "P_T_MT",
                  label = "Frequency of T-morphotype among M.trossulus in calibrating samples",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  value = 0.75), 
      
      #
      sliderInput(inputId = "P_E_ME",
                  label = "Frequency of E-morphotype among M.edulis in calibrating samples",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  value = 0.90),
      
      #Задаем параметр P_T
      sliderInput(inputId = "P_T",
                  label = "Proportion of T-morphotype in population of interest:",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  value = 0.5) #То с чего начинается
      
      
      
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("my_graph", height = "700px")
    )
  )
)

# Define server logic required to draw 

server <- function(input, output) {
  
  output$my_graph <- renderPlot({
    
    #Здесь может быть код 
    #Функция рисующая график 

    # Ptros = (PT – (1 - P(E|edu)))/ (P(T|tros) -  (1 -P(E|edu)) 
    # P_T_MT = 0.7
    # P_E_ME = 0.99
    # P_T = 0.5
    # P(tros|T) = Ptros*P(T|tros)/(1-Ptros)*(1 - P(E|edu)) + Ptros*P(Т|tros)   
    # P(edu|E) = (1-Ptros)*P(E|edu)/(1-Ptros)*P(E|edu) + Ptros*(1 - P(Т|tros) )     
    
    Ptros_sample <- (input$P_T - (1 - input$P_E_ME)) / (input$P_T_MT - (1-input$P_E_ME))
    P_MT_T_sample <- Ptros_sample*input$P_T_MT/((1-Ptros_sample)*(1 - input$P_E_ME) + Ptros_sample*input$P_T_MT)
    P_ME_E_sample <- (1 - Ptros_sample)*input$P_E_ME/((1-Ptros_sample)*input$P_E_ME + Ptros_sample * (1- input$P_T_MT))


    # Ptros_sample <- (P_T - (1 - P_E_ME)) / (P_T_MT - (1-P_E_ME))
    # P_MT_T_sample <- Ptros_sample*P_T_MT/((1-Ptros_sample)*(1 - P_E_ME) + Ptros_sample*P_T_MT)
    # P_ME_E_sample <- (1 - Ptros_sample)*P_E_ME/((1-Ptros_sample)*P_E_ME + Ptros_sample * (1- P_T_MT)) 
    # 
    
        
    df_sample <- data.frame(Ptros = c(Ptros_sample, Ptros_sample), Sp = c("M.trossulus", "M.edulis"), P = c(P_MT_T_sample, P_ME_E_sample ))
    
    
    
    df_bayes <- data.frame(Ptros = seq(0, 1, by = 0.01))
    
    df_bayes$P_MT_T <- df_bayes$Ptros * input$P_T_MT /((1-df_bayes$Ptros)*(1 - input$P_E_ME) + df_bayes$Ptros*input$P_T_MT) 
    df_bayes$P_ME_E <- (1 - df_bayes$Ptros)*input$P_E_ME/((1-df_bayes$Ptros)*input$P_E_ME + df_bayes$Ptros * (1- input$P_T_MT)) 
    
    
    
    ggplot(df_bayes, aes(x = Ptros)) + 
      geom_line(aes(y = P_MT_T), color = "red", size = 1) + 
      geom_line(aes(y = P_ME_E), color = "blue", size = 1)  +
      geom_point(data = df_sample, aes(x = Ptros, y = P, fill = Sp), positon = position_jitter(), shape = 21, size = 4) +
      geom_text(data = df_sample, aes(x = Ptros, y = P + 0.02, label = round(P, 2)) ) + 
      scale_fill_manual(values = c("blue", "red")) +
      labs(fill = "Probability of correct identification") + 
      theme(legend.position = "bottom")
    
    # +
    #   geom_vline(xintercept = min) + 
    #   geom_vline(xintercept = --input$P_E_ME)


    
    
  })
}




# Run the application 
shinyApp(ui = ui, server = server)

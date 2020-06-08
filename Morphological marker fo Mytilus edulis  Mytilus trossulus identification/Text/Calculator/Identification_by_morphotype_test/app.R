
library(gridExtra)
library(ggplot2)
library(shiny)
library(ggrepel)



# Define UI for application 
ui1 <- fluidPage(
  
  # Application title
  titlePanel("Assessment of population structure and individual identification probability by morphotype-test"),
  
  sidebarLayout(
    sidebarPanel(
            
      #
      sliderInput(inputId = "P_T_MT",
                  label = "Frequency of T-morphotype among M.trossulus in calibrating samples (P(T|tros) for max different populations)",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  value = 0.75), 
      
      #
      sliderInput(inputId = "P_T_ME",
                  label = "Frequency of T-morphotype among M.edulis in calibrating samples (P(T|edu) for max different populations)",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  value = 0.05),
      
     
      #
      sliderInput(inputId = "P_T_MT2",
                  label = "Frequency of T-morphotype among M.trossulus in calibrating samples (P*(T|tros) for  mixed populations)",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  value = 0.75), 
      
      #
      sliderInput(inputId = "P_T_ME2",
                  label = "Frequency of T-morphotype among M.edulis in calibrating samples (P*(E|edu) for  mixed populations)",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  value = 0.05),
      
      fluidRow("Desirable probability of correct species identification",
        column(width = 6,
               selectInput("Dtros", "for M.trossulus", seq(0.5, 1, 0.01), selected = 0.75)
        ),
        column(width = 6, 
               selectInput("Dedu", "for M.edulis", seq(0.5, 1, 0.01), selected = 0.75)
        )
      ),
      
      
      #Задаем параметр P_T
      sliderInput(inputId = "P_T",
                  label = "Frequency of T-morphotype in population of interest:",
                  min = 0,
                  max = 1,
                  step = 0.001,
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
    
    #########################
    #Пороговые значения Ptros и PT для выбора миногного морфотипа
    
    D_tros <- as.numeric(input$Dtros)
    
    Ptros_minor_tros <- (-D_tros)*input$P_T_ME2/(D_tros*input$P_T_MT2 - D_tros*input$P_T_ME2 - input$P_T_MT2)
    
    PT_minor_tros <- Ptros_minor_tros * (input$P_T_MT - input$P_T_ME) + input$P_T_ME
    #
    
    D_edu <- as.numeric(input$Dedu)
    
    Ptros_minor_edu <- (-D_edu + D_edu * input$P_T_ME2 + 1 - input$P_T_ME2)/(D_edu * input$P_T_ME2 - D_edu * input$P_T_MT2 + 1 - input$P_T_ME2)
    
    PT_minor_edu <- Ptros_minor_edu * (input$P_T_MT - input$P_T_ME) + input$P_T_ME
    

    
    Ptros_sample <- (input$P_T -  input$P_T_ME) / (input$P_T_MT - input$P_T_ME)
    P_MT_T_sample <- Ptros_sample*input$P_T_MT2/((1-Ptros_sample)*input$P_T_ME2 + Ptros_sample*input$P_T_MT2)
    P_ME_E_sample <- (1 - Ptros_sample)*(1-input$P_T_ME2)/((1-Ptros_sample)*(1-input$P_T_ME2) + Ptros_sample * (1- input$P_T_MT2))

    min_P_T_sample <- input$P_T_ME
    max_P_T_sample <- input$P_T_MT
    
   
    
    
    # min_Ptros_sample <- (min_P_T_sample - (1 - input$P_E_ME)) / (input$P_T_MT - (1-input$P_E_ME))
    # max_Ptros_sample <- (max_P_T_sample - (1 - input$P_E_ME)) / (input$P_T_MT - (1-input$P_E_ME))
    
    
    # Ptros_sample <- (P_T - (1 - P_E_ME)) / (P_T_MT - (1-P_E_ME))
    # P_MT_T_sample <- Ptros_sample*P_T_MT/((1-Ptros_sample)*(1 - P_E_ME) + Ptros_sample*P_T_MT)
    # P_ME_E_sample <- (1 - Ptros_sample)*P_E_ME/((1-Ptros_sample)*P_E_ME + Ptros_sample * (1- P_T_MT)) 
    # 
    
        
    df_sample <- data.frame(Ptros = c(Ptros_sample, Ptros_sample), Sp = c("M.trossulus", "M.edulis"), P = c(P_MT_T_sample, P_ME_E_sample ))
    
    
    
    df_bayes <- data.frame(Ptros = seq(0, 1, by = 0.01), P_T = seq(0, 1, by = 0.01))
    

    df_bayes$P_MT_T <- df_bayes$Ptros * input$P_T_MT2 /((1-df_bayes$Ptros)*input$P_T_ME2 + df_bayes$Ptros*input$P_T_MT2) 
    df_bayes$P_ME_E <- (1 - df_bayes$Ptros)*(1-input$P_T_ME2)/((1-df_bayes$Ptros)*(1-input$P_T_ME2) + df_bayes$Ptros * (1- input$P_T_MT2)) 
    
    
    df_bayes$Ptros_predicted <- (df_bayes$P_T - input$P_T_ME) / (input$P_T_MT - input$P_T_ME)
    
    
Pl_congr <-  ggplot(df_bayes, aes(x = Ptros)) + 
      geom_line(aes(y = P_MT_T), color = "red", size = 1) + 
      geom_line(aes(y = P_ME_E), color = "blue", size = 1)  +
      geom_point(data = df_sample, aes(x = Ptros, y = P, fill = Sp), positon = position_jitter(), shape = 21, size = 4) +
      geom_text_repel(data = df_sample, aes(x = Ptros, y = P, label = round(P, 2)), direction = "both", point.padding = 1) + 
      scale_fill_manual(values = c("blue", "red")) +
      labs(y = "Probability of correct identification", x = "Mytilus trossulus prevalence", fill = "Species") + 
      theme(legend.position = "bottom") + 
      xlim(0, 1) +
      ylim(0, 1) +
  geom_hline(yintercept = 0.5, linetype = 2)+
      ggtitle("Individual mussel identification")
  

   
Pl_ptros <- ggplot(df_bayes, aes(x = P_T, y = Ptros_predicted)) + 
  geom_line(size = 1, color = "darkgray") + 
  xlim(0,1) + 
  ylim(0, 1) + 
  geom_point(x = input$P_T, y = Ptros_sample, size = 4) + 
  geom_text(x = input$P_T+0.1, y = Ptros_sample + 0.02, aes(label = paste("Ptros = ", round(Ptros_sample, 2) ))) + 
  labs(x = "Frequency of T-morphotype", y ="Predicted frequency of Mytilus trossulus") +
  geom_abline(linetype = 2, size = 0.5) +
  geom_point(x = min_P_T_sample, y = 0, shape = 25, size = 5, fill = "yellow") + 
  geom_text(x = min_P_T_sample + 0.1, y = 0, label = paste("min P_T = ", round(min_P_T_sample, 2) ), hjust = "left" ) +
  geom_point(x = max_P_T_sample, y = 1, shape = 24, size = 5, fill = "yellow") + 
  geom_text(x = max_P_T_sample - 0.1, y = 1, label = paste("max P_T = ", round(max_P_T_sample, 2) ), hjust = "right" )+
  ggtitle("Taxonomical structure of mixed population")


#пририсовываем пороговые значения, отсекающие мнорный морфотип

Pl_ptros <- Pl_ptros + 
  geom_vline(xintercept = PT_minor_tros, linetype = 2, color = "red") +
  geom_vline(xintercept = PT_minor_edu, linetype = 2, color = "blue") +
  geom_text(x = PT_minor_tros-0.05, y = 0.75, label = paste("min PT \nfor Desirable\nprobability\nPT = ", round(PT_minor_tros, 2)), color = "red") +
  geom_text(x = PT_minor_edu+0.05, y = 0.25, label = paste("max PT \nfor Desirable\nprobability\nPT = ", round(PT_minor_edu, 2)), color = "blue")
  

Pl_congr <- Pl_congr + 
  geom_vline(xintercept = Ptros_minor_tros, linetype = 2, color = "red")+
  geom_vline(xintercept = Ptros_minor_edu, linetype = 2, color = "blue") + 
  geom_segment(x = Ptros_minor_tros, xend = Ptros_minor_tros-0.05, y = 0.2, yend = 0.2, color = "red", arrow =  arrow(angle = 10,type = "closed", ends = "last")) + 
  geom_text(x = Ptros_minor_tros-0.08, y = 0.2, label = paste("Minor \nSpecies\nPtros < ", round(Ptros_minor_tros, 2)), color = "red")+ 
  geom_segment(x = Ptros_minor_edu, xend = Ptros_minor_edu + 0.05, y = 0.2, yend = 0.2, color = "blue", arrow =  arrow(angle = 10,type = "closed", ends = "last"))+ 
  geom_text(x = Ptros_minor_edu + 0.08, y = 0.2, label = paste("Minor \nSpecies\nPtros > ", round(Ptros_minor_edu, 2)), color = "blue")

grid.arrange(Pl_ptros, Pl_congr, nrow = 2)

    
  })
}






# Run the application 
shinyApp(ui = ui1, server = server)

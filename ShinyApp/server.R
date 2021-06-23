#############################################
# Install packages if they are not installed yet
# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
#ipak <- function(pkg){
#  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#  if (length(new.pkg)) 
#    install.packages(new.pkg, dependencies = TRUE)
#  sapply(pkg, require, character.only = TRUE)
#}
#packages <- c("caret", "randomForest", "data.table", "shiny", "shinythemes", "shinydashboard")
#ipak(packages)

# Load library
library(caret) 
library(randomForest)
library(data.table)
library(shiny)
library(shinythemes)
library(shinydashboard)
#############################################

#Load data and trained model
load("www/train4.Rdata")
load("www/test4.Rdata")
load("www/MeOxRFModel.Rdata")
load("www/Performance4.Rdata")
load("www/Descriptors4.Rdata")



server <- function(input, output) {

  output$Performance4 <- renderTable({
    data.frame("Parameter" =  c("R2 test", "RMSE test","MAE test", "R2 train", "RMSE train", "MAE train", "R2 cross validation","RMSE cross validation","MAE cross validation"),
               "Value" = c(Performance4$R2_test[1], Performance4$RMSE_test[1], Performance4$MAE_test[1],Performance4$R2_train[1], Performance4$RMSE_train[1], Performance4$MAE_train[1],Performance4$R2_CV[1], Performance4$RMSE_CV[1], Performance4$MAE_CV[1]))
  }, digits = 3)
  
  #make table of user input data of MeOx based nano-mixtures
  output$MeOxdatainput <- renderTable({
    data.frame("Parameter" = c("MeOxNP",
                           "MeOx core diameter (nm)",
                           "MeOx concentration (ug/L)",
                           "Mixed chemical",
                           "Mixed chemical concentration (ug/L)",
                           "Exposure time (h)"),
               "Value" = c(input$MeOx,
                           input$CSize,
                           input$C1,
                           input$mixchem,
                           input$C2,
                           input$ETime)
               )
  })
  
  
  #make table of predicted data  
  output$Prediction4 <- renderValueBox({
    a <- which(apply(as.data.frame(Descriptors4[,2]), 1, function(r) any(r %in% input$mixchem)))
    b <- which(apply(as.data.frame(Descriptors4[,2]), 1, function(r) any(r %in% input$MeOx)))
    Cmix <- input$C1 + input$C2
    Mol1 <- input$C1/Descriptors4[b,3]/(input$C1/Descriptors4[b,3]+input$C2/Descriptors4[a,3])
    Mol2 <- 1-Mol1
    table1 <- data.frame("Concentration_Mat1" = input$C1,
                         "ExposureTime" = input$ETime,
                         "Concentration_Mat2" = input$C2,
                         "CoreSize"   = input$CSize,
                         "ExposureTime"  = input$ETime,
                         "Eta_Bmix9"     = sqrt(Mol1*abs(Descriptors4[b,11])*Mol2*abs(Descriptors4[a,11])),
                         "SpAbs_B_m_mix9"= sqrt(Mol1*abs(Descriptors4[b,4])*Mol2*abs(Descriptors4[a,4])),
                         "Immobilization"= input$Endpoint)

    Imm_MeOxDmix <- predict(MeOxRFmodel, table1)

    color_MeOxDmix <- fifelse(Imm_MeOxDmix <= 25, "green", 
                    fifelse(Imm_MeOxDmix <= 50, "yellow",
                        fifelse(Imm_MeOxDmix <= 75, "red","red"))) 
    icon_MeOxDmix <- fifelse(Imm_MeOxDmix <= 25, "fas fa-envira","exclamation-triangle")
    title_MeOxDmix <- paste(round(Imm_MeOxDmix, digits = 1), " %", sep = "" )
    subtitle_MeOxDmix <- p("of ",em("Daphnia magna")," population might be immobilized")
    
    valueBox(title_MeOxDmix, subtitle_MeOxDmix, icon = icon(icon_MeOxDmix), color = color_MeOxDmix)
  })
  
  
  #make table of predicted data
  output$LinearityMeOx <- renderPlot({
    a <- which(apply(as.data.frame(Descriptors4[,2]), 1, function(r) any(r %in% input$mixchem)))
    b <- which(apply(as.data.frame(Descriptors4[,2]), 1, function(r) any(r %in% input$MeOx)))
    Cmix <- input$C1 + input$C2
    Mol1 <- input$C1/Descriptors4[b,3]/(input$C1/Descriptors4[b,3]+input$C2/Descriptors4[a,3])
    Mol2 <- 1-Mol1
    table1 <- data.frame("Concentration_Mat1" = input$C1,
                         "ExposureTime" = input$ETime,
                         "Concentration_Mat2" = input$C2,
                         "CoreSize"   = input$CSize,
                         "ExposureTime"  = input$ETime,
                         "Eta_Bmix9"     = sqrt(Mol1*abs(Descriptors4[b,11])*Mol2*abs(Descriptors4[a,11])),
                         "SpAbs_B_m_mix9"= sqrt(Mol1*abs(Descriptors4[b,4])*Mol2*abs(Descriptors4[a,4])),
                         "Immobilization"= input$Endpoint)
    
    table2 <- data.frame("X" = input$Endpoint, "Y" = predict(MeOxRFmodel, table1))
    train4$Predicted.Immobilization <- predict(MeOxRFmodel, train4)
    test4$Predicted.Immobilization <- predict(MeOxRFmodel, test4)
    #Draw plots for linearity
    p1 <- ggplot(train4, aes(x=Immobilization, y=Predicted.Immobilization)) + geom_point(aes(x=Immobilization, y=Predicted.Immobilization), color="red", alpha=1.0, size = 4, shape=1) +
      theme_bw() +
      xlab(expression(paste("Observed immobilization (%)"))) +
      ylab(expression(paste("Predicted immobilization (%)"))) +
      xlim(0,100) +
      ylim(0,100) +
      theme(axis.ticks = element_blank(),
            axis.text = element_text(size=16),
            axis.title = element_text(size=24),
            axis.title.x = element_text(size=24),
            axis.ticks.length = unit(0.0, "cm"),
            legend.text=element_text(size=16),
            legend.position="top",
            legend.direction="horizontal",
            legend.box = "vertical",
            plot.margin = unit(c(0.5, 0.5, 0.1, 0.1), "cm"),
            panel.spacing = unit(c(0.01, 0.01, 0.01, 0.01), "cm")) 
    p2 <- p1 + geom_point(data=test4, aes(x=Immobilization, y=Predicted.Immobilization), colour="black", alpha=1.0, size = 4, shape=1) +
      annotate("text", x = 15, y = 90, label = "Train set") + 
      annotate("text", x = 15, y = 80, label = "Test set") +
      annotate("text", x = 15, y = 70, label = "Prediction") +
      annotate("pointrange", x = 5, y = 90, ymin = 90, ymax = 90, colour = "red", size = 1.0, shape=1)+
      annotate("pointrange", x = 5, y = 80, ymin = 80, ymax = 80, colour = "black", size = 1.0, shape=1)+
      annotate("pointrange", x = 5, y = 70, ymin = 70, ymax = 70, colour = "blue", size = 2.0, shape=18)+
      geom_line(data=data.frame("X"=c(0,100), "Y"=c(0,100)),aes(x=X, y=Y), colour="black", alpha=1.0) +
      geom_point(data=table2, aes(x=X, y=Y), colour="blue", alpha=1.0, size = 10, shape=18)
    p2
  })
  
  
  
  
}


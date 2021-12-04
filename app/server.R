library(shiny)
library(shinydashboard)

shinyServer(function(input, output){
  
  output$freqPlot <- renderPlot({
    #Column name variable
    cat_val = ifelse(input$cat == 'Age', 'AGE',
                     ifelse(input$cat == 'DSM Diagnosis', 'DSMCRIT',ifelse(input$cat == 'Education', 'EDUC',
                                   ifelse(input$cat == 'Employment Status', 'EMPLOY',
                                          ifelse(input$cat == 'Length of stay', 'LOS',
                                          ifelse(input$cat == 'Ethnicity', 'ETHNIC',
                                                 ifelse(input$cat == 'Frequency of use (primary)', 'FREQ1',
                                                        ifelse(input$cat == 'Frequency of use (secondary)', 'FREQ2',
                                                               ifelse(input$cat == 'Frequency of use (tertiary)', 'FREQ3',
                                                                      ifelse(input$cat == 'Age of first use (primary)', 'FRSTUSE1',
                                                                             ifelse(input$cat == 'Age of first use (secondary)', 'FRSTUSE2',
                                                                                    ifelse(input$cat == 'Age of first use (tertiary)', 'FRSTUSE3',
                                                                                           ifelse(input$cat == 'Gender', 'GENDER',
                                                                                                  ifelse(input$cat == 'Living Status', 'LIVARAG',
                                                                                                         ifelse(input$cat == 'Marital Status', 'MARSTAT',
                                                                                                                ifelse(input$cat == 'Medicated assisted opioid therapy', 'METHUSE',
                                                                                                                       ifelse(input$cat == 'Previous trestment', 'NOPRIOR',
                                                                                                                              ifelse(input$cat == 'Source of referral', 'PSOURCE',
                                                                                                                                     ifelse(input$cat == 'Race', 'RACE',
                                                                                                                                            ifelse(input$cat == 'Route of intake (primary)', 'ROUTE1',
                                                                                                                                                   ifelse(input$cat == 'Route of intake (secondary)', 'ROUTE2',
                                                                                                                                                          ifelse(input$cat == 'Route of intake (tertiary)', 'ROUTE3',
                                                                                                                                                                 ifelse(input$cat == 'Service type', 'SERVICES',
                                                                                                                                                                 ifelse(input$cat == 'Substance (primary)', 'SUB1',
                                                                                                                                                                        ifelse(input$cat == 'Substance (secondary)', 'SUB2',
                                                                                                                                                                               ifelse(input$cat == 'Substance (tertiary)', 'SUB3',
                                                                                                                                                                                      ifelse(input$cat == 'Veteran status', 'VET',
                                                                                                                                                                                             ifelse(input$cat == 'Alcohol and other drugs', 'ALCDRUG',
                                                                                                                                                                                             ifelse(input$cat == 'Treatment complete (y/n)', 'REASON', 'REASON' )))))))))))))))))))))))))))))
                        
    
    #Frecuency plot
    ggplot(data = patientData, aes(x = patientData[[cat_val]]))+
      geom_bar(stat = 'count', fill = 'mediumseagreen', 
               width = 0.5)+
      stat_count(geom = 'text', size = 4,
                 aes(label = ..count..),
                 position = position_stack(vjust = 1.03))+
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face="bold"))+
      labs(title = sprintf('Frecuency plot of the variable %s', cat_val),
           x = sprintf('%s', input$cat), y = 'Count')
    
  })
  
  
  #Prediction model
  #React value when using the action button
  a <- reactiveValues(result = NULL)
  
  observeEvent(input$cal, {
    #Copy of the test data without the dependent variable
    test_pred <- test_set[-30]
    
    #Dataframe for the single prediction
    values1 = data.frame(CASEID = as.integer(input$p_id), AGE = input$p_age, ALCDRUG = input$p_alcother,   
                        DSMCRIT = input$p_dsm, EDUC = input$p_educ, EMPLOY = input$p_emp, ETHNIC = input$p_ethnic, 
                        FREQ1 = input$p_feq1, FREQ2 = input$p_feq2, FREQ3 = input$p_feq3, FRSTUSE1 = input$p_frst1,
                        FRSTUSE2 = input$p_frst1, FRSTUSE3 = input$p_frst1, GENDER = input$p_gender, LIVARAG = input$p_liv, 
                        LOS = input$p_los, MARSTAT = input$p_mar, METHUSE = input$p_meth, NOPRIOR = input$p_np,
                        PSOURCE = input$p_psource, RACE = input$p_race, ROUTE1 = input$p_rou1, ROUTE2 = input$p_rou2,
                        ROUTE3 = input$p_rou3, SERVICES = input$p_ser, SUB1 = input$p_sub1, SUB2 = input$p_sub2, SUB3 = input$p_sub3, VET = input$p_vet)
    
    
    
    #Include the values into the new data
    #test_pred <- rbind(test_pred, values)
    
    
    #Single prediction using the randomforest model
    #a$result <-  predict(rf, newdata = test_pred[nrow(test_pred),])
    
    print(values1)     
    res <- predict(rf, newdata = values1)
    res <- as.numeric(res)
    print(res)
  
    a$result <- ifelse(sum(res) == 2, 'Success', 'Fail')
  })
  

  output$value <- renderText({
    #Display the prediction value
    paste(a$result)
  })
  
  
  
  
  
  
  
})
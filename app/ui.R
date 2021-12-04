library(shiny)
library(shinydashboard)

#Data Preparation

#mae_rf = mae(test_set[[30]], rf_pred_both)
#rmse_rf = rmse(test_set[[30]], rf_pred_both)

shinyUI(
  dashboardPage(
    dashboardHeader( title = "Menu Bar"),
    dashboardSidebar(
      sidebarMenu(
      menuItem("Plots", tabName = "plot",icon = icon("dashboard")),
        #menuSubItem("Data set"),
      menuItem("Model Predictions", tabName = "mp", icon = icon("search"))
      
    )),
    dashboardBody(
      tabItems(
        tabItem(tabName = "plot", h2("Visualize Dataset"),
                #Frequency plot filter
                box(status = 'primary', title = 'Filter for the frequency plot',
                    selectInput('cat', 'Categorical variables:', 
                                c('Age', 'DSM Diagnosis', 'Education', 'Employment Status', 'Ethnicity', 'Frequency of use (primary)', 'Frequency of use (secondary)', 'Frequency of use (tertiary)', 
                                  'Age of first use (primary)','Age of first use (secondary)', 'Age of first use (tertiary)', 'Gender', 'Living Status', 'Length of stay', 'Marital Status', 'Medicated assisted opioid therapy', 
                                  'Previous trestment','Source of referral', 'Race', 'Route of intake (primary)', 'Route of intake (secondary)', 'Route of intake (tertiary)', 
                                  'Service type', 'Substance (primary)', 'Substance (secondary)', 'Substance (tertiary)', 'Veteran status', 'Alcohol and other drugs','Treatment complete (Y/N)')),
                    footer = 'Frequency plot for categorical variables'),
                #Boxes to display the plot
                box(plotOutput('freqPlot'))),
        
        tabItem(tabName = "mp", h2("Model Predictions"),
                        #Filters for categorical variables
                        box(title = 'Patient Characteristics', status = 'primary', width = 12, collapsible = TRUE, collapsed = TRUE,
                            splitLayout(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                        cellWidths = c('0%', '20%', '2%', '19%', '4%', '19%', '4%', '20%', '1%', '8%'),
                
                                        fluidRow(column( width = 10,   numericInput('p_id', 'CaseId', 0),
                                        div(),
                                        
                                        selectInput('p_educ', 'Education', c('No school/ kg – grade 8', 'Grades 9 – 11', 'Grade 12/(GED)','1-3 years of college', '4+ years of college')))),
                                        div(),
                                        
                                        fluidRow(column( width = 10, selectInput('p_age', 'Age', c("12-14", "15-17","18-20","21-24","25-29","30-34","35-39","40-44",
                                                                       "45-49","50-54","55-64","65 >")),
                                        div(),
                                        
                                        selectInput('p_mar','Marital Status', c('Never married', 'Now married', 'Separated', 'Widowed/Divorced')))),
                                        div(),
                                        fluidRow(column( width = 10,  selectInput('p_race', 'Race', c('Alaska Native','American Indian/other',
                                                                        'Asian or Pacific','Black or African American','White','Asian','Other single races',
                                                                        'Two or more races','Native Hawaiian')),
                                        div(),
                                         selectInput('p_ethnic','Ethnicity', c('Puerto Rican', 'Mexican', 'Cuban', 'Not Hispanic/Latino', 'Hispanic/Latino')))),
                                        div(),
                                        
                                        fluidRow(column( width = 10,  selectInput('p_emp','Employment Status', c('Full time', 'Part time', 'Unemployed', 'Not in labor')),
                                       
                                        selectInput('p_liv','Living Arrangement', c('Homeless','Dependent Living','Independent Living')))),
                                        div(),
                                        fluidRow(column( width = 10, radioButtons('p_gender', 'Gender', c('Male', 'Female')) ,
                                        div(),
                                        radioButtons('p_vet','Veteran', c('Yes', 'No')))))),
                
                        #Filters for numeric variables
                        box(title = 'Treatment Characteristics', status = 'primary', width = 12, collapsible = TRUE, collapsed = TRUE,
                            splitLayout(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                        cellWidths = c('0%', '25%', '2%', '19%', '4%', '15%', '4%', '15%', '1%', '16.6%'),
                                        
                                        fluidRow(column( width = 10, sliderInput('p_los', 'Length of stay', min = 1, max = 100, value = 0),  
                                        div(),
                                        
                                        selectInput('p_ser', 'Services', c('Detox 24 hours Inpatient', 'Detox 24-hours free-standing',
                              'Rehab non detox', 'short-term < 30', 'long-term > 30',
                              'Intensive outpatient', 'Non-Intensive outpatient','Detoxification')),
                                        div(),
                              
                                        
                                        selectInput('p_psource', 'Referral source', c('Individual (includes self- referral)','Alcohol/drug use care provider','Other health care provider','School','Employee','Community referral', 'Court/criminal/DWI/DUI')),
                                        div(),
                              
                                        selectInput('p_dsm', 'DSM diagnosis', c('Alcohol-induced disorder','Substance-induced disorder','Alcohol intoxication','Alcohol dependence','Opioid dependence','Cocaine dependence','Cannabis dependence','Other substance dependence','Alcohol abuse','Cannabis abuse')))),
                                        div(),
                                        
                                        fluidRow(column( width = 10, selectInput('p_sub1', 'Sub type 1', c('None','Alcohol','Cocaine/Crack','Marijuana','Heroin','Other opiates/synthetics','Methamphetamine')),
                                        div(),
                                        selectInput('p_sub2', 'Sub type 2', c('None','Alcohol','Cocaine/Crack','Marijuana','Heroin','Other opiates/synthetics','Methamphetamine')),
                                        div(),
                                        selectInput('p_sub3', 'Sub type 3', c('None','Alcohol','Cocaine/Crack','Marijuana','Heroin','Other opiates/synthetics','Methamphetamine')),
                                        div(),
                                        selectInput('p_np', 'Treatment history', c('No prior treatment','1 or more treatments')))),
                                        div(),
                                        
                                        fluidRow(column( width = 10, selectInput('p_rou1', 'Route 1', c('Oral','Smoking','Inhalation','Injection', 'Other')),
                                        div(),
                                                         
                                        selectInput('p_rou2', 'Route 2', c('Oral','Smoking','Inhalation','Injection', 'Other')),
                                        div(),
                                                         
                                        selectInput('p_rou3', 'Route 3', c('Oral','Smoking','Inhalation','Injection', 'Other')),
                                        div(),
                                        
                                        selectInput('p_meth', 'Medicated opoid therapy', c('Yes', 'No')))),
                                        div(),
                                        
                                        fluidRow(column( width = 10, selectInput('p_feq1', 'Freq 1', c('No use', 'Some use', 'Daily use')),
                                        div(),
                                        
                                        selectInput('p_feq2', 'Freq 2', c('No use', 'Some use', 'Daily use')),
                                        div(),
                                        
                                        selectInput('p_feq3', 'Freq 3', c('No use', 'Some use', 'Daily use')),
                                        div(),
                                        
                                        selectInput('p_alcother', 'Alcohol/Other drugs', c('None','Alcohol only','Other drugs only','Both')))),
                                        div(),
                                        
                                        fluidRow(column( width = 10, selectInput('p_frst1', 'First-use age 1', c('11 years and under','12–14 years','15–17 years','18–20 years','21–24 years', '25–29 years', '30–95 years')),
                                         div(),
                                                         
                                        selectInput('p_frst2', 'First-use age 2', c('11 years and under','12–14 years','15–17 years','18–20 years','21–24 years', '25–29 years', '30–95 years')),
                                        div(),
                                                         
                                        selectInput('p_frst3', 'First-use age 3', c('11 years and under','12–14 years','15–17 years','18–20 years','21–24 years', '25–29 years', '30–95 years')))),
                                        div() )),
                                        
                                        
                                                                            
                                
          
                #Box to display the prediction results
                box(title = 'Prediction result', status = 'success', solidHeader = TRUE, width = 4, height = 260,
                    div(h5('Final Prediction:')),
                    verbatimTextOutput("value", placeholder = TRUE),
                    actionButton('cal','Calculate', icon = icon('calculator')))
               
                
                ))
                # 
                # 
    )
  )
)
library(shiny)
library(ggplot2)
library(shinydashboard)
library(lpSolve)
library(reshape2)
library(shinyWidgets)

F_min <- data.frame(A = c(100,30,25,15,8,2),
                    B = c(75,40,30,20,15,5),
                    C = c(100,50,35,25,15,5),
                    D = c(100,60,50,40,25,10),
                    E = c(100,100,55,40,20,6),
                    `F` = c(100,100,10,55,30,8))

F_max <- data.frame(A = c(100,65,55,40,20,8),
                    B = c(90,75,60,45,30,15),
                    C = c(100,85,65,50,30,15),
                    D = c(100,100,85,70,45,25),
                    E = c(100,100,100,100,50,20),
                    `F` = c(100,100,100,100,70,25))

ui <- dashboardPage(
  dashboardHeader(title = "EstabGran"),
  dashboardSidebar(
    tags$h5(tags$strong('Propriedades da Mistura Final'), align='center'),
    numericInput('n_mat', 'Qtd Materiais',3,2,5,1, width='100%'),
    sliderInput('LL_max','Limite de Liquidez Máx',5,40,25,1),
    sliderInput('IP_max','Índice de Plasticidade Máx',1,20,6,1),
    sliderInput('R','Razão entre # 200 e #40',0,1,2/3,0.01),
    selectInput('Faixa','Faixa granulométrica desejada',choices=c('A','B','C','D','E','F','Curva Adaptada')),
    actionButton("curva_adapt", "Definir Curva Adaptada"),
    hr(),
    actionButton('help', label="Entenda o Modelo", 
                 icon = icon("calculator"), 
                 onclick ="window.open('https://rpubs.com/pedreirajr/EstabGran')")
    
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML('
      .form-group, .selectize-control {
           padding-top: 0px;
           margin-bottom: 10px;
      }
      .box-body {
          padding-top: 0px;
          padding-bottom: 0px;
      }'))),
    
    uiOutput("boxes"),
    
    
   fluidRow(
      column(6,offset = 5,
             actionBttn('otim_mist', label = 'Gerar Mistura Ótima',
                        icon('arrow-circle-down'), style='material-flat',
                        color = 'success')
      )
    ), #Row
    
    hr(),
    div(style = "margin-top:-2em", 
        uiOutput('OtimResult'),
        )  

  ) #dashboardBody
) #dashboardPage



server <- function(input, output) {
# Boxes ####  
  output$boxes <- renderUI(if(input$n_mat == 2){
   fluidRow(
    tabBox(
      title = '', id = 'tabbox2', height = 400, width = 7,
      
      #|--TabPanel1
      tabPanel(tags$h5('Material 1', align = 'center'), 
          tags$table(style="width:100%",
                     tags$tr(width = "50%",
                             tags$td(width = "50%", 
                                     textInput('nome1','Nome do Material',value = "", width = '95%')),
                             tags$td(width = "50%", 
                                     sliderInput('custo1','Custo Unitário (R$/m³)',0,250,25,1, width = '95%'))
                     )
          ),
          tags$strong('% Passante:'),
          tags$table(style="width:100%",
                     tags$tr(width = "30%",
                             tags$td(width = "15%", numericInput('p11','# 1\"',100,0,100,1,width = '75%')),
                             tags$td(width = "15%", numericInput('p12','# 3/8\"',30,0,100,1,width = '75%')),
                             tags$td(width = "15%", numericInput('p13','# Nº 4',1.6,0,100,1,width = '75%')),
                             tags$td(width = "15%", numericInput('p14','# Nº 10',0.7,0,100,1,width = '75%')),
                             tags$td(width = "15%", numericInput('p15','# Nº 40 ',0.7,0,100,1,width = '75%')),
                             tags$td(width = "15%", numericInput('p16','# Nº 200',0.6,0,100,1,width = '75%')),
                     )
          ),
          tags$table(style="width:100%",
                     tags$tr(width = "50%",
                             tags$td(width = "50%",
                                     sliderInput('LL1','Limite de Liquidez (# Nº 40):',0,80,0,1, width = '95%')),
                             tags$td(width = "50%", 
                                     sliderInput('IP1','Índice de Plasticidade (# Nº 40):',0,80,0,1, width = '95%'))
                     )
          )
        ), #TabPanel1--|
    
    #|--TabPanel2
    tabPanel(tags$h5('Material 2', align = 'center'), 
        tags$table(style="width:100%",
                   tags$tr(width = "50%",
                           tags$td(width = "50%", 
                                   textInput('nome2','Nome do Material',value = "", width = '95%')),
                           tags$td(width = "50%", 
                                   sliderInput('custo2','Custo Unitário (R$/m³)',0,250,25,1, width = '95%'))
                   )
        ),
        tags$strong('% Passante:'),
        tags$table(style="width:100%",
                   tags$tr(width = "30%",
                           tags$td(width = "15%", numericInput('p21','# 1\"',100,0,100,1,width = '75%')),
                           tags$td(width = "15%", numericInput('p22','# 3/8\"',100,0,100,1,width = '75%')),
                           tags$td(width = "15%", numericInput('p23','# Nº 4',100,0,100,1,width = '75%')),
                           tags$td(width = "15%", numericInput('p24','# Nº 10',40,0,100,1,width = '75%')),
                           tags$td(width = "15%", numericInput('p25','# Nº 40 ',22,0,100,1,width = '75%')),
                           tags$td(width = "15%", numericInput('p26','# Nº 200',6,0,100,1,width = '75%')),
                   )
        ),
        tags$table(style="width:100%",
                   tags$tr(width = "50%",
                           tags$td(width = "50%", 
                                   sliderInput('LL2','Limite de Liquidez (# Nº 40):',0,80,0,1, width = '95%')),
                           tags$td(width = "50%", 
                                   sliderInput('IP2','Índice de Plasticidade (# Nº 40):',0,80,0,1, width = '95%'))
                   )
        )
      ) #TabPanel2--|
    ),
    column(width = 5,
           plotOutput('CurvMat')
    )
  )
    
  } else if(input$n_mat==3) {
    fluidRow(
      tabBox(
        title = '', id = 'tabbox3', height = 400, width = 7,
        #|--TabPanel1
        tabPanel(tags$h5('Material 1', align = 'center'), 
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            textInput('nome1','Nome do Material',value = "", width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('custo1','Custo Unitário (R$/m³)',0,250,25,1, width = '95%'))
                            )
                 ),
                 tags$strong('% Passante:'),
                 tags$table(style="width:100%",
                            tags$tr(width = "30%",
                                    tags$td(width = "15%", numericInput('p11','# 1\"',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p12','# 3/8\"',30,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p13','# Nº 4',1.6,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p14','# Nº 10',0.7,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p15','# Nº 40 ',0.6,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p16','# Nº 200',0.6,0,100,1,width = '75%')),
                            )
                 ),
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%",
                                            sliderInput('LL1','Limite de Liquidez (# Nº 40):',0,80,0,1, width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('IP1','Índice de Plasticidade (# Nº 40):',0,80,0,1, width = '95%'))
                            )
                 )
        ), #TabPanel1--|
        
        #|--TabPanel2
        tabPanel(tags$h5('Material 2', align = 'center'), 
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            textInput('nome2','Nome do Material',value = "", width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('custo2','Custo Unitário (R$/m³)',0,250,25,1, width = '95%'))
                            )
                 ),
                 tags$strong('% Passante:'),
                 tags$table(style="width:100%",
                            tags$tr(width = "30%",
                                    tags$td(width = "15%", numericInput('p21','# 1\"',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p22','# 3/8\"',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p23','# Nº 4',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p24','# Nº 10',40,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p25','# Nº 40 ',22,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p26','# Nº 200',6,0,100,1,width = '75%')),
                            )
                 ),
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            sliderInput('LL2','Limite de Liquidez (# Nº 40):',0,80,0,1, width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('IP2','Índice de Plasticidade (# Nº 40):',0,80,0,1, width = '95%'))
                            )
                 )
        ), #TabPanel2--|
       
        #|--TabPanel3
        tabPanel(tags$h5('Material 3', align = 'center'), 
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            textInput('nome3','Nome do Material',value = "", width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('custo3','Custo Unitário (R$/m³)',0,250,25,1, width = '95%'))
                            )
                 ),
                 tags$strong('% Passante:'),
                 tags$table(style="width:100%",
                            tags$tr(width = "30%",
                                    tags$td(width = "15%", numericInput('p31','# 1\"',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p32','# 3/8\"',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p33','# Nº 4',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p34','# Nº 10',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p35','# Nº 40 ',85,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p36','# Nº 200',16,0,100,1,width = '75%')),
                            )
                 ),
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            sliderInput('LL3','Limite de Liquidez (# Nº 40):',0,80,29,1, width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('IP3','Índice de Plasticidade (# Nº 40):',0,80,8,1, width = '95%'))
                            )
                 )
        ) #TabPanel3--|
      ),
      column(width = 5,
        plotOutput('CurvMat')
      )
    )
  } else if(input$n_mat==4) {
    fluidRow(
      tabBox(
        title = '', id = 'tabbox4', height = 400, width = 7,
        #|--TabPanel1
        tabPanel(tags$h5('Material 1', align = 'center'), 
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            textInput('nome1','Nome do Material',value = "", width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('custo1','Custo Unitário (R$/m³)',0,250,25,1, width = '95%'))
                            )
                 ),
                 tags$strong('% Passante:'),
                 tags$table(style="width:100%",
                            tags$tr(width = "30%",
                                    tags$td(width = "15%", numericInput('p11','# 1\"',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p12','# 3/8\"',30,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p13','# Nº 4',1.6,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p14','# Nº 10',0.7,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p15','# Nº 40 ',0.7,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p16','# Nº 200',0.6,0,100,1,width = '75%')),
                            )
                 ),
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%",
                                            sliderInput('LL1','Limite de Liquidez (# Nº 40):',0,80,0,1, width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('IP1','Índice de Plasticidade (# Nº 40):',0,80,0,1, width = '95%'))
                            )
                 )
        ), #TabPanel1--|
        
        #|--TabPanel2
        tabPanel(tags$h5('Material 2', align = 'center'), 
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            textInput('nome2','Nome do Material',value = "", width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('custo2','Custo Unitário (R$/m³)',0,250,25,1, width = '95%'))
                            )
                 ),
                 tags$strong('% Passante:'),
                 tags$table(style="width:100%",
                            tags$tr(width = "30%",
                                    tags$td(width = "15%", numericInput('p21','# 1\"',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p22','# 3/8\"',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p23','# Nº 4',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p24','# Nº 10',40,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p25','# Nº 40 ',22,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p26','# Nº 200',6,0,100,1,width = '75%')),
                            )
                 ),
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            sliderInput('LL2','Limite de Liquidez (# Nº 40):',0,80,0,1, width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('IP2','Índice de Plasticidade (# Nº 40):',0,80,0,1, width = '95%'))
                            )
                 )
        ), #TabPanel2--|
        
        #|--TabPanel3
        tabPanel(tags$h5('Material 3', align = 'center'), 
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            textInput('nome3','Nome do Material',value = "", width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('custo3','Custo Unitário (R$/m³)',0,250,25,1, width = '95%'))
                            )
                 ),
                 tags$strong('% Passante:'),
                 tags$table(style="width:100%",
                            tags$tr(width = "30%",
                                    tags$td(width = "15%", numericInput('p31','# 1\"',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p32','# 3/8\"',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p33','# Nº 4',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p34','# Nº 10',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p35','# Nº 40 ',85,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p36','# Nº 200',16,0,100,1,width = '75%')),
                            )
                 ),
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            sliderInput('LL3','Limite de Liquidez (# Nº 40):',0,80,40,1, width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('IP3','Índice de Plasticidade (# Nº 40):',0,80,40,1, width = '95%'))
                            )
                 )
        ), #TabPanel3--|
        
        #|--TabPanel4
        tabPanel(tags$h5('Material 4', align = 'center'), 
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            textInput('nome4','Nome do Material',value = "", width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('custo4','Custo Unitário (R$/m³)',0,250,25,1, width = '95%'))
                            )
                 ),
                 tags$strong('% Passante:'),
                 tags$table(style="width:100%",
                            tags$tr(width = "30%",
                                    tags$td(width = "15%", numericInput('p41','# 1\"',50,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p42','# 3/8\"',50,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p43','# Nº 4',50,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p44','# Nº 10',50,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p45','# Nº 40 ',50,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p46','# Nº 200',50,0,100,1,width = '75%')),
                            )
                 ),
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            sliderInput('LL4','Limite de Liquidez (# Nº 40):',0,80,40,1, width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('IP4','Índice de Plasticidade (# Nº 40):',0,80,40,1, width = '95%'))
                            )
                 )
        ) #TabPanel4--|
      ),
      column(width = 5,
             plotOutput('CurvMat')
      )
    )
  } else if(input$n_mat == 5) {
    fluidRow(
      tabBox(
        title = '', id = 'tabbox5', height = 400, width = 7,
        #|--TabPanel1
        tabPanel(tags$h5('Material 1', align = 'center'), 
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            textInput('nome1','Nome do Material',value = "", width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('custo1','Custo Unitário (R$/m³)',0,250,25,1, width = '95%'))
                            )
                 ),
                 tags$strong('% Passante:'),
                 tags$table(style="width:100%",
                            tags$tr(width = "30%",
                                    tags$td(width = "15%", numericInput('p11','# 1\"',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p12','# 3/8\"',30,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p13','# Nº 4',2,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p14','# Nº 10',1,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p15','# Nº 40 ',1,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p16','# Nº 200',1,0,100,1,width = '75%')),
                            )
                 ),
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%",
                                            sliderInput('LL1','Limite de Liquidez (# Nº 40):',0,80,0,1, width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('IP1','Índice de Plasticidade (# Nº 40):',0,80,0,1, width = '95%'))
                            )
                 )
        ), #TabPanel1--|
        
        #|--TabPanel2
        tabPanel(tags$h5('Material 2', align = 'center'), 
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            textInput('nome2','Nome do Material',value = "", width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('custo2','Custo Unitário (R$/m³)',0,250,25,1, width = '95%'))
                            )
                 ),
                 tags$strong('% Passante:'),
                 tags$table(style="width:100%",
                            tags$tr(width = "30%",
                                    tags$td(width = "15%", numericInput('p21','# 1\"',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p22','# 3/8\"',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p23','# Nº 4',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p24','# Nº 10',40,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p25','# Nº 40 ',22,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p26','# Nº 200',6,0,100,1,width = '75%')),
                            )
                 ),
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            sliderInput('LL2','Limite de Liquidez (# Nº 40):',0,80,0,1, width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('IP2','Índice de Plasticidade (# Nº 40):',0,80,0,1, width = '95%'))
                            )
                 )
        ), #TabPanel2--|
        
        #|--TabPanel3
        tabPanel(tags$h5('Material 3', align = 'center'), 
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            textInput('nome3','Nome do Material',value = "", width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('custo3','Custo Unitário (R$/m³)',0,250,25,1, width = '95%'))
                            )
                 ),
                 tags$strong('% Passante:'),
                 tags$table(style="width:100%",
                            tags$tr(width = "30%",
                                    tags$td(width = "15%", numericInput('p31','# 1\"',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p32','# 3/8\"',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p33','# Nº 4',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p34','# Nº 10',100,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p35','# Nº 40 ',85,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p36','# Nº 200',16,0,100,1,width = '75%')),
                            )
                 ),
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            sliderInput('LL3','Limite de Liquidez (# Nº 40):',0,80,29,1, width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('IP3','Índice de Plasticidade (# Nº 40):',0,80,8,1, width = '95%'))
                            )
                 )
        ), #TabPanel3--|
        #|--TabPanel4
        tabPanel(tags$h5('Material 4', align = 'center'), 
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            textInput('nome4','Nome do Material',value = "", width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('custo4','Custo Unitário (R$/m³)',0,250,25,1, width = '95%'))
                            )
                 ),
                 tags$strong('% Passante:'),
                 tags$table(style="width:100%",
                            tags$tr(width = "30%",
                                    tags$td(width = "15%", numericInput('p41','# 1\"',50,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p42','# 3/8\"',50,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p43','# Nº 4',50,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p44','# Nº 10',50,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p45','# Nº 40 ',50,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p46','# Nº 200',50,0,100,1,width = '75%')),
                            )
                 ),
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            sliderInput('LL4','Limite de Liquidez (# Nº 40):',0,80,40,1, width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('IP4','Índice de Plasticidade (# Nº 40):',0,80,40,1, width = '95%'))
                            )
                 )
        ), #TabPanel4--|
        #|--TabPanel5
        tabPanel(tags$h5('Material 5', align = 'center'), 
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            textInput('nome5','Nome do Material',value = "", width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('custo5','Custo Unitário (R$/m³)',0,250,25,1, width = '95%'))
                            )
                 ),
                 tags$strong('% Passante:'),
                 tags$table(style="width:100%",
                            tags$tr(width = "30%",
                                    tags$td(width = "15%", numericInput('p51','# 1\"',50,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p52','# 3/8\"',50,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p53','# Nº 4',50,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p54','# Nº 10',50,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p55','# Nº 40 ',50,0,100,1,width = '75%')),
                                    tags$td(width = "15%", numericInput('p56','# Nº 200',50,0,100,1,width = '75%')),
                            )
                 ),
                 tags$table(style="width:100%",
                            tags$tr(width = "50%",
                                    tags$td(width = "50%", 
                                            sliderInput('LL5','Limite de Liquidez (# Nº 40):',0,80,40,1, width = '95%')),
                                    tags$td(width = "50%", 
                                            sliderInput('IP5','Índice de Plasticidade (# Nº 40):',0,80,40,1, width = '95%'))
                            )
                 )
        ) #TabPanel5--|
      ),
      column(width = 5,
             plotOutput('CurvMat')
      )
    )
  }
  )
# Modal Dialog ####
  v <- reactiveValues()
  
  observeEvent(input$curva_adapt, {
    showModal(
      modalDialog(
        title = "Limites de % Passante (Curva Adaptada)",
        tags$strong('% Mínimo:'),
        tags$table(style="width:100%",
                   tags$tr(width = "30%",
                           tags$td(width = "15%", numericInput('ca_min1','# 1\"',F_min$A[1],0,100,1,width = '75%')),
                           tags$td(width = "15%", numericInput('ca_min2','# 3/8\"',F_min$A[2],0,100,1,width = '75%')),
                           tags$td(width = "15%", numericInput('ca_min3','# Nº 4',F_min$A[3],0,100,1,width = '75%')),
                           tags$td(width = "15%", numericInput('ca_min4','# Nº 10',F_min$A[4],0,100,1,width = '75%')),
                           tags$td(width = "15%", numericInput('ca_min5','# Nº 40 ',F_min$A[5],0,100,1,width = '75%')),
                           tags$td(width = "15%", numericInput('ca_min6','# Nº 200',F_min$A[6],0,100,1,width = '75%'))
                           )
                   ),
        tags$strong('% Máximo:'),
        tags$table(style="width:100%",
                   tags$tr(width = "30%",
                           tags$td(width = "15%", numericInput('ca_max1','# 1\"',F_max$A[1],0,100,1,width = '75%')),
                           tags$td(width = "15%", numericInput('ca_max2','# 3/8\"',F_max$A[2],0,100,1,width = '75%')),
                           tags$td(width = "15%", numericInput('ca_max3','# Nº 4',F_max$A[3],0,100,1,width = '75%')),
                           tags$td(width = "15%", numericInput('ca_max4','# Nº 10',F_max$A[4],0,100,1,width = '75%')),
                           tags$td(width = "15%", numericInput('ca_max5','# Nº 40 ',F_max$A[5],0,100,1,width = '75%')),
                           tags$td(width = "15%", numericInput('ca_max6','# Nº 200',F_max$A[6],0,100,1,width = '75%'))
                   )
        ),
        ignoreNULL = FALSE,
        footer = tagList(
          modalButton("Close")
        )
      )
    )
  })
    
  
# Remaining ####  
  observe(
   if(input$Faixa == 'Curva Adaptada'){
      v$I <- c(input$ca_min1,input$ca_min2,input$ca_min3,
               input$ca_min4,input$ca_min5,input$ca_min6)
      v$S <- c(input$ca_max1,input$ca_max2,input$ca_max3,
               input$ca_max4,input$ca_max5,input$ca_max6)
    } else {
      v$I <- unlist(F_min[input$Faixa])
      v$S <- unlist(F_max[input$Faixa])
    })
  
  observe(
    if(input$n_mat==2){
      v$p <- cbind(c(input$p11,input$p12,input$p13,input$p14,input$p15,input$p16),
                 c(input$p21,input$p22,input$p23,input$p24,input$p25,input$p26))
      v$nomes <- c(ifelse(input$nome1=="",'Material 1',input$nome1),
                   ifelse(input$nome2=="",'Material 2',input$nome2))
    } else if (input$n_mat==3){
      v$p <- cbind(c(input$p11,input$p12,input$p13,input$p14,input$p15,input$p16),
                 c(input$p21,input$p22,input$p23,input$p24,input$p25,input$p26),
                 c(input$p31,input$p32,input$p33,input$p34,input$p35,input$p36))
      v$nomes <- c(ifelse(input$nome1=="",'Material 1',input$nome1),
                   ifelse(input$nome2=="",'Material 2',input$nome2),
                   ifelse(input$nome3=="",'Material 3',input$nome3))
    } else if (input$n_mat==4){
      v$p <- cbind(c(input$p11,input$p12,input$p13,input$p14,input$p15,input$p16),
                 c(input$p21,input$p22,input$p23,input$p24,input$p25,input$p26),
                 c(input$p31,input$p32,input$p33,input$p34,input$p35,input$p36),
                 c(input$p41,input$p42,input$p43,input$p44,input$p45,input$p46))
      v$nomes <- c(ifelse(input$nome1=="",'Material 1',input$nome1),
                   ifelse(input$nome2=="",'Material 2',input$nome2),
                   ifelse(input$nome3=="",'Material 3',input$nome3),
                   ifelse(input$nome4=="",'Material 4',input$nome4))
    } else if (input$n_mat==5){
      v$p <- cbind(c(input$p11,input$p12,input$p13,input$p14,input$p15,input$p16),
                 c(input$p21,input$p22,input$p23,input$p24,input$p25,input$p26),
                 c(input$p31,input$p32,input$p33,input$p34,input$p35,input$p36),
                 c(input$p41,input$p42,input$p43,input$p44,input$p45,input$p46),
                 c(input$p51,input$p52,input$p53,input$p54,input$p55,input$p56))
      v$nomes <- c(ifelse(input$nome1=="",'Material 1',input$nome1),
                   ifelse(input$nome2=="",'Material 2',input$nome2),
                   ifelse(input$nome3=="",'Material 3',input$nome3),
                   ifelse(input$nome4=="",'Material 4',input$nome4),
                   ifelse(input$nome5=="",'Material 5',input$nome5))
    }
  )

  otim <- eventReactive(input$otim_mist, {
    n = input$n_mat #numero de materiais
    m = 6 #numero de peneiras
    if(input$n_mat==2){
      cost <- c(input$custo1,input$custo2)
      LL <- c(input$LL1,input$LL2)
      IP <- c(input$IP1,input$IP2)
    } else if (input$n_mat==3){
      cost <- c(input$custo1,input$custo2,input$custo3)
      LL <- c(input$LL1,input$LL2,input$LL3)
      IP <- c(input$IP1,input$IP2,input$IP3)
    } else if (input$n_mat==4){
      cost <- c(input$custo1,input$custo2,input$custo3,input$custo4)
      LL <- c(input$LL1,input$LL2,input$LL3,input$LL4)
      IP <- c(input$IP1,input$IP2,input$IP3,input$IP4)
    } else if (input$n_mat==5){
      cost <- c(input$custo1,input$custo2,input$custo3,input$custo4,input$custo5)
      LL <- c(input$LL1,input$LL2,input$LL3,input$LL4,input$LL5)
      IP <- c(input$IP1,input$IP2,input$IP3,input$IP4,input$IP5)
    }
    p <- v$p
    I <- v$I
    S <- v$S
    LLmax <- input$LL_max
    IPmax <- input$IP_max
    R <- input$R
    
    # Set coefficients of the objective function
    f.obj <- cost
    
    # Set matrix corresponding to coefficients of constraints by rows
    # Do not consider the non-negative constraint; it is automatically assumed
    f.con <- rbind(v$p, v$p, v$p[5,]*(LL - LLmax), v$p[5,]*(IP - IPmax), v$p[6,]-R*v$p[5,],rep(1,n))
    
    # Set unequality signs
    f.dir <- c(rep('<=',m),rep('>=',m),'<=','<=','<=','=')
    
    # Set right hand side coefficients
    f.rhs <- c(S,I,LLmax,IPmax,R,1)
    
    # Final value (z)
    lp("min", f.obj, f.con, f.dir, f.rhs)
    
  })
  
  output$CurvMat <- renderPlot({
    df <- data.frame(cbind(c(25.4,9.52,4.76,2,0.42,0.074),v$p))
    colnames(df) <- c('x',v$nomes)
    df_m <- melt(df, id = 'x')
    ggplot(df_m, aes(x = x, y = value, color = variable)) +
      geom_line(size = 1.1) +
      geom_point() +
      labs(x = 'Diâmetro dos grãos (mm)', y = 'Passante (%)',
           title = 'Curva Granulométrica dos Materiais da Mistura') +
      theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom',
            legend.title = element_blank()) +
      ylim(0,100) +
      scale_x_continuous(trans = 'log10')
  })
  
  output$OtimResult <- renderUI(
    if(otim()$status == 2){
      fluidRow(
        box(
          renderText({'Não é possível atingir uma mistura com as propriedades 
                          pré-definidas utilizando os materiais escolhidos'}),
          height = 50)
      ) #Row
    } else if (otim()$status != 2 & input$n_mat==length(otim()$solution)){
      fluidRow(
        box(
          tags$h4(tags$strong('Resultado:'), align = 'center'),
          renderTable({
            result <- otim()
            df_mist = data.frame(v$nomes,round(100*result$solution,3))
            colnames(df_mist) =c('Material', 'Composição (%)')
            df_mist
          }),
          tags$h4(tags$strong(paste('Custo Final Otimizado: R$ ', round(otim()$objval,2), '/m³')))
          
          , height = 300 
        ), # Box
        box(
          renderPlot({
            sol <- otim()$solution
            cg <- apply(apply(v$p, 1, function(x){x*sol}),2,sum)
            df <- data.frame(x = c(25.4,9.52,4.76,2,0.42,0.074),
                             y = cg,
                             min = v$I,
                             max = v$S
            )
            ggplot(df, aes(x = x, y = y, label = round(y,1))) +
              geom_line() +
              geom_point() +
              geom_ribbon(aes(ymin = min, ymax = max), fill='red', alpha = 0.2) +
              #scale_fill_manual(name = 'Legenda', 
              #                  labels = 'Lim.', 
              #                  values = adjustcolor("red", alpha.f = 0.5)) +
              scale_x_continuous(trans='log10')  +
              geom_text(hjust=0, vjust=2) +
              labs(x = 'Diâmetro dos grãos (mm)', y = 'Passante (%)', 
                   title = paste('Curva Granulométrica da Mistura e Região Viável da Faixa',input$Faixa)) +
              theme(plot.title = element_text(hjust = 0.5))
          }), height = 400
        ) # Box
      ) #Row
    } else {
      fluidRow(
        
      )
    }
  )
  
  #observe({
  #  input$n_mat   
  #  output$OtimResult <- renderUI({})
  #})
  
  output$text_otim2 <- renderText({
    result <- otim()
    sol <- result$solution
    apply(apply(v$p, 1, function(x){x*sol}),2,sum)
  })
}

shinyApp(ui, server)
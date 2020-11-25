
# Casen
# Victor Enamorado - Christian Castro
# 25 de Noviembre del 2020
# version 15:40 am
  
library(ggplot2)
library(ggpubr)
library(markdown)
library(shiny)
library(shinythemes)
library(tidyverse)
library(magrittr)
library(lubridate)
library(plotly)
library(xts)
library(dygraphs)
library(kableExtra)
library(knitr)
library("readxl")
library(rsconnect)
library(dplyr)
library(summarytools)
library(epiDisplay)
#library(leaflet)
library(haven)
library(epiDisplay)
library("readxl")
library(expss)
library(hrbrthemes)
library(viridis)
library(viridisLite)
library(DescTools)
library(roperators)
library(shinycssloaders)
library(writexl)

oldw <- getOption("warn")
options(warn = -1)

####################### consumo de tablas excel ##################################

##################################### 2006 #######################################

dataset2006 <- read.csv('mydata2006_sub.csv')

dataset2006_col <- colnames(dataset2006)

data_2006_3_5 <- dataset2006[, c(4,6)]
data_2006_1_2_colnames <- colnames(data_2006_3_5)

data_2006_5_348 <- dataset2006[, 7:348]
data_2006_5_348_colnames <- colnames(data_2006_5_348)

###################################### 2009 ############################################

dataset2009 <- read.csv('mydata2009_sub.csv')

dataset2009_col <- colnames(dataset2009)

data_2009_3_5 <- dataset2009[, c(4,6)]
data_2009_1_2_colnames <- colnames(data_2009_3_5)

data_2009_5_348 <- dataset2009[, 7:348]
data_2009_5_348_colnames <- colnames(data_2009_5_348)


######################### 2011 pendiente ###############################################

dataset2011 <- read.csv('mydata2011_sub.csv')

dataset2011_col <- colnames(dataset2011)

data_2011_3_5 <- dataset2011[, 1:6]
data_2011_1_2_colnames <- colnames(data_2011_3_5)

data_2011_5_348 <- dataset2011[, 7:348]
data_2011_5_348_colnames <- colnames(data_2011_5_348)


###################################### 2013 ############################################

dataset2013 <- read.csv('mydata2013_sub.csv')
dataset2013_col <- colnames(dataset2013)

# extraccion de las cabeceras para la carga de los filtros por categoria

data_2013_3_5 <- dataset2013[, c(4,6)]
data_2013_1_2_colnames <- colnames(data_2013_3_5)

data_2013_5_348 <- dataset2013[, 7:348]
data_2013_5_348_colnames <- colnames(data_2013_5_348)

# extraccion de las cabeceras para la carga de los ddl para frecuencias por modulo
###################################### ###################################### ###################################### 
###################################### Esto sera borral al desplegar un dropdownlist para todas las columnas ###################################### 
###################################### ###################################### ###################################### 
dataset2013_sub_I <- dataset2013[,1:10]
dataset2013_col_I <- colnames(dataset2013_sub_I)
dataset2013_sub_II <- dataset2013[,11:20]
dataset2013_col_II <- colnames(dataset2013_sub_II)
dataset2013_sub_III <- dataset2013[,21:30]
dataset2013_col_III <- colnames(dataset2013_sub_III)
dataset2013_sub_IV <- dataset2013[,31:40]
dataset2013_col_IV <- colnames(dataset2013_sub_IV)
dataset2013_sub_V <- dataset2013[,41:50]
dataset2013_col_V <- colnames(dataset2013_sub_V)
dataset2013_sub_VI <- dataset2013[,51:60]
dataset2013_col_VI <- colnames(dataset2013_sub_VI)
dataset2013_sub_VII <- dataset2013[,61:70]
dataset2013_col_VII <- colnames(dataset2013_sub_VII)
dataset2013_sub_VIII <- dataset2013[,71:80]
dataset2013_col_VIII <- colnames(dataset2013_sub_VIII)
###################################### ###################################### ###################################### 
###################################### ###################################### ###################################### 
###################################### ###################################### ###################################### 

###################################### 2015 ############################################

dataset2015 <- read.csv('mydata2015_sub.csv')
dataset2015_col <- colnames(dataset2015)

# extraccion de las cabeceras para la carga de los filtros por categoria

data_2015_3_5 <- dataset2015[, c(4,6)]
data_2015_1_2_colnames <- colnames(data_2015_3_5)

data_2015_5_348 <- dataset2015[, 7:348]
data_2015_5_348_colnames <- colnames(data_2015_5_348)

# extraccion de las cabeceras para la carga de los ddl para frecuencias por modulo

dataset2015_sub_I <- dataset2015[,1:10]
dataset2015_col_I <- colnames(dataset2015_sub_I)
dataset2015_sub_II <- dataset2015[,11:20]
dataset2015_col_II <- colnames(dataset2015_sub_II)
dataset2015_sub_III <- dataset2015[,21:30]
dataset2015_col_III <- colnames(dataset2015_sub_III)
dataset2015_sub_IV <- dataset2015[,31:40]
dataset2015_col_IV <- colnames(dataset2015_sub_IV)
dataset2015_sub_V <- dataset2015[,41:50]
dataset2015_col_V <- colnames(dataset2015_sub_V)
dataset2015_sub_VI <- dataset2015[,51:60]
dataset2015_col_VI <- colnames(dataset2015_sub_VI)
dataset2015_sub_VII <- dataset2015[,61:70]
dataset2015_col_VII <- colnames(dataset2015_sub_VII)
dataset2015_sub_VIII <- dataset2015[,71:80]
dataset2015_col_VIII <- colnames(dataset2015_sub_VIII)

########################################################################################

dataset2011 <- read.csv('mydata2011_sub.csv')
dataset2011 <- dataset2011[1:1000,]
# dataset2011_col <- colnames(dataset2011)

##################   2017 ##############################

dataset2017 <-  read.csv('mydata2017_sub.csv')
datos_df_exp <- colnames(dataset2017)

########################################################


data_2017 <- read_xlsx("casen_2017_mil.xlsx")
data_2017_colnames <- colnames(data_2017)

data_2017_modulo_I <- data_2017[,1:42]
data_2017_modulo_I_colnames <- colnames(data_2017_modulo_I)
data_2017_modulo_II <- data_2017[,43:102]
data_2017_modulo_II_colnames <- colnames(data_2017_modulo_II)
data_2017_modulo_III <- data_2017[,103:151]
data_2017_modulo_III_colnames <- colnames(data_2017_modulo_III)
data_2017_modulo_IV <- data_2017[,152:304]
data_2017_modulo_IV_colnames <- colnames(data_2017_modulo_IV)

datos_df_1000  <- read_xlsx("casen_2006_mil.xlsx")
datos_df_educacion <- datos_df_1000[, 1:32]
datos_df_educacion_preg <- colnames(datos_df_educacion)

datos_df_2000 <- read_xlsx("casen_2009_mil_ymt.xlsx")
datos_df_2009_ymt <- datos_df_2000[, 1:5]
datos_df_2009_ymt_preg <- colnames(datos_df_2009_ymt)

datos_df_casen_2009_mil_mn <- read_xlsx("casen_2009_mil_mn.xlsx")
datos_df_casen_2009_mil_mn <- datos_df_casen_2009_mil_mn[, 1:34]
datos_df_casen_2009_mil_mn_preg <- colnames(datos_df_casen_2009_mil_mn)

data_2009_filtros_terr <- datos_df_casen_2009_mil_mn[, 1:2]
data_2009_filtros_terr_ddl <- colnames(data_2009_filtros_terr)
data_2009_filtros_cat <- datos_df_casen_2009_mil_mn[, 9:32]
data_2009_filtros_cat_ddl <- colnames(data_2009_filtros_cat)

datos_df_casen_2011_mil_mn <- read_xlsx("casen_2011_mil_mn.xlsx")
datos_df_casen_2011_mil_mn <- datos_df_casen_2011_mil_mn[, 1:34]
datos_df_casen_2011_mil_mn_preg <- colnames(datos_df_casen_2011_mil_mn)

data_2011_filtros_terr <- datos_df_casen_2011_mil_mn[, 1:2]
data_2011_filtros_terr_ddl <- colnames(data_2011_filtros_terr)
data_2011_filtros_cat <- datos_df_casen_2011_mil_mn[, 9:32]
data_201_filtros_cat_ddl <- colnames(data_2011_filtros_cat)

datos_df_casen_2011_mil_ymt <- read_xlsx("casen_2011_mil_ymt.xlsx")
datos_df_casen_2011_mil_ymt  <- datos_df_casen_2011_mil_ymt [, 1:24]
datos_df_casen_2011_mil_ymt_preg <- colnames(datos_df_casen_2011_mil_ymt )

###################

datos_df_casen_2013_mil <- read_xlsx("casen_2013_mil.xlsx")
datos_df_casen_2013_mil <- datos_df_casen_2013_mil[, 1:600]
datos_df_casen_2013_mil_preg <- colnames(datos_df_casen_2013_mil)

datos_df_casen_2015_mil <- read_xlsx("casen_2015_mil.xlsx")
datos_df_casen_2015_mil <- datos_df_casen_2015_mil[, 1:776]
datos_df_casen_2015_mil_preg <- colnames(datos_df_casen_2015_mil)

datos_df_casen_2017_mil <- read_xlsx("casen_2017_mil.xlsx")
datos_df_casen_2017_mil <- datos_df_casen_2017_mil[, 1:808]
datos_df_casen_2017_mil_preg <- colnames(datos_df_casen_2017_mil)

datos_df_casen_2017_miledu <- read_xlsx("casen_2017_mil.xlsx")
datos_df_casen_2017_miledu <- datos_df_casen_2017_miledu[, 43:102]
datos_df_casen_2017_mil_pregedu <- colnames(datos_df_casen_2017_miledu)



ui <- fluidPage(theme = shinytheme("cerulean"),
                
                br(), 
                br(),
                br(),
                
                selectInput("variable_anio", h4("Seleccione base de datos:"),
                            
                            c("Seleccione año" = "2",
                              "Casen 2006" = "2006",
                              "Casen 2009" = "2009",
                              "Casen 2011" = "2011",
                              # "Casen 2011 ymt" = "20110",
                              # "Casen 2011 mn" = "20111",
                              "Casen 2013" = "2013",
                              "Casen 2015" = "2015",
                              "Casen 2017" = "2017"
                              
                            )),       
                titlePanel(h1("HIVGIE: Herramienta para la interpretación de variables y generación de informes estadísticos de la CASEN")),
                br(),

                
                img(src = "myImage.jpg"),
                
                uiOutput("navbarPageUI")
                
) 

# Definir la logica del server
server <- function(input, output, session) {
    
    output$navbarPageUI <- renderUI({
        
        user <- input$variable_anio
        
        if (user == 2006) {
            
            navbarPage(
                
                br(),
                
                tabPanel("Introducción a la Encuesta",
                         fluidRow(column(9, includeMarkdown("info_2006_intro.md")))),

                tabPanel("Despliegue total de la base de datos Casen 2006",
                         fluidRow(column(3, includeMarkdown("info_2006_tabla.md")),
                                  column(12, dataTableOutput('table_2006')))),
                
                
                navbarMenu("Módulos de la Casen 2006",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_modulos.txt")))),
                           "----",
                           "",
                           tabPanel("Módulo 1: Identificación", fluidRow(
                               column(12, includeMarkdown("modulo_1_2006.md")),
                               column(12, dataTableOutput("modulo_1_2006"))
                           )),
                           
                           tabPanel("Módulo 2: Expansión", fluidRow(
                               column(12, includeMarkdown("modulo_2_2006.md")),
                               column(12, dataTableOutput("modulo_2_2006"))
                           )),
                           
                           tabPanel("Módulo 3: Residentes", fluidRow(
                               column(4, includeMarkdown("modulo_3_2006_1.md")),
                               column(4, includeMarkdown("modulo_3_2006_2.md")),
                               column(4, includeMarkdown("modulo_3_2006_3.md")),
                               
                               column(12, dataTableOutput("modulo_3_2006"))
                           )),
                           
                           tabPanel("Módulo 4: Educación", fluidRow(
                               column(4, includeMarkdown("modulo_4_2006_1.md")),
                               column(4, includeMarkdown("modulo_4_2006_2.md")),
                               column(4, includeMarkdown("modulo_4_2006_3.md")),
                               
                               column(12, dataTableOutput("modulo_4_2006"))
                           )),
                           
                           tabPanel("Módulo 5: Empleo", fluidRow(
                               column(4, includeMarkdown("modulo_5_2006_1.md")),
                               column(4, includeMarkdown("modulo_5_2006_2.md")),
                               column(4, includeMarkdown("modulo_5_2006_3.md")),
                               
                               column(12, dataTableOutput("modulo_5_2006"))
                           )),
                           
                           tabPanel("Módulo 6: Ingresos", fluidRow(
                             column(4, includeMarkdown("modulo_6_2006_1.md")),
                             column(4, includeMarkdown("modulo_6_2006_2.md")),
                             column(4, includeMarkdown("modulo_6_2006_3.md")),
                               column(12, dataTableOutput("modulo_6_2006"))
                           )),
                           
                           tabPanel("Módulo 7: Salud", fluidRow(
                             column(4, includeMarkdown("modulo_7_2006_1.md")),
                             column(4, includeMarkdown("modulo_7_2006_2.md")),
                             column(4, includeMarkdown("modulo_7_2006_3.md")),
                               column(12, dataTableOutput("modulo_7_2006"))
                           )),
                           tabPanel("Módulo 8: Tema Discapacidad", fluidRow(
                             column(4, includeMarkdown("modulo_8_2006_1.md")),
                             column(4, includeMarkdown("modulo_8_2006_2.md")),
                               column(12, dataTableOutput("modulo_8_2006"))
                           )),
                           tabPanel("Módulo 9: Tema Chile solidario", fluidRow(
                             column(12, includeMarkdown("modulo_9_2006.md")),
                             column(9, dataTableOutput("modulo_9_2006"))
                           )),
                           
                           tabPanel("Módulo 10: Tema Etnia", fluidRow(
                             column(12, includeMarkdown("modulo_10_2006.md")),
                             column(12, dataTableOutput("modulo_10_2006"))
                           )),
                           
                           tabPanel("Módulo 11: Tema Migración", fluidRow(
                             column(12, includeMarkdown("modulo_11_2006.md")),
                             column(12, dataTableOutput("modulo_11_2006"))
                           )),
                           
                           tabPanel("Módulo 12: Tema Cultura", fluidRow(
                             column(12, includeMarkdown("modulo_12_2006.md")),
                             column(12, dataTableOutput("modulo_12_2006"))
                           )),
                           
                           tabPanel("Módulo 13: Tema Autobiografía", fluidRow(
                             column(4, includeMarkdown("modulo_13_2006_1.md")),
                             column(4, includeMarkdown("modulo_13_2006_2.md")),
                             column(4, includeMarkdown("modulo_13_2006_3.md")),
                             column(12, dataTableOutput("modulo_13_2006"))
                           )),
                           
                           tabPanel("Módulo 14: Módulo Vivienda", fluidRow(
                             column(4, includeMarkdown("modulo_14_2006_1.md")),
                             column(4, includeMarkdown("modulo_14_2006_2.md")),
                             column(4, includeMarkdown("modulo_14_2006_3.md")),
                             column(12, dataTableOutput("modulo_14_2006"))
                           )),
                           
                           tabPanel("15 Variables creadas: Educación", fluidRow(
                             column(12, includeMarkdown("modulo_15_2006.md")),
                             column(12, dataTableOutput("modulo_15_2006"))
                           )),
                           
                           
                           tabPanel("16 Variables creadas: Empleo", fluidRow(
                             column(12, includeMarkdown("modulo_16_2006.md")),
                             column(12, dataTableOutput("modulo_16_2006"))
                           )),
                           tabPanel("17 Variables creadas: Ingresos del trabajo", fluidRow(
                             column(12, includeMarkdown("modulo_17_2006.md")),
                             column(12, dataTableOutput("modulo_17_2006"))
                           )),
                           tabPanel("18 Variables creadas: Subsidios monetarios", fluidRow(
                             column(12, includeMarkdown("modulo_18_2006.md")),
                             column(12, dataTableOutput("modulo_18_2006"))
                           )),
                           tabPanel("19 Variables creadas: Otros ingresos", fluidRow(
                             column(12, includeMarkdown("modulo_19_2006.md")),
                             column(12, dataTableOutput("modulo_19_2006"))
                           )),
                           
                           tabPanel("20 Variables creadas: Línea de pobreza, quintiles y deciles de ingreso", fluidRow(
                             column(12, includeMarkdown("modulo_20_2006.md")),
                             column(12, dataTableOutput("modulo_20_2006"))
                           )),

                           "----",
                           "",

                           tabPanel(" ")),
                
                navbarMenu("Tabla de contingencia > 2x2",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           tabPanel("Tablas de contingencia > 2x2",fluidRow(
                             column(12, includeMarkdown("ejercicio_001_2006.md")),
                              # selectInput("nada", "Identifique la variable:", c(data_2017_colnames)),
                               column(12,

                                      selectInput("p2006_primerav", "ingrese primera variable:", c(dataset2006_col)),
                                      selectInput("p2006_segundav", "ingrese segunda variable:", c(dataset2006_col)),
                                      selectInput("p2006_tercerav", "ingrese tercera variable:", c(dataset2006_col)),
                                      selectInput("p2006_cuartav", "ingrese cuarta variable:", c(dataset2006_col)),
                                      
                                    #  includeMarkdown("frecuencias_muestrales.md"),
                                      
                                    column(12, includeMarkdown("frecuencias_muestrales.md")),
                                    
                                    
                                      downloadButton("boton_ttcc_2006", "Descargar"),
                                    
                                    
                                    
                                      verbatimTextOutput("tabla_d_c_generalizada_2006") %>% withSpinner(type = 5, color = "#e6460b", size = 0.5),
                                      
                                      
                                    #  includeMarkdown("frecuencias_poblacionales.md"),
                                      
                                    column(12, includeMarkdown("frecuencias_poblacionales.md")),
                                      downloadButton("boton_ttcc_2006_pon", "Descargar"),
                                      #   tableOutput("tabla_d_c_generalizada") %>% withSpinner(color="#0dc5c1")))),
                                      verbatimTextOutput("tabla_d_c_generalizada_pon_2006") %>% withSpinner(type = 5, color = "#bd1c52", size = 0.5)
                               )
                           )),
                           
                           tabPanel("Cochran–Mantel–Haenszel",fluidRow(column(12,
                                                                              selectInput("p2006_primerav", "ingrese primera variable:", c(dataset2006_col)),
                                                                              selectInput("p2006_segundav", "ingrese segunda variable:", c(dataset2006_col)),
                                                                              selectInput("p2006_tercerav", "ingrese tercera variable:", c(dataset2006_col)),
                                                                              verbatimTextOutput("tabla_chi_generalizada"))))
                ),
                
                
                
                tabPanel("Frecuencias de respuestas por campo", fluidRow(
                    column(12, includeMarkdown("info_2006_frec.md")),
                    selectInput("ptabla_2006", "Seleccione pregunta:", c(dataset2006_col)),
                    column(12, dataTableOutput("prueba_tabla"))
                )),

                
                navbarMenu("Estadísticas y gráficas",
                           tabPanel("Promedios", fluidRow(
                               column(12, includeMarkdown("info_2006_prom.md")),
                               selectInput("ptabla_promedios", "Seleccione variable:", c(dataset2006_col)),
                               column(12, verbatimTextOutput("promedios_2006"))
                           )),
                           
                           
                           
                           tabPanel("Diagrama de caja y bigotes", fluidRow(
                               
                               column(12, includeMarkdown("info_2006_cyb.md")),

                               selectInput("ptabla_cyb", "Seleccione la variable:", c(dataset2006_col)),
                               
                               downloadButton("plot_cyb_2006", "Descargar"),

                               column(12, plotOutput("cyb_2006"))
                           ))
                ),

                navbarMenu("Filtros agrupados por categorías",
                           tabPanel("Seleccione variable que funga como grupo:", fluidRow(
                               column(12, includeMarkdown("info_2006_prom.md")),
                               selectInput("nivel_filtro", "Seleccione unidad social:", c(data_2006_1_2_colnames)),
                               selectInput("categoria_filtro", "Seleccione atributo:", c(data_2006_5_348_colnames)),
                               column(12, tableOutput("promedios_filtros_2006"))
                           ))),
                
                navbarMenu("Pobreza y exclusión social",
                           tabPanel("Seleccione variable que funga como grupo:", fluidRow(
                             column(12, includeMarkdown("info_2006_prom.md"))
                           )))
            )
            
        }
        
        
        
        
        
        
        
        
        
        
        else if (user == 2009){
            
            navbarPage(
                
                
                br(),
                
                tabPanel("Introducción a la Encuesta",
                         fluidRow(column(9, includeMarkdown("about_intro.md")))),
                
                tabPanel("Despliegue total de la base de datos Casen 2009",
                         fluidRow(column(9, includeMarkdown("about_pobporin_vei.txt")),
                                  column(12,  dataTableOutput("table_2009")))),
                
                
                navbarMenu("Módulos de la Casen 2009",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_modulos.txt")))),
                           "----",
                           "",
                           tabPanel("Módulo 1: Identificacion", fluidRow(
                               column(12, includeMarkdown("modulo1_2009.md")),
                               column(12, dataTableOutput("modulo_1_2009"))
                           )),
                           
                           tabPanel("Módulo 2: Expansión", fluidRow(
                             column(12, includeMarkdown("modulo_2_2009.md")),
                             column(12, dataTableOutput("modulo_2_2009"))
                           )),
                           
                           
                           
                           tabPanel("Módulo 3: Residentes", fluidRow(
                             column(12, includeMarkdown("modulo_3_2009.md")),
                             column(12, dataTableOutput("modulo_3_2009"))
                           )),
                           tabPanel("Módulo 3: Residentes(Situación en la vivienda)", fluidRow(
                             column(12, includeMarkdown("modulo_3_res_sit.md")),
                             column(12, dataTableOutput("modulo_31_2009"))
                           )),
                           tabPanel("Módulo 3: Residentes(Patrimonio)", fluidRow(
                             column(12, includeMarkdown("modulo_3_res_pat.md")),
                             column(12, dataTableOutput("modulo_32_2009"))
                           )),
                           tabPanel("Módulo 3: Residentes(Lugar de uso tecnologias)", fluidRow(
                             column(12, includeMarkdown("modulo_3_res_lug.md")),
                             column(12, dataTableOutput("modulo_33_2009"))
                           )),
                           tabPanel("Módulo 3: Residentes(uso del internet)", fluidRow(
                             column(12, includeMarkdown("modulo_3_res_int.md")),
                             column(12, dataTableOutput("modulo_34_2009"))
                           )),
                           ###############################################################################3
                           
                           tabPanel("Módulo 4: Educación", fluidRow(
                               column(12, includeMarkdown("modulo4_2009.md")),
                               column(12, dataTableOutput("modulo_4_2009"))
                           )),
                           
                           tabPanel("Módulo 5: Empleo", fluidRow(
                               column(12, includeMarkdown("modulo5_2009.md")),
                               column(12, dataTableOutput("modulo_5_2009"))
                           )),
                           
                           tabPanel("Módulo 6: Ingresos", fluidRow(
                               column(12, includeMarkdown("modulo6_2009.md")),
                               column(12, dataTableOutput("modulo_6_2009"))
                           )),
                           
                           tabPanel("Módulo 7: Salud", fluidRow(
                             column(4, includeMarkdown("modulo_7_2009_1.md")),
                             column(4, includeMarkdown("modulo_7_2009_2.md")),
                             column(4, includeMarkdown("modulo_7_2009_3.md")),
                             
                               column(12, dataTableOutput("modulo_7_2009"))
                           )),
                           
                           tabPanel("Módulo 8: Tema Discapacidad", fluidRow(
                             column(12, includeMarkdown("modulo8_2009.md")),
                             column(12, dataTableOutput("modulo_8_2009"))
                           )),
                           
                           tabPanel("Módulo 9: Tema Chile solidario", fluidRow(
                             column(12, includeMarkdown("modulo9_2009.md")),
                             column(12, dataTableOutput("modulo_9_2009"))
                           )),
                           
                           tabPanel("Módulo 10: Tema Etnia", fluidRow(
                             column(12, includeMarkdown("modulo10_2009.md")),
                             column(12, dataTableOutput("modulo_10_2009"))
                           )),
                           #################################################################
                           tabPanel("Módulo 11: Tema Migración", fluidRow(
                             column(12, includeMarkdown("modulo_11_2009.md")),
                             column(12, dataTableOutput("modulo_11_2009"))
                           )),
                           
                           tabPanel("Módulo 12: Tema: Lugar de trabajo, estudio y transporte", fluidRow(
                             column(12, includeMarkdown("modulo_12a_2009.md")),
                             column(12, dataTableOutput("modulo_12a_2009"))
                           )),
                           
                           tabPanel("Módulo 13: Tema Autobiografía", fluidRow(
                             column(4, includeMarkdown("modulo_13_2009_1.md")),
                             column(12, dataTableOutput("modulo_13_2009"))
                           )),
                           
                           
                           tabPanel("Módulo 14: Tema Redes y participación", fluidRow(
                             column(12, includeMarkdown("modulo_14a_2009.md")),
                             column(12, dataTableOutput("modulo_14a_2009"))
                           )),
                           
                           
                           
                           
                           tabPanel("Módulo 14: Módulo Vivienda", fluidRow(
                             column(4, includeMarkdown("modulo_14_2009_1.md")),
                             column(4, includeMarkdown("modulo_14_2009_2.md")),
                             column(4, includeMarkdown("modulo_14_2009_3.md")),
                             column(12, dataTableOutput("modulo_14_2009"))
                           )),
                           
                           
                           ####################################################################3
                           
                           
                           tabPanel("15 Variables creadas: Educación", fluidRow(
                             column(12, includeMarkdown("modulo_15_2009.md")),
                             column(12, dataTableOutput("modulo_15_2009"))
                           )),
                           
                           tabPanel("16 Variables creadas: Empleo", fluidRow(
                             column(12, includeMarkdown("modulo_16_2009.md")),
                             column(12, dataTableOutput("modulo_16_2009"))
                           )),
                           
                           tabPanel("17 Variables creadas: Linea de pobreza, quintiles y deciles de ingresos", fluidRow(
                             column(12, includeMarkdown("modulo_17_2009.md")),
                             column(12, dataTableOutput("modulo_17_2009"))
                           )),
                           
                           tabPanel("18 Variables creadas: Ingresos del trabajo", fluidRow(
                             column(12, includeMarkdown("modulo_18_2009.md")),
                             column(12, dataTableOutput("modulo_18_2009"))
                           )),
                           
                           tabPanel("19 Variables creadas: Subsidios monetarios", fluidRow(
                             column(12, includeMarkdown("modulo_19_2009.md")),
                             column(12, dataTableOutput("modulo_19_2009"))
                           )),
                           
                           tabPanel("20 Variables creadas: Otros ingresos", fluidRow(
                             column(12, includeMarkdown("modulo_20_2009.md")),
                             column(12, dataTableOutput("modulo_20_2009"))
                           )),
                           
                           tabPanel("21 Variables creadas: Indices creados", fluidRow(
                             column(12, includeMarkdown("modulo_21_2009.md")),
                             column(12, dataTableOutput("modulo_21_2009"))
                           )),
                           
                           
                           
                           
                           
                           tabPanel(" ")),
                
                navbarMenu("Tabla de contingencia > 2x2",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           tabPanel("Tablas de contingencia > 2x2",fluidRow(
                               # selectInput("nada", "Identifique la variable:", c(data_2017_colnames)),
                               column(12,
                                      
                                      selectInput("p2009_primerav", "ingrese primera variable:", c(dataset2009_col)),
                                      selectInput("p2009_segundav", "ingrese segunda variable:", c(dataset2009_col)),
                                      selectInput("p2009_tercerav", "ingrese tercera variable:", c(dataset2009_col)),
                                      selectInput("p2009_cuartav", "ingrese cuarta variable:", c(dataset2009_col)),
                                      downloadButton("boton_ttcc_2009", "Descargar"),
                                      verbatimTextOutput("tabla_d_c_generalizada_2009") %>% withSpinner(type = 5, color = "#e6460b", size = 0.5),
                                      
                                      downloadButton("boton_ttcc_2009_pon", "Descargar"),
                                      #   tableOutput("tabla_d_c_generalizada") %>% withSpinner(color="#0dc5c1")))),
                                      verbatimTextOutput("tabla_d_c_generalizada_pon_2009") %>% withSpinner(type = 5, color = "#bd1c52", size = 0.5)
                               )
                           )),
                           
                           tabPanel("Cochran–Mantel–Haenszel",fluidRow(column(12,
                                                                              selectInput("p2006_primerav", "ingrese primera variable:", c(dataset2006_col)),
                                                                              selectInput("p2006_segundav", "ingrese segunda variable:", c(dataset2006_col)),
                                                                              selectInput("p2006_tercerav", "ingrese tercera variable:", c(dataset2006_col)),
                                                                              verbatimTextOutput("tabla_chi_generalizada"))))
                ),
                
                
                tabPanel("Frecuencias de respuestas por campo", fluidRow(
                    column(12, includeMarkdown("about_educacion.md.txt")),
                    selectInput("ptabla_2009", "Seleccione la pregunta:", c(dataset2009_col)),
                    column(12, dataTableOutput("prueba_tabla"))
                )),
                
                
                
                
                navbarMenu("Estadísticas y gráficas",
                           tabPanel("Promedios", fluidRow(
                               column(12, includeMarkdown("info_2006_prom.md")),
                               selectInput("input_promedios_2009", "Ingrese variable:", c(dataset2009_col)),
                               column(12, verbatimTextOutput("promedios_2009"))
                           )),
                           
                           tabPanel("Diagrama de caja y bigotes", fluidRow(
                               column(12, includeMarkdown("info_2006_cyb.md")),
                               selectInput("input_cyb_2009", "Ingrese variable:", c(dataset2009_col)),
                               downloadButton("plot_cyb_2009", "Descargar"),
                               column(12, plotOutput("cyb_2009"))
                           ))
                           
                ),
                
                
                

                
                
                
                
                navbarMenu("Filtros agrupados por categorías",
                           tabPanel("Seleccione variable que funga como grupo:", fluidRow(
                               column(12, includeMarkdown("info_2006_prom.md")),
                               selectInput("nivel_filtro_2009", "Seleccione unidad social:", c(data_2009_1_2_colnames)),
                               selectInput("categoria_filtro_2009", "Seleccione atributo:", c(data_2009_5_348_colnames)),

                               column(12, tableOutput("promedios_filtros_2009"))
                           )))
            )
        }
        

        
        else if (user == 2011){
          
            navbarPage(
                
                br(),
                
                tabPanel("Introducción a la Encuesta",
                         fluidRow(column(9, includeMarkdown("about_intro.md")))),
                
                tabPanel("Despliegue total de la base de datos Casen 2011",
                         fluidRow(column(9, includeMarkdown("about_pobporin_vei.txt")),
                                  column(12,  dataTableOutput("table_2011")))),
                
                
                navbarMenu("Módulos de la Casen 2011",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_modulos.txt")))),

                           tabPanel("Módulo 1: Identificación", fluidRow(
                               column(12, includeMarkdown("modulo_1_2011.md")),
                               column(12, dataTableOutput("modulo_1_2011"))
                           )),
                           
                           tabPanel("Módulo 1b: Varianzas", fluidRow(
                             column(12, includeMarkdown("modulo_1b_2011.md")),
                             column(12, dataTableOutput("modulo_1b_2011"))
                           )),
                           
                           tabPanel("Módulo 2: Expansión", fluidRow(
                               column(12, includeMarkdown("modulo_2_2011.md")),
                               column(12, dataTableOutput("modulo_2_2011"))
                           )),
                           
                           tabPanel("Módulo 3: Residentes", fluidRow(
                               column(12, includeMarkdown("modulo_3_2011.md")),
                               column(12, dataTableOutput("modulo_3_2011"))
                           )),
                           
                           tabPanel("Módulo 4: Educación", fluidRow(
                               column(4, includeMarkdown("modulo_4_2011_1.md")),
                               column(4, includeMarkdown("modulo_4_2011_2.md")),
                               column(4, includeMarkdown("modulo_4_2011_3.md")),
                               column(12, dataTableOutput("modulo_4_2011"))
                           )),
                           
                           tabPanel("Módulo 5: Empleo", fluidRow(
                               column(12, includeMarkdown("modulo_5_2011.md")),
                               column(12, dataTableOutput("modulo_5_2011"))
                           )),

                          tabPanel("Tema: Chile solidario", fluidRow(
                             column(12, includeMarkdown("modulo_Chile_solidario_2011.md")),
                             column(12, dataTableOutput("modulo_Chile_solidario_2011"))
                           )),
                             
                           tabPanel("Módulo 7: Ingresos", fluidRow(
                             column(12, includeMarkdown("modulo_7_2011.md")),
                             column(12, dataTableOutput("modulo_7_2011"))
                           )),
                          
                           tabPanel("Módulo 8: Salud", fluidRow(
                             column(12, includeMarkdown("modulo_8_2011.md")),
                             column(12, dataTableOutput("modulo_8_2011"))
                           )),
                           
                          tabPanel("Tema 1: Discapacidad", fluidRow(
                            column(12, includeMarkdown("modulo_Discapacidad_2011.md")),
                            column(12, dataTableOutput("modulo_Discapacidad_2011"))
                          )), 
                          
                           
                           tabPanel("Tema 2: Quien responde al módulo Salud", fluidRow(
                             column(12, includeMarkdown("modulo_Quien_responde_al_modulo_Salud_2011.md")),
                             column(12, dataTableOutput("modulo_Quien_responde_al_modulo_Salud_2011"))
                           )),

                           tabPanel("Tema 3: Migración", fluidRow(
                             column(12, includeMarkdown("modulo_Migracion_2011.md")),
                             column(12, dataTableOutput("modulo_Migracion_2011"))
                           )),

                           tabPanel("Tema 4: Autobiografía (Padres y su nivel educacional)", fluidRow(
                             column(12, includeMarkdown("modulo_Autobiografía_2011.md")),
                             column(12, dataTableOutput("modulo_Autobiografía_2011"))
                           )),
                           tabPanel("Tema 5: Etnia", fluidRow(
                             column(12, includeMarkdown("modulo_Etnia_2011.md")),
                             column(12, dataTableOutput("modulo_Etnia_2011"))
                           )),
                           tabPanel("Tema 6: Participación", fluidRow(
                             column(12, includeMarkdown("modulo_Participacion_2011.md")),
                             column(12, dataTableOutput("modulo_Participacion_2011"))
                           )),
                           
                          tabPanel("Módulo 3: Residentes: Patrimonio.", fluidRow(
                            column(12, includeMarkdown("modulo_Residentes_Patrimonio_2011.md")),
                            column(12, dataTableOutput("modulo_Residentes_Patrimonio_2011"))
                          )),
                          
                          tabPanel("Tema 7: Internet", fluidRow(
                            column(12, includeMarkdown("modulo_Internet_2011.md")),
                            column(12, dataTableOutput("modulo_Internet_2011"))
                          )),
                          
                          tabPanel("Tema 8: Teléfono móvil", fluidRow(
                            column(12, includeMarkdown("modulo_telefono_movil_2011.md")),
                            column(12, dataTableOutput("modulo_telefono_movil_2011"))
                          )),
                          
                          tabPanel("Tema 9: Satisfacción con la vida", fluidRow(
                            column(12, includeMarkdown("modulo_Satisfaccion_con_la_vida_2011.md")),
                            column(12, dataTableOutput("modulo_Satisfaccion_con_la_vida_2011"))
                          )),

                          tabPanel("Tema 10: Quien responde al módulo residentes", fluidRow(
                            column(12, includeMarkdown("modulo_Quien_responde_al_modulo_residentes_2011.md")),
                            column(12, dataTableOutput("modulo_Quien_responde_al_modulo_residentes_2011"))
                          )),

                           tabPanel("Módulo 9: Vivienda", fluidRow(
                             column(12, includeMarkdown("modulo_9_2011.md")),
                             column(12, dataTableOutput("modulo_9_2011"))
                           )),
                          
                          tabPanel("Tema 11: Lugar donde se produce la entrevista", fluidRow(
                            column(12, includeMarkdown("modulo_Lugar_donde_se_produce_la_entrevista_2011.md")),
                            column(12, dataTableOutput("modulo_Lugar_donde_se_produce_la_entrevista_2011"))
                          )),
                          
                          
                          "----",
                          "",
                          
                          # Variables creadas:
                          # https://studylib.es/doc/1097171/manual-usuario-casen-2006.pdf
                          
                           tabPanel("1 Variables creadas: Educación", fluidRow(
                             column(12, includeMarkdown("vvcc_Educacion_2011.md")),
                             column(12, dataTableOutput("vvcc_Educacion_2011"))
                           )),

                          tabPanel("2 Variables creadas: Empleo", fluidRow(
                            column(12, includeMarkdown("vvcc_Empleooo_2011.md")),
                            column(12, dataTableOutput("vvcc_Empleo_2011"))
                          )),
                          tabPanel("3 Variables creadas: Línea de pobreza, quintiles y deciles de ingreso", fluidRow(
                            column(12, includeMarkdown("vvcc_Linea_de_pobreza_2011.md")),
                            column(12, dataTableOutput("vvcc_Linea_de_pobreza_2011"))
                          )),
                          tabPanel("4 Variables creadas: Ingresos del trabajo", fluidRow(
                            column(12, includeMarkdown("vvcc_Ingresos_del_trabajo_2011.md")),
                            column(12, dataTableOutput("vvcc_Ingresos_del_trabajo_2011"))
                          )),
                          tabPanel("5 Variables creadas: Subsidios monetarios", fluidRow(
                            column(12, includeMarkdown("vvcc_Subsidios_monetarios_2011.md")),
                            column(12, dataTableOutput("vvcc_Subsidios_monetarios_2011"))
                          )),
                          tabPanel("6 Variables creadas: Otros Ingresos", fluidRow(
                            column(12, includeMarkdown("vvcc_Otros_Ingresos_2011.md")),
                            column(12, dataTableOutput("vvcc_Otros_Ingresos_2011"))
                          )),
                          tabPanel("7 Variables creadas: Índices e indicadores", fluidRow(
                            column(12, includeMarkdown("vvcc_Indices_2011.md")),
                            column(12, dataTableOutput("vvcc_Indices_2011"))
                          )),
                           
                          "----",
                          "",
                          
                          tabPanel("Tema 12: Índice de Hacinamiento", fluidRow(
                            column(12, includeMarkdown("vvcc_Indice_de_Hacinamiento_2011.md")),
                            column(12, dataTableOutput("vvcc_Indice_de_Hacinamiento_2011"))
                          )),
                          
                          tabPanel("Variable: Fecha", fluidRow(
                            column(12, includeMarkdown("fecha_2011.md")),
                            column(12, dataTableOutput("fecha_2011"))
                          )),

                           tabPanel(" ")),
                
                
                
                
                
                
                
                
                
                navbarMenu("Tabla de contingencia > 2x2",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           tabPanel("Tablas de contingencia > 2x2",fluidRow(
                             # selectInput("nada", "Identifique la variable:", c(data_2017_colnames)),
                             column(12,
                                    
                                    selectInput("p2011_primerav", "ingrese primera variable:", c(dataset2011_col)),
                                    selectInput("p2011_segundav", "ingrese segunda variable:", c(dataset2011_col)),
                                    selectInput("p2011_tercerav", "ingrese tercera variable:", c(dataset2011_col)),
                                    selectInput("p2011_cuartav", "ingrese cuarta variable:", c(dataset2011_col)),
                                    downloadButton("boton_ttcc_2011", "Descargar"),
                                    verbatimTextOutput("tabla_d_c_generalizada_2011") %>% withSpinner(type = 5, color = "#e6460b", size = 0.5),
                                    
                                    downloadButton("boton_ttcc_2011_pon", "Descargar"),
                                    verbatimTextOutput("tabla_d_c_generalizada_pon_2011") %>% withSpinner(type = 5, color = "#bd1c52", size = 0.5)
                             )
                           )),
                           
                           tabPanel("Cochran–Mantel–Haenszel",fluidRow(column(12,
                                                                              selectInput("p2006_primerav", "ingrese primera variable:", c(dataset2011_col)),
                                                                              selectInput("p2006_segundav", "ingrese segunda variable:", c(dataset2011_col)),
                                                                              selectInput("p2006_tercerav", "ingrese tercera variable:", c(dataset2011_col)),
                                                                              verbatimTextOutput("tabla_chi_generalizada"))))
                ),
                
                tabPanel("Frecuencias de respuestas por campo", fluidRow(
                    column(12, includeMarkdown("about_educacion.md.txt")),
                    selectInput("ptabla_2011", "prueba tabla:", c(dataset2011_col)),
                    column(12, dataTableOutput("prueba_tabla"))
                )),
                
                
                navbarMenu("Estadísticas y gráficas",
                           tabPanel("Promedios", fluidRow(
                             column(12, includeMarkdown("info_2006_prom.md")),
                             selectInput("input_promedios_2011", "Ingrese variable:", c(dataset2011_col)),
                             column(12, verbatimTextOutput("promedios_2011"))
                           )),
                           
                           tabPanel("Diagrama de caja y bigotes", fluidRow(
                             column(12, includeMarkdown("info_2006_cyb.md")),
                             selectInput("input_cyb_2011", "Ingrese variable:", c(dataset2011_col)),
                             downloadButton("plot_cyb_2011", "Descargar"),
                             column(12, plotOutput("cyb_2011"))
                           ))
                           
                ),
                
                navbarMenu("Filtros agrupados por categorías",
                           tabPanel("Seleccione variable que funga como grupo:", fluidRow(
                             column(12, includeMarkdown("info_2006_prom.md")),
                             selectInput("nivel_filtro_2011", "Seleccione unidad social:", c(data_2011_1_2_colnames)),
                             selectInput("categoria_filtro_2011", "Seleccione atributo:", c(data_2011_5_348_colnames)),
                             
                             column(12, tableOutput("promedios_filtros_2011"))
                           )))
                
                
            )
        }
        
       
        
        else if (user == 2013){
            navbarPage(
                
                
                br(),
                
                tabPanel("Introducción a la Encuesta",
                         fluidRow(column(9, includeMarkdown("about_intro.md")))),
                
                
                
                tabPanel("Despliegue total de la base de datos Casen 2013",
                         fluidRow(column(3, includeMarkdown("info_2006_tabla.md")),
                                  column(12, dataTableOutput('table_2013')))),
                
                
                
                tabPanel("Variables de identificación",
                         fluidRow(column(9, includeMarkdown("about_varia_intro.md")))),
                

                
                navbarMenu("Factores de expansión",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("facintro.txt")))),
                           "----",
                           "",
                           
                           tabPanel("Factor de Expansión Comunal",
                                    fluidRow(column(9, includeMarkdown("facco.txt")))),
                           
                           tabPanel("Factor de Expansión Regional",
                                    fluidRow(column(9, includeMarkdown("facor.txt")))),
                           
                           tabPanel("Factor de Expansión sobre orientación sexual e identidad de género",
                                    fluidRow(column(9, includeMarkdown("facreg.txt")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Varianza ",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_varianza.txt")))),
                           "----",
                           "",
                           
                           tabPanel("Estratos de Varianza",
                                    fluidRow(column(9, includeMarkdown("varianza_estratos.txt")))),
                           
                           tabPanel("Conglomerados de Varianza",
                                    fluidRow(column(9, includeMarkdown("varianza_conglomerado.txt")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Descripción conceptual de módulos y variables",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_modulos.txt")))),
                           
                           "----",
                           "",
                           
                           ####################### Registro residentes ########################
                           
                           tabPanel("Primer módulo: Registro Residentes -columnas 1:42-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Identificacion de los encuestados -1:8-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Factores de expansion -9:11-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Varianza -12:13-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Fecha de la entrevista -14:16-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Personas, nucleos, hogares y parejas -17:20-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Preguntas identificatorias -21:36-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Dificultades o limitaciones fisicas -37:42-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           ############################# Modulo educacion ######################################
                           
                           "----",
                           "",
                           tabPanel("Segundo módulo (E): Educacion -columnas 43:101-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Dificultades o limitaciones fisicas -43:69-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Recepcion de beneficios estatales -70:92-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Pago autonomo por educacion -93:101-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           "----",
                           "",                           
                           
                           ############################# Modulo Trabajo ######################################
                           
                           
                           tabPanel("Tercer módulo (O): Trabajo -columnas 102:151-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           
                           
                           "----",
                           "",                           
                           
                           ############################# Modulo ingresos ######################################
                           
                           
                           tabPanel("Cuarto módulo (Y): Ingresos -columnas 152:304-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Alimentacion y estado nutricional -columnas 305:309-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           

                           
                           tabPanel("Quinto módulo (S): Salud", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("Sexto módulo (R): Identidades, redes y participación", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("Séptimo módulo (V): Vivienda y Entorno", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           "----",
                           "",
                           tabPanel("______Submódulo: Hogares carentes: hh_d_asis-hh_d_seg", fluidRow(
                             column(12, includeMarkdown("hh_d_asis.md"))
                           )),
                           
                           tabPanel(" ")),
                
                
                navbarMenu("Módulos 2013",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_modulos.txt")))),
                           "----",
                           "",
                           tabPanel("Módulo 1: Identificación", fluidRow(
                             column(12, includeMarkdown("modulo_1_2013.md")),
                             column(12, dataTableOutput("modulo_1_2013"))
                           )),
                           
                           tabPanel("Módulo 2: Vivienda compartida", fluidRow(
                             column(12, includeMarkdown("modulo_2_2013.md")),
                             column(12, dataTableOutput("modulo_2_2013"))
                           )),
                           
                           tabPanel("Módulo 3: Educación ", fluidRow(
                             column(12, includeMarkdown("modulo_3_2013.md")),
                             column(12, dataTableOutput("modulo_3_2013"))
                           )),
                           
                           tabPanel("Módulo 4: Empleo ", fluidRow(
                             column(4, includeMarkdown("modulo_4_2013_1.md")),
                             column(4, includeMarkdown("modulo_4_2013_2.md")),
                             column(4, includeMarkdown("modulo_4_2013_3.md")),
                             column(12, dataTableOutput("modulo_4_2013"))
                           )),
                           
                           tabPanel("Módulo 5: Ingresos ", fluidRow(
                             column(12, includeMarkdown("modulo_5_2013.md")),
                             column(12, dataTableOutput("modulo_5_2013"))
                           )),
                           
                           tabPanel("Módulo 6: Salud", fluidRow(
                             column(12, includeMarkdown("modulo_6_2013.md")),
                             column(12, dataTableOutput("modulo_6_2013"))
                           )),
                           
                           tabPanel("Tema 1: Discapacidad", fluidRow(
                             column(12, includeMarkdown("modulo_7_2013.md")),
                             column(12, dataTableOutput("modulo_7_2013"))
                           )),
                           
                           tabPanel("Tema 2: Quién responde el módulo salud", fluidRow(
                             column(12, includeMarkdown("modulo_8_2013.md")),
                             column(12, dataTableOutput("modulo_8_2013"))
                           )),
                           
                           
                           ####################################333
                           
                           tabPanel("Tema 3: Migración", fluidRow(
                             column(12, includeMarkdown("modulo_9_2013.md")),
                             column(12, dataTableOutput("modulo_9_2013"))
                           )),
                           
                           tabPanel("Tema 4: Auto-biografía", fluidRow(
                             column(12, includeMarkdown("modulo_10_2013.md")),
                             column(12, dataTableOutput("modulo_10_2013"))
                           )),
                           
                           tabPanel("Tema 5: Etnia", fluidRow(
                             column(12, includeMarkdown("modulo_11_2013.md")),
                             column(12, dataTableOutput("modulo_11_2013"))
                           )),
                           
                           tabPanel("Tema 6: Participación", fluidRow(
                             column(12, includeMarkdown("modulo_12_2013.md")),
                             column(12, dataTableOutput("modulo_12_2013"))
                           )),
                           
                           tabPanel("___Módulo 7: Residentes (Patrimonio)", fluidRow(
                             column(12, includeMarkdown("modulo_13_2013.md")),
                             column(12, dataTableOutput("modulo_13_2013"))
                           )),
                           
                           tabPanel("___Módulo 7: Residentes (Internet)", fluidRow(
                             column(12, includeMarkdown("modulo_14_2013.md")),
                             column(12, dataTableOutput("modulo_14_2013"))
                           )),
                           
                           tabPanel("___Módulo 7: Residentes (Teléfono móvil)", fluidRow(
                             column(12, includeMarkdown("modulo_15_2013.md")),
                             column(12, dataTableOutput("modulo_15_2013"))
                           )),
                           
                           tabPanel("___Módulo 7: Residentes (Satisfacción con la vida)", fluidRow(
                             column(12, includeMarkdown("modulo_16_2013.md")),
                             column(12, dataTableOutput("modulo_16_2013"))
                           )),
                           
                           tabPanel("___Módulo 7: Residentes (Conseguir dinero)", fluidRow(
                             column(12, includeMarkdown("modulo_17_2013.md")),
                             column(12, dataTableOutput("modulo_17_2013"))
                           )),
                           
                           tabPanel("___Módulo 7: Residentes (Respuesta al módulo residentes)", fluidRow(
                             column(12, includeMarkdown("modulo_18_2013.md")),
                             column(12, dataTableOutput("modulo_18_2013"))
                           )),
                           
                           tabPanel("Módulo 8: Vivienda", fluidRow(
                             column(12, includeMarkdown("modulo_19_2013.md")),
                             column(12, dataTableOutput("modulo_19_2013"))
                           )),
                           
                           tabPanel("Tema 7: (Lugar donde se produce la entrevista)", fluidRow(
                             column(12, includeMarkdown("modulo_20_2013.md")),
                             column(12, dataTableOutput("modulo_20_2013"))
                           )),
                           
                           tabPanel("Módulo 9: Ingresos", fluidRow(
                             column(12, includeMarkdown("modulo_21_2013.md")),
                             column(12, dataTableOutput("modulo_21_2013"))
                           )),
                           
                           tabPanel("Módulo 10: Expansión", fluidRow(
                             column(12, includeMarkdown("modulo_22_2013.md")),
                             column(12, dataTableOutput("modulo_22_2013"))
                           )),
                           
                           tabPanel("Módulo 11: Varianzas", fluidRow(
                             column(12, includeMarkdown("modulo_23_2013.md")),
                             column(12, dataTableOutput("modulo_23_2013"))
                           )),
                           
                           
                           tabPanel("Módulo 12: Fecha", fluidRow(
                             column(12, includeMarkdown("modulo_24_2013.md")),
                             column(12, dataTableOutput("modulo_24_2013"))
                           )),
                           
                           
                           tabPanel("Módulo 13: Línea de pobreza, quintiles y deciles de ingreso", fluidRow(
                             column(12, includeMarkdown("modulo_25_2013.md")),
                             column(12, dataTableOutput("modulo_25_2013"))
                           )),
                           
                           
                           tabPanel("14 Tema: Número de personas en el hogar", fluidRow(
                             column(12, includeMarkdown("modulo_26_2013.md")),
                             column(12, dataTableOutput("modulo_26_2013"))
                           )),
                           
                           
                           tabPanel("15 Variables creadas: Educación", fluidRow(
                             column(12, includeMarkdown("modulo_27_2013.md")),
                             column(12, dataTableOutput("modulo_27_2013"))
                           )),
                           
                           tabPanel("16 Variables creadas: Empleo", fluidRow(
                             column(12, includeMarkdown("modulo_28_2013.md")),
                             column(12, dataTableOutput("modulo_28_2013"))
                           )),
                           
                           tabPanel("17 Variables creadas: Indicadores de calidad de la vivienda", fluidRow(
                             column(12, includeMarkdown("modulo_29_2013.md")),
                             column(12, dataTableOutput("modulo_29_2013"))
                           )),
                           
                           tabPanel("18 Variables creadas: Indicadores de allegamiento", fluidRow(
                             column(12, includeMarkdown("modulo_30_2013.md")),
                             column(12, dataTableOutput("modulo_30_2013"))
                           )),
                           
                           tabPanel("19 Variables creadas: Hacinamiento", fluidRow(
                             column(12, includeMarkdown("modulo_31_2013.md")),
                             column(12, dataTableOutput("modulo_31_2013"))
                           )),
                           
                           
                           
                           
                          
                           # 
                           tabPanel(" ")),
                
                
                
                
                
                
                navbarMenu("Cálculos propios de los Indicadores Casen",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("about_intro_cc.txt"))
                                    )),
                           "----",
                           "",
                           
                           tabPanel("Variables",
                                    fluidRow(column(9, includeMarkdown("about_variables_cc.txt")),
                                             column(3,  tableOutput("contents8")))),
                           
                           tabPanel(" ")
                           ),
                
                
                
                
                
                

                navbarMenu("Variables e indicadores de pobreza",
                           tabPanel("Introducción",
                                    fluidRow(column(9, includeMarkdown("about_veip.md")))),
                           "----",
                           
                           tabPanel("Pobreza por ingresos",
                                    fluidRow(column(9, includeMarkdown("about_pobporin_vei.txt")),
                                             column(3,  tableOutput("contents12")))),
                           
                           tabPanel("Pobreza multidimensional",
                                    tabsetPanel(
                                        tabPanel("Redes y cohesión social", fluidRow(column(9, includeMarkdown("pmredes.md")))),
                                        tabPanel("Educación",  fluidRow(column(9, includeMarkdown("pm_educacion.txt")))),
                                        tabPanel("Salud", fluidRow(column(9, includeMarkdown("pm_salud.txt")))),
                                        tabPanel("Trabajo y seguridad social", fluidRow(column(9, includeMarkdown("pm_ts.txt")))),
                                        tabPanel("Vivienda y entorno", fluidRow(column(9, includeMarkdown("pm_vivienda.txt"))))
                                    )),
                           
                           
                           tabPanel("Pobreza multidimensional 4d y 5d",
                                    fluidRow(column(9, includeMarkdown("about_pobmul4d5d_vei.txt")),
                                             column(3,  tableOutput("contents14")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Indicadores",
                           tabPanel("Introducción",
                                    fluidRow(column(9, includeMarkdown("about_intro_mds.txt")))),
                           "----",
                           "",
                           
                           tabPanel("Variables",
                                    tabsetPanel(
                                        tabPanel("numper", fluidRow(column(9, includeMarkdown("var_numper.txt")))),
                                        tabPanel("asiste",  fluidRow(column(9, includeMarkdown("var_asiste.txt")))),
                                        tabPanel("esc", fluidRow(column(9, includeMarkdown("var_esc.txt")))),
                                        tabPanel("educ", fluidRow(column(9, includeMarkdown("var_educ.txt")))),
                                        tabPanel("depen", fluidRow(column(9, includeMarkdown("var_depen.txt")))),
                                        tabPanel("activ", fluidRow(column(9, includeMarkdown("var_activ.txt")))),
                                        tabPanel("indmat", fluidRow(column(9, includeMarkdown("var_indmat.txt")))),
                                        tabPanel("indsan", fluidRow(column(9, includeMarkdown("var_indsan.txt")))),
                                        tabPanel("calglobviv", fluidRow(column(9, includeMarkdown("var_calglobviv.txt")))),
                                        tabPanel("iae", fluidRow(column(9, includeMarkdown("var_iae.txt")))),
                                        tabPanel("iai", fluidRow(column(9, includeMarkdown("var_iai.txt")))),
                                        tabPanel("hacinamiento", fluidRow(column(9, includeMarkdown("var_hacinamiento.txt"))))
                                        
                                    )),
                           
                           tabPanel(" ")),
                
                navbarMenu("Frecuencias de respuestas por campo",
                           
                           

                           
                    tabPanel("Frecuencias por módulo I: Registro de residentes.", fluidRow(
                    column(12, includeMarkdown("info_2006_frec.md")),
                    selectInput("ptabla_2013_I", "Seleccione pregunta:", c(dataset2013_col_I)),
                    column(12, dataTableOutput("frecuencias_I")))),
                
                
                
                tabPanel("Frecuencias por módulo II: Registro de residentes.", fluidRow(
                  column(12, includeMarkdown("info_2006_frec.md")),
                  selectInput("ptabla_2013_II", "Seleccione pregunta:", c(dataset2013_col_II)),
                  column(12, dataTableOutput("frecuencias_II")))),
                
                
                
                tabPanel("Frecuencias por módulo III: Registro de residentes.", fluidRow(
                  column(12, includeMarkdown("info_2006_frec.md")),
                  selectInput("ptabla_2013_III", "Seleccione pregunta:", c(dataset2013_col_III)),
                  column(12, dataTableOutput("frecuencias_III"))
                )),
                
                tabPanel("Frecuencias por módulo IV: Registro de residentes.", fluidRow(
                  column(12, includeMarkdown("info_2006_frec.md")),
                  selectInput("ptabla_2013_IV", "Seleccione pregunta:", c(dataset2013_col_IV)),
                  column(12, dataTableOutput("frecuencias_IV"))
                ))
                
                
                ),

                
                navbarMenu("Estadísticas y gráficas",
                           tabPanel("Promedios", fluidRow(
                             column(12, includeMarkdown("info_2006_prom.md")),
                             selectInput("ptabla_promedios_2013", "Seleccione variable:", c(dataset2013_col)),
                             column(12, verbatimTextOutput("promedios_2013"))
                           )),
                           
                           
                           
                           tabPanel("Diagrama de caja y bigotes", fluidRow(
                             
                             column(12, includeMarkdown("info_2006_cyb.md")),
                             
                             selectInput("ptabla_cyb_2013", "Seleccione la variable:", c(dataset2013_col)),
                             
                             downloadButton("plot_cyb_2013", "Descargar"),
                             
                             column(12, plotOutput("cyb_2013"))
                           ))
                ),
                
                
                
                
                navbarMenu("Tablas de contingencia",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           
                           
                           
                           
                           tabPanel("Tablas de contingencia de 2x2",fluidRow(column(5,
                                                                                    selectInput("ptabla2013_primerav", "ingrese primera variable:", c(dataset2013_col)),
                                                                                    selectInput("ptabla2013_segundav", "ingrese segunda variable:", c(dataset2013_col)),
                                                                                    downloadButton("boton_ttcc_2013", "Descargar"),
                                                                                    verbatimTextOutput("tabla_d_c_2013") %>% withSpinner(color="#c50d78"),
                                                                                    downloadButton("boton_ponderadas_2013", "Descargar"),
                                                                                    verbatimTextOutput("tabla_d_c_ponderadas") %>% withSpinner(color="#0e8c0e")))),
                                                                                    
                           tabPanel("Pearson's Chi-squared test",fluidRow(column(3,selectInput("ptabla2013_primerav", "ingrese primera variable:", c(dataset2013_col)),
                                                                                 selectInput("ptabla2013_segundav", "ingrese segunda variable:", c(dataset2013_col)),
                                                                                 verbatimTextOutput("tabla_chi13"))))
                ),
                
                
                
                navbarMenu("Tablas de contingencia > 2x2",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           
                           tabPanel("Tablas de contingencia > 2x2",fluidRow(
                             selectInput("nada", "Identifique la variable:", c(data_2017_colnames)),
                             column(12,
                                    selectInput("ptabla2013_primeravx", "ingrese primera variable:", c(dataset2013_col)),
                                    selectInput("ptabla2013_segundavx", "ingrese segunda variable:", c(dataset2013_col)),
                                    selectInput("ptabla2013_terceravx", "ingrese tercera variable:", c(dataset2013_col)),
                                    selectInput("ptabla2013_cuartavx", "ingrese cuarta variable:", c(dataset2013_col)),
                                    
                                    downloadButton("tabla_2013_csv", "Descargar"),
                                    verbatimTextOutput("tabla_d_c_generalizada_2013") %>% withSpinner(type = 5, color = "#e6460b", size = 0.5),
                                    
                                    downloadButton("tabla_2013_csv_pon", "Descargar"),
                                    verbatimTextOutput("tabla_d_c_generalizada_2013_pon") %>% withSpinner(type = 5, color = "#bd1c52", size = 0.5)
                                    
                                    ))),
                                    
                           
                           tabPanel("Cochran–Mantel–Haenszel",fluidRow(column(12,
                                                                              selectInput("ptabla2013_primerav", "ingrese primera variable:", c(dataset2013_col)),
                                                                              selectInput("ptabla2013_segundav", "ingrese segunda variable:", c(dataset2013_col)),
                                                                              selectInput("ptabla2013_tercerav", "ingrese tercera variable:", c(dataset2013_col)),
                                                                              verbatimTextOutput("tabla_chi_generalizada_2013"))))
                ),
                
                navbarMenu("Tablas de contingencia > 2x2 para promedios",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           tabPanel("Tablas de contingencia > 2x2",fluidRow(column(7,
                                                                                   selectInput("ptabla2013_primeravx_prom", "ingrese primera variable:", c(datos_df_exp)),
                                                                                   selectInput("ptabla2013_segundavx_prom", "ingrese segunda variable:", c(datos_df_exp)),
                                                                                   selectInput("ptabla2013_terceravx_prom", "ingrese tercera variable:", c(datos_df_exp)),
                                                                                   
                                                                                   selectInput("ptabla2013_cuartavx", "ingrese cuarta variable:", c(datos_df_exp)),
                                                                                   
                                                                                   downloadButton("boton_ttcc_mayor_23_prom", "Descargar"),
                                                                                   
                                                                                   verbatimTextOutput("tabla3_d_c_generalizada_prom")))),
                           
                           tabPanel("Cochran–Mantel–Haenszel",fluidRow(column(12,
                                                                              selectInput("ptabla2013_primerav_prom", "ingrese primera variable:", c(datos_df_exp)),
                                                                              selectInput("ptabla2013_segundav_prom", "ingrese segunda variable:", c(datos_df_exp)),
                                                                              selectInput("ptabla2013_tercerav_prom", "ingrese tercera variable:", c(datos_df_exp)),
                                                                              verbatimTextOutput("tabla3_chi_generalizada_prom"))))
                ),
                
                navbarMenu("Diccionario de variables",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           
                           tabPanel("exp",fluidRow(
                             column(12, includeMarkdown("info_rpubs.md")),
                             
                             column(12 )))
                ),
                
                
                navbarMenu("Filtros agrupados por categorías",
                           tabPanel("Seleccione variable que funga como grupo:", fluidRow(
                             column(12, includeMarkdown("info_2006_prom.md")),
                             selectInput("nivel_filtro", "Seleccione unidad social:", c(data_2013_1_2_colnames)),
                             selectInput("categoria_filtro", "Seleccione atributo:", c(data_2013_5_348_colnames)),
                             column(12, tableOutput("promedios_filtros_2013"))
                           ))
                ),
                
                navbarMenu("Análisis de series en el tiempo",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           
                           tabPanel("exp",fluidRow(
                             column(12, includeMarkdown("info_rpubs.md")),
                             
                             column(12 )))
                ),
                navbarMenu("Análisis de algunas tablas de contingencia",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           
                           tabPanel("exp",fluidRow(
                             column(12, includeMarkdown("info_rpubs.md")),
                             
                             column(12 )))
                )
                
            )
        }
        
        else if (user == 2015){
            navbarPage(
                
                
                br(),
                
                tabPanel("Introducción a la Encuesta",
                         fluidRow(column(9, includeMarkdown("about_intro.md")))),
                
                
                tabPanel("Despliegue total de la base de datos Casen 2015",
                         fluidRow(column(3, includeMarkdown("info_2006_tabla.md")),
                                  column(12, dataTableOutput('table_2015')))),
                
                
                
                
                tabPanel("Variables de identificación",
                         fluidRow(column(9, includeMarkdown("about_varia_intro.md")))),
                

                
                navbarMenu("Factores de expansión",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("facintro.txt")))),
                           "----",
                           "",
                           
                           tabPanel("Factor de Expansión Comunal",
                                    fluidRow(column(9, includeMarkdown("facco.txt")))),
                           
                           tabPanel("Factor de Expansión Regional",
                                    fluidRow(column(9, includeMarkdown("facor.txt")))),
                           
                           tabPanel("Factor de Expansión sobre orientación sexual e identidad de género",
                                    fluidRow(column(9, includeMarkdown("facreg.txt")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Varianza ",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_varianza.txt")))),
                           "----",
                           "",
                           
                           tabPanel("Estratos de Varianza",
                                    fluidRow(column(9, includeMarkdown("varianza_estratos.txt")))),
                           
                           tabPanel("Conglomerados de Varianza",
                                    fluidRow(column(9, includeMarkdown("varianza_conglomerado.txt")))),
                           
                           tabPanel(" ")),
                
                
                navbarMenu("Descripción conceptual de módulos y variables",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_modulos.txt")))),
                           
                           "----",
                           "",
                           
                           ####################### Registro residentes ########################
                           
                           tabPanel("Primer módulo: Registro Residentes -columnas 1:42-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Identificacion de los encuestados -1:8-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Factores de expansion -9:11-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Varianza -12:13-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Fecha de la entrevista -14:16-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Personas, nucleos, hogares y parejas -17:20-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Preguntas identificatorias -21:36-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Dificultades o limitaciones fisicas -37:42-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           ############################# Modulo educacion ######################################
                           
                           "----",
                           "",
                           tabPanel("Segundo módulo (E): Educacion -columnas 43:101-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Dificultades o limitaciones fisicas -43:69-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Recepcion de beneficios estatales -70:92-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Pago autonomo por educacion -93:101-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           "----",
                           "",                           
                           
                           ############################# Modulo Trabajo ######################################
                           
                           
                           tabPanel("Tercer módulo (O): Trabajo -columnas 102:151-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           
                           
                           "----",
                           "",                           
                           
                           ############################# Modulo ingresos ######################################
                           
                           
                           tabPanel("Cuarto módulo (Y): Ingresos -columnas 152:304-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Alimentacion y estado nutricional -columnas 305:309-", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           
                           
                           tabPanel("Quinto módulo (S): Salud", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("Sexto módulo (R): Identidades, redes y participación", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("Séptimo módulo (V): Vivienda y Entorno", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           "----",
                           "",
                           tabPanel("______Submódulo: Hogares carentes: hh_d_asis-hh_d_seg", fluidRow(
                             column(12, includeMarkdown("hh_d_asis.md"))
                           )),
                           
                           tabPanel(" ")),
                
                ###########################################################################################################################################
                
                navbarMenu("Módulos 2015",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_modulos.txt")))),
                           "----",
                           "",
                           tabPanel("Módulo 1: Identificación", fluidRow(
                             column(12, includeMarkdown("modulo_1_2013.md")),
                             column(12, dataTableOutput("modulo_1_2015"))
                           )),
                           
                           tabPanel("Módulo 2: Educación", fluidRow(
                             column(12, includeMarkdown("modulo_2_2015_edu.md")),
                             column(12, dataTableOutput("modulo_2_2015"))
                           )),
                           
                           tabPanel("Módulo 3: Empleo ", fluidRow(
                             column(12, includeMarkdown("modulo_3_2015.md")),
                             column(12, dataTableOutput("modulo_3_2015"))
                           )),
                           
                           tabPanel("Módulo 4: Ingresos ", fluidRow(
                             column(4, includeMarkdown("modulo_4_2015_1.md")),
                             column(4, includeMarkdown("modulo_4_2013_2.md")),
                             column(4, includeMarkdown("modulo_4_2013_3.md")),
                             column(12, dataTableOutput("modulo_4_2015"))
                           )),
                           
                           tabPanel("Módulo 5: Salud ", fluidRow(
                             column(12, includeMarkdown("modulo_5_2015.md")),
                             column(12, dataTableOutput("modulo_5_2015"))
                           )),
                           
                           tabPanel("Tema 1: Discapacidad", fluidRow(
                             column(12, includeMarkdown("modulo_6_2015.md")),
                             column(12, dataTableOutput("modulo_6_2015"))
                           )),
                           
                           tabPanel("Módulo 6: Migración", fluidRow(
                             column(12, includeMarkdown("modulo_7_2015.md")),
                             column(12, dataTableOutput("modulo_7_2015"))
                           )),
                           
                           tabPanel("Tema 2: Etnias", fluidRow(
                             column(12, includeMarkdown("modulo_8_2015.md")),
                             column(12, dataTableOutput("modulo_8_2015"))
                           )),
                           
                           tabPanel("Tema 3: Redes y Participación", fluidRow(
                             column(12, includeMarkdown("modulo_9_2015.md")),
                             column(12, dataTableOutput("modulo_9_2015"))
                           )),
                           
                           tabPanel("Módulo 7: Discriminación", fluidRow(
                             column(12, includeMarkdown("modulo_10_2015.md")),
                             column(12, dataTableOutput("modulo_10_2015"))
                           )),
                           
                           tabPanel("Tema 4: Autobiografia", fluidRow(
                             column(12, includeMarkdown("modulo_11_2015.md")),
                             column(12, dataTableOutput("modulo_11_2015"))
                           )),
                           
                           tabPanel("Módulo 8: Residentes (Patrimonio)", fluidRow(
                             column(12, includeMarkdown("modulo_12_2015.md")),
                             column(12, dataTableOutput("modulo_12_2015"))
                           )),
                           
                           tabPanel("Módulo 9: Residentes (Internet)", fluidRow(
                             column(12, includeMarkdown("modulo_13_2015.md")),
                             column(12, dataTableOutput("modulo_13_2015"))
                           )),
                           
                           tabPanel("Módulo 10: Residentes (Telefono movil)", fluidRow(
                             column(12, includeMarkdown("modulo_14_2015.md")),
                             column(12, dataTableOutput("modulo_14_2015"))
                           )),
                           
                           tabPanel("Tema 5: Orientación sexual", fluidRow(
                             column(12, includeMarkdown("modulo_15_2015.md")),
                             column(12, dataTableOutput("modulo_15_2015"))
                           )),
                           
                           tabPanel("Tema 6: Respuesta en módulo residentes", fluidRow(
                             column(12, includeMarkdown("modulo_16_2015.md")),
                             column(12, dataTableOutput("modulo_16_2015"))
                           )),
                           
                           tabPanel("Módulo 11: Vivienda", fluidRow(
                             column(12, includeMarkdown("modulo_17_2015.md")),
                             column(12, dataTableOutput("modulo_17_2015"))
                           )),
                           
                           tabPanel("Módulo 12: Ingresos", fluidRow(
                             column(12, includeMarkdown("modulo_18_2015.md")),
                             column(12, dataTableOutput("modulo_18_2015"))
                           )),
                           
                           tabPanel("Módulo 13: Pobreza, quintiles y deciles", fluidRow(
                             column(12, includeMarkdown("modulo_19_2015.md")),
                             column(12, dataTableOutput("modulo_19_2015"))
                           )),
                           
                           tabPanel("Módulo 14: Expansión", fluidRow(
                             column(12, includeMarkdown("modulo_20_2015.md")),
                             column(12, dataTableOutput("modulo_20_2015"))
                           )),
                           
                           tabPanel("Tema 7: Comunas auto-representadas", fluidRow(
                             column(12, includeMarkdown("modulo_21_2015.md")),
                             column(12, dataTableOutput("modulo_21_2015"))
                           )),
                           
                           tabPanel("Módulo 15: Varianzas", fluidRow(
                             column(12, includeMarkdown("modulo_22_2015.md")),
                             column(12, dataTableOutput("modulo_22_2015"))
                           )),
                           
                           tabPanel("Módulo 16: Fecha", fluidRow(
                             column(12, includeMarkdown("modulo_23_2015.md")),
                             column(12, dataTableOutput("modulo_23_2015"))
                           )),
                           
                           
                           tabPanel("Tema 8: Número de personas en el hogar", fluidRow(
                             column(12, includeMarkdown("modulo_24_2015.md")),
                             column(12, dataTableOutput("modulo_24_2015"))
                           )),
                           
                           
                           tabPanel("Módulo 17: Variables creadas: Educación", fluidRow(
                             column(12, includeMarkdown("modulo_25_2015.md")),
                             column(12, dataTableOutput("modulo_25_2015"))
                           )),
                           
                           
                           ##############################
                           ##############################
                           ##############################
                           
                           
                           tabPanel("Módulo 18: Variables creadas: Condición de actividad", fluidRow(
                             column(12, includeMarkdown("modulo_26_2015.md")),
                             column(12, dataTableOutput("modulo_26_2015"))
                           )),
                           
                           
                           tabPanel("Módulo 19: Variables creadas: Indicadores de calidad de la vivienda", fluidRow(
                             column(12, includeMarkdown("modulo_27_2015.md")),
                             column(12, dataTableOutput("modulo_27_2015"))
                           )),
                           
                           tabPanel("Módulo 20: Variables creadas: Indicadores de allegamiento", fluidRow(
                             column(12, includeMarkdown("modulo_28_2015.md")),
                             column(12, dataTableOutput("modulo_28_2015"))
                           )),
                           
                           tabPanel("Módulo 21: Variables creadas: Hacinamiento", fluidRow(
                             column(12, includeMarkdown("modulo_29_2015.md")),
                             column(12, dataTableOutput("modulo_29_2015"))
                           )),
                           
                           tabPanel("Módulo 22: Variables creadas: Carecias", fluidRow(
                             column(12, includeMarkdown("modulo_30_2015.md")),
                             column(12, dataTableOutput("modulo_30_2015"))
                           )),
                           
                           tabPanel("Módulo 23: Variables creadas: Pobreza", fluidRow(
                             column(12, includeMarkdown("modulo_31_2015.md")),
                             column(12, dataTableOutput("modulo_31_2015"))
                           )),
                           
                           
                           ##############################################
                           ##############################################
                           ##############################################
                           
                           
                           tabPanel(" ")),
                
                
                
                navbarMenu("Cálculos propios de los Indicadores Casen",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("about_intro_cc.txt"))
                           )),
                           "----",
                           "",
                           
                           tabPanel("Variables",
                                    fluidRow(column(9, includeMarkdown("about_variables_cc.txt")),
                                             column(3,  tableOutput("contents8")))),
                           
                           tabPanel(" ")
                ),
                
                
                
                
                
        
                navbarMenu("Variables e indicadores de pobreza",
                           tabPanel("Introducción",
                                    fluidRow(column(9, includeMarkdown("about_veip.md")))),
                           "----",
                           
                           tabPanel("Pobreza por ingresos",
                                    fluidRow(column(9, includeMarkdown("about_pobporin_vei.txt")),
                                             column(3,  tableOutput("contents12")))),
                           
                           tabPanel("Pobreza multidimensional",
                                    tabsetPanel(
                                        tabPanel("Redes y cohesión social", fluidRow(column(9, includeMarkdown("pmredes.md")))),
                                        tabPanel("Educación",  fluidRow(column(9, includeMarkdown("pm_educacion.txt")))),
                                        tabPanel("Salud", fluidRow(column(9, includeMarkdown("pm_salud.txt")))),
                                        tabPanel("Trabajo y seguridad social", fluidRow(column(9, includeMarkdown("pm_ts.txt")))),
                                        tabPanel("Vivienda y entorno", fluidRow(column(9, includeMarkdown("pm_vivienda.txt"))))
                                    )),
                           
                           
                           tabPanel("Pobreza multidimensional 4d y 5d",
                                    fluidRow(column(9, includeMarkdown("about_pobmul4d5d_vei.txt")),
                                             column(3,  tableOutput("contents14")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Indicadores",
                           tabPanel("Introducción",
                                    fluidRow(column(9, includeMarkdown("about_intro_mds.txt")))),
                           "----",
                           "",
                           
                           tabPanel("Variables",
                                    tabsetPanel(
                                        tabPanel("numper", fluidRow(column(9, includeMarkdown("var_numper.txt")))),
                                        tabPanel("asiste",  fluidRow(column(9, includeMarkdown("var_asiste.txt")))),
                                        tabPanel("esc", fluidRow(column(9, includeMarkdown("var_esc.txt")))),
                                        tabPanel("educ", fluidRow(column(9, includeMarkdown("var_educ.txt")))),
                                        tabPanel("depen", fluidRow(column(9, includeMarkdown("var_depen.txt")))),
                                        tabPanel("activ", fluidRow(column(9, includeMarkdown("var_activ.txt")))),
                                        tabPanel("indmat", fluidRow(column(9, includeMarkdown("var_indmat.txt")))),
                                        tabPanel("indsan", fluidRow(column(9, includeMarkdown("var_indsan.txt")))),
                                        tabPanel("calglobviv", fluidRow(column(9, includeMarkdown("var_calglobviv.txt")))),
                                        tabPanel("iae", fluidRow(column(9, includeMarkdown("var_iae.txt")))),
                                        tabPanel("iai", fluidRow(column(9, includeMarkdown("var_iai.txt")))),
                                        tabPanel("hacinamiento", fluidRow(column(9, includeMarkdown("var_hacinamiento.txt"))))
                                        
                                    )),
                           
                           tabPanel(" ")),
                
                
                navbarMenu("Frecuencias de respuestas por campo",
                           
                           
                           
                           
                           tabPanel("Frecuencias por módulo I: Registro de residentes.", fluidRow(
                             column(12, includeMarkdown("info_2006_frec.md")),
                             selectInput("ptabla_2015_I", "Seleccione pregunta:", c(dataset2015_col_I)),
                             column(12, dataTableOutput("frecuencias_2015_I")))),
                           
                           
                           
                           tabPanel("Frecuencias por módulo II: Registro de residentes.", fluidRow(
                             column(12, includeMarkdown("info_2006_frec.md")),
                             selectInput("ptabla_2015_II", "Seleccione pregunta:", c(dataset2015_col_II)),
                             column(12, dataTableOutput("frecuencias_2015_II")))),
                           
                           
                           
                           tabPanel("Frecuencias por módulo III: Registro de residentes.", fluidRow(
                             column(12, includeMarkdown("info_2006_frec.md")),
                             selectInput("ptabla_2015_III", "Seleccione pregunta:", c(dataset2015_col_III)),
                             column(12, dataTableOutput("frecuencias_2015_III"))
                           )),
                           
                           tabPanel("Frecuencias por módulo IV: Registro de residentes.", fluidRow(
                             column(12, includeMarkdown("info_2006_frec.md")),
                             selectInput("ptabla_2015_IV", "Seleccione pregunta:", c(dataset2015_col_IV)),
                             column(12, dataTableOutput("frecuencias_2015_IV"))
                           ))
                           
                           
                ),
                
                
                navbarMenu("Estadísticas y gráficas",
                           tabPanel("Promedios", fluidRow(
                             column(12, includeMarkdown("info_2006_prom.md")),
                             selectInput("ptabla_promedios_2015", "Seleccione variable:", c(dataset2015_col)),
                             column(12, verbatimTextOutput("promedios_2015"))
                           )),
                           
                           
                           
                           tabPanel("Diagrama de caja y bigotes", fluidRow(
                             
                             column(12, includeMarkdown("info_2006_cyb.md")),
                             
                             selectInput("ptabla_cyb_2015", "Seleccione la variable:", c(dataset2015_col)),
                             
                             downloadButton("plot_cyb_2015", "Descargar"),
                             
                             column(12, plotOutput("cyb_2015"))
                           ))
                ),
                
                
                
                navbarMenu("Tablas de contingencia",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           
                           
                           
                           
                           tabPanel("Tablas de contingencia de 2x2",fluidRow(column(5,
                                                                                    selectInput("ptabla2015_primerav", "ingrese primera variable:", c()),
                                                                                    selectInput("ptabla2015_segundav", "ingrese segunda variable:", c()),
                                                                                    verbatimTextOutput("tabla_d_c15")))),
                           
                           tabPanel("Pearson's Chi-squared test",fluidRow(column(3, 
                                                                                 selectInput("ptabla2015_primerav", "ingrese primera variable:", c()),
                                                                                 selectInput("ptabla2015_segundav", "ingrese segunda variable:", c()),
                                                                                 verbatimTextOutput("tabla_chi15"))))

                           
                            ) ,
                
                
                
                
                navbarMenu("Tablas de contingencia > 2x2",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           
                           tabPanel("Tablas de contingencia > 2x2",fluidRow(
                             selectInput("nada", "Identifique la variable:", c(data_2017_colnames)),
                             column(12,
                                    selectInput("ptabla2015_primeravx", "ingrese primera variable:", c(dataset2015_col)),
                                    selectInput("ptabla2015_segundavx", "ingrese segunda variable:", c(dataset2015_col)),
                                    selectInput("ptabla2015_terceravx", "ingrese tercera variable:", c(dataset2015_col)),
                                    selectInput("ptabla2015_cuartavx", "ingrese cuarta variable:", c(dataset2015_col)),
                                    
                                    downloadButton("tabla_2015_csv", "Descargar"),
                                    verbatimTextOutput("tabla_d_c_generalizada_2015") %>% withSpinner(type = 5, color = "#e6460b", size = 0.5),
                                    
                                    downloadButton("tabla_2015_csv_pon", "Descargar"),
                                    verbatimTextOutput("tabla_d_c_generalizada_2015_pon") %>% withSpinner(type = 5, color = "#bd1c52", size = 0.5)
                                    
                             ))),
                           
                           
                           tabPanel("Cochran–Mantel–Haenszel",fluidRow(column(12,
                                                                              selectInput("ptabla2015_primerav", "ingrese primera variable:", c(dataset2015_col)),
                                                                              selectInput("ptabla2015_segundav", "ingrese segunda variable:", c(dataset2015_col)),
                                                                              selectInput("ptabla2015_tercerav", "ingrese tercera variable:", c(dataset2015_col)),
                                                                              verbatimTextOutput("tabla_chi_generalizada_2015"))))
                ),
                
                navbarMenu("Tablas de contingencia > 2x2 para promedios",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           tabPanel("Tablas de contingencia > 2x2",fluidRow(column(7,
                                                                                   selectInput("ptabla2015_primeravx_prom", "ingrese primera variable:", c(datos_df_exp)),
                                                                                   selectInput("ptabla2015_segundavx_prom", "ingrese segunda variable:", c(datos_df_exp)),
                                                                                   selectInput("ptabla2015_terceravx_prom", "ingrese tercera variable:", c(datos_df_exp)),
                                                                                   
                                                                                   selectInput("ptabla2015_cuartavx", "ingrese cuarta variable:", c(datos_df_exp)),
                                                                                   
                                                                                   downloadButton("boton_ttcc_mayor_2_prom_2015", "Descargar"),
                                                                                   
                                                                                   verbatimTextOutput("tabla_d_c_generalizada_prom_2015")))),
                           
                           tabPanel("Cochran–Mantel–Haenszel",fluidRow(column(12,
                                                                              selectInput("ptabla2015_primerav_prom", "ingrese primera variable:", c(datos_df_exp)),
                                                                              selectInput("ptabla2015_segundav_prom", "ingrese segunda variable:", c(datos_df_exp)),
                                                                              selectInput("ptabla2015_tercerav_prom", "ingrese tercera variable:", c(datos_df_exp)),
                                                                              verbatimTextOutput("tabla_chi_generalizada_prom_2015"))))
                ),
                
                navbarMenu("Diccionario de variables",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           
                           tabPanel("exp",fluidRow(
                               column(12, includeMarkdown("info_rpubs.md"))))
                ),
                
                navbarMenu("Filtros agrupados por categorías",
                           tabPanel("Seleccione variable que funga como grupo:", fluidRow(
                             column(12, includeMarkdown("info_2006_prom.md")),
                             selectInput("nivel_filtro", "Seleccione unidad social:", c(data_2015_1_2_colnames)),
                             selectInput("categoria_filtro", "Seleccione atributo:", c(data_2015_5_348_colnames)),
                             column(12, tableOutput("promedios_filtros_2015"))
                           ))
                ),
                
                navbarMenu("Análisis de series en el tiempo",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           
                           tabPanel("exp",fluidRow(
                               column(12, includeMarkdown("info_rpubs.md"))))
                ),
                
                navbarMenu("Análisis de algunas tablas de contingencia",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           
                           tabPanel("exp",fluidRow(
                               column(12, includeMarkdown("info_rpubs.md"))))
                )
            )
        }
        
        else if (user == 2017){
            navbarPage(
                
                
                br(),
                
                tabPanel("Introducción a la Encuesta",
                         fluidRow(column(9, includeMarkdown("about_intro.md")))),
                
                tabPanel("Despliegue total de la base de datos Casen 2017",
                         fluidRow(column(3, includeMarkdown("info_2006_tabla.md")),
                                  column(12, dataTableOutput('table_2015')))),
                
                tabPanel("Variables de identificación",
                         fluidRow(column(9, includeMarkdown("about_varia_intro.md")))),

                
                navbarMenu("Factores de expansión",
                           tabPanel("Introducción", fluidRow(
                               
                               
                               
                               column(12, includeMarkdown("facintro.md")))),
                           "----",
                           "",
                           
                           tabPanel("Factor de Expansión Comunal",
                                    fluidRow(column(9, includeMarkdown("facco.md")))),
                           
                           tabPanel("Factor de Expansión Regional",
                                    fluidRow(column(9, includeMarkdown("facor.txt")))),
                           
                           tabPanel("Factor de Expansión sobre orientación sexual e identidad de género",
                                    fluidRow(column(9, includeMarkdown("facreg.md")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Varianza ",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_varianza.txt")))),
                           "----",
                           "",
                           
                           tabPanel("Estratos de Varianza",
                                    fluidRow(column(9, includeMarkdown("varianza_estratos.txt")))),
                           
                           tabPanel("Conglomerados de Varianza",
                                    fluidRow(column(9, includeMarkdown("varianza_conglomerado.txt")))),
                           
                           tabPanel(" ")),
                
                
                
                navbarMenu("Descripción conceptual de módulos y variables",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_modulos.txt")))),
                           
                           "----",
                           "",
                           
                           ####################### Registro residentes ########################
                           
                           tabPanel("Primer módulo: Registro Residentes -columnas 1:42-", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt"))
                           )),

                           tabPanel("___Sub módulo: Identificacion de los encuestados -1:8-", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Factores de expansion -9:11-", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Varianza -12:13-", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Fecha de la entrevista -14:16-", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Personas, nucleos, hogares y parejas -17:20-", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Preguntas identificatorias -21:36-", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Dificultades o limitaciones fisicas -37:42-", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt"))
                           )),

                           ############################# Modulo educacion ######################################
                           
                        "----",
                        "",
                           tabPanel("Segundo módulo (E): Educacion -columnas 43:101-", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt"))
                           )),

                           tabPanel("___Sub módulo: Dificultades o limitaciones fisicas -43:69-", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Recepcion de beneficios estatales -70:92-", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("___Sub módulo: Pago autonomo por educacion -93:101-", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           "----",
                           "",                           
                           
                           ############################# Modulo Trabajo ######################################

                           
                           tabPanel("Tercer módulo (O): Trabajo -columnas 102:151-", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt"))
                           )),


                           
                           "----",
                           "",                           
                           
                           ############################# Modulo ingresos ######################################
                           
                           
                           tabPanel("Cuarto módulo (Y): Ingresos -columnas 152:304-", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt"))
                           )),

                           tabPanel("___Sub módulo: Alimentacion y estado nutricional -columnas 305:309-", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           
                           
                           
                           
                           
                           
                           
                           
                           
                           tabPanel("Quinto módulo (S): Salud", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                        
                        
                        
                        
                        
                        
                        
                           
                           tabPanel("Sexto módulo (R): Identidades, redes y participación", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           
                           tabPanel("Séptimo módulo (V): Vivienda y Entorno", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt"))
                           )),
                           "----",
                           "",
                           tabPanel("______Submódulo: Hogares carentes: hh_d_asis-hh_d_seg", fluidRow(
                               column(12, includeMarkdown("hh_d_asis.md"))
                           )),
                           
                           tabPanel(" ")),
                
                
                
                
                
                navbarMenu("Módulos 2017",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_modulos.txt")))),
                           "----",
                           "",
                           tabPanel("Módulo 1: Identificación", fluidRow(
                             column(12, includeMarkdown("modulo_1_2017.md")),
                             column(12, dataTableOutput("modulo_1_2017"))
                           )),
                           
                           tabPanel("Módulo 2: Expansión", fluidRow(
                             column(12, includeMarkdown("modulo_2_2017.md")),
                             column(12, dataTableOutput("modulo_2_2017"))
                           )),
                           
                           tabPanel("Módulo 3: Varianzas ", fluidRow(
                             column(12, includeMarkdown("modulo_3_2017.md")),
                             column(12, dataTableOutput("modulo_3_2017"))
                           )),
                           
                           tabPanel("Tema 1: Fecha", fluidRow(
                             column(4, includeMarkdown("modulo_4_2017.md")),
                             
                             column(12, dataTableOutput("modulo_4_2017"))
                           )),
                           
                           tabPanel("Tema 2: Totales", fluidRow(
                             column(12, includeMarkdown("modulo_5_2017.md")),
                             column(12, dataTableOutput("modulo_5_2017"))
                           )),
                           
                           tabPanel("Tema 3: Respuesta al módulo registro de residentes", fluidRow(
                             column(12, includeMarkdown("modulo_6_2017.md")),
                             column(12, dataTableOutput("modulo_6_2017"))
                           )),
                           
                           tabPanel("Tema 4: Identificacion continuacion ", fluidRow(
                             column(12, includeMarkdown("modulo_7_2017.md")),
                             column(12, dataTableOutput("modulo_7_2017"))
                           )),
                           
                           tabPanel("Tema 5: Dificultades físicas (pseudo-discapacidades) ", fluidRow(
                             column(12, includeMarkdown("modulo_13_2017.md")),
                             column(12, dataTableOutput("modulo_8_2017"))
                           )),
                           
                           tabPanel("Módulo 4: Educación", fluidRow(
                             column(12, includeMarkdown("modulo_9_2017.md")),
                             column(12, dataTableOutput("modulo_9_2017"))
                           )),
                           
                           tabPanel("Módulo 5: Empleo", fluidRow(
                             column(12, includeMarkdown("modulo_10_2017.md")),
                             column(12, dataTableOutput("modulo_10_2017"))
                           )),
                           
                           tabPanel("Módulo 6: Ingreso", fluidRow(
                             column(12, includeMarkdown("modulo_11_2017.md")),
                             column(12, dataTableOutput("modulo_11_2017"))
                           )),
                           
                           tabPanel("Módulo 7: Salud", fluidRow(
                             column(12, includeMarkdown("modulo_12_2017.md")),
                             column(12, dataTableOutput("modulo_12_2017"))
                           )),
                           
                           tabPanel("Tema 6: Discapacidad", fluidRow(
                             column(12, includeMarkdown("modulo_13_2017.md")),
                             column(12, dataTableOutput("modulo_13_2017"))
                           )),
                           
                           tabPanel("Tema 7: Quién responde al modulo salud", fluidRow(
                             column(12, includeMarkdown("modulo_14_2017.md")),
                             column(12, dataTableOutput("modulo_14_2017"))
                           )),
                           
                           tabPanel("Módulo 8: Migración", fluidRow(
                             column(12, includeMarkdown("modulo_15_2017.md")),
                             column(12, dataTableOutput("modulo_15_2017"))
                           )),
                           
                           tabPanel("Tema 8: Etnia", fluidRow(
                             column(12, includeMarkdown("modulo_16_2017.md")),
                             column(12, dataTableOutput("modulo_16_2017"))
                           )),
                           #######################################################3333
                           tabPanel("Tema 9: Redes y participación", fluidRow(
                             column(12, includeMarkdown("modulo_17_2017.md")),
                             column(12, dataTableOutput("modulo_17_2017"))
                           )),
                           
                           tabPanel("Tema 10: Preocupación por la alimentación", fluidRow(
                             column(12, includeMarkdown("modulo_18_2017.md")),
                             column(12, dataTableOutput("modulo_18_2017"))
                           )),
                           
                           tabPanel("Módulo: Discriminacion", fluidRow(
                             column(12, includeMarkdown("modulo_19_2017.md")),
                             column(12, dataTableOutput("modulo_19_2017"))
                           )),
                           
                           tabPanel("Tema 11: Auto-biografia", fluidRow(
                             column(12, includeMarkdown("modulo_20_2017.md")),
                             column(12, dataTableOutput("modulo_20_2017"))
                           )),
                           
                           tabPanel("Módulo: Residentes (Patrimonio)", fluidRow(
                             column(12, includeMarkdown("modulo_21_2017.md")),
                             column(12, dataTableOutput("modulo_21_2017"))
                           )),
                           
                           tabPanel("Módulo: Residentes (Internet)", fluidRow(
                             column(12, includeMarkdown("modulo_22_2017.md")),
                             column(12, dataTableOutput("modulo_22_2017"))
                           )),
                           
                           tabPanel("Módulo: Residentes (Telefono movil)", fluidRow(
                             column(12, includeMarkdown("modulo_23_2017.md")),
                             column(12, dataTableOutput("modulo_23_2017"))
                           )),
                           
                           tabPanel("Tema 12: Orientacion sexual", fluidRow(
                             column(12, includeMarkdown("modulo_24_2017.md")),
                             column(12, dataTableOutput("modulo_24_2017"))
                           )),
                           
                           tabPanel("Tema 13: Respuesta a módulo: Redes y participacion", fluidRow(
                             column(12, includeMarkdown("modulo_25_2017.md")),
                             column(12, dataTableOutput("modulo_25_2017"))
                           )),
                           
                           tabPanel("Moódulo: Vivienda", fluidRow(
                             column(12, includeMarkdown("modulo_26_2017.md")),
                             column(12, dataTableOutput("modulo_26_2017"))
                           )),
                           
                           tabPanel("Módulo Ingresos II", fluidRow(
                             column(12, includeMarkdown("modulo_27_2017.md")),
                             column(12, dataTableOutput("modulo_27_2017"))
                           )),
                           
                           tabPanel(" Módulo: Pobreza, quintiles y deciles", fluidRow(
                             column(12, includeMarkdown("modulo_28_2017.md")),
                             column(12, dataTableOutput("modulo_28_2017"))
                           )),
                           
                           tabPanel("Tema 14: Numero de personas en el hogar", fluidRow(
                             column(12, includeMarkdown("modulo_29_2017.md")),
                             column(12, dataTableOutput("modulo_29_2017"))
                           )),
                           
                           tabPanel("Módulo: Variables creadas (Educacion)", fluidRow(
                             column(12, includeMarkdown("modulo_30_2017.md")),
                             column(12, dataTableOutput("modulo_30_2017"))
                           )),
                           
                           tabPanel("Módulo: Variables creadas (Condicion de actividad)", fluidRow(
                             column(12, includeMarkdown("modulo_31_2017.md")),
                             column(12, dataTableOutput("modulo_31_2017"))
                           )),
                           
                           tabPanel("Módulo: Variables creadas (Indicadores de calidad de la vivienda)", fluidRow(
                             column(12, includeMarkdown("modulo_32_2017.md")),
                             column(12, dataTableOutput("modulo_32_2017"))
                           )),
                           
                           tabPanel("Módulo: Variables creadas (Indicadores de allegamiento)", fluidRow(
                             column(12, includeMarkdown("modulo_33_2017.md")),
                             column(12, dataTableOutput("modulo_33_2017"))
                           )),
                           
                           tabPanel("Módulo: Variables creadas (Hacinamiento)", fluidRow(
                             column(12, includeMarkdown("modulo_34_2017.md")),
                             column(12, dataTableOutput("modulo_34_2017"))
                           )),
                           
                           tabPanel("Módulo: Variables creadas (Carencias)", fluidRow(
                             column(12, includeMarkdown("modulo_35_2017.md")),
                             column(12, dataTableOutput("modulo_35_2017"))
                           )),
                           
                           tabPanel("Módulo: Variables creadas (Pobreza)", fluidRow(
                             column(12, includeMarkdown("modulo_36_2017.md")),
                             column(12, dataTableOutput("modulo_36_2017"))
                           )),
                           
                           
                           
                           # tabPanel("Módulo 11: Vivienda", fluidRow(
                           #   column(12, includeMarkdown("modulo_17_2013.md")),
                           #   column(12, dataTableOutput("modulo_17_2017"))
                           # )),
                           # 
                           # tabPanel("Tema 9: Redes y participacion", fluidRow(
                           #   column(12, includeMarkdown("modulo_18_2013.md")),
                           #   column(12, dataTableOutput("modulo_18_2017"))
                           # )),
                           # 
                           # tabPanel("Tema 10: Preocupacion por la alimentacion", fluidRow(
                           #   column(12, includeMarkdown("modulo_19_2013.md")),
                           #   column(12, dataTableOutput("modulo_19_2017"))
                           # )),
                           # 
                           # tabPanel("Módulo: Discriminacion", fluidRow(
                           #   column(12, includeMarkdown("modulo_20_2013.md")),
                           #   column(12, dataTableOutput("modulo_20_2017"))
                           # )),
                           # 
                           # tabPanel("Tema 11: Auto-biografia", fluidRow(# desde r11
                           #   column(12, includeMarkdown("modulo_21_2013.md")),
                           #   column(12, dataTableOutput("modulo_21_2017"))
                           # )),
                           # 
                           # tabPanel("Módulo: Residentes(Patrimonio)", fluidRow(
                           #   column(12, includeMarkdown("modulo_22_2013.md")),
                           #   column(12, dataTableOutput("modulo_22_2017"))
                           # )),
                           # 
                           # tabPanel("Módulo: Residentes(Internet)", fluidRow(
                           #   column(12, includeMarkdown("modulo_23_2013.md")),
                           #   column(12, dataTableOutput("modulo_23_2017"))
                           # )),
                           # 
                           # 
                           # tabPanel("Módulo: Residentes(Telefono movil)", fluidRow(
                           #   column(12, includeMarkdown("modulo_24_2013.md")),
                           #   column(12, dataTableOutput("modulo_24_2017"))
                           # )),
                           # 
                           # 
                           # tabPanel("Tema 12: Orientacion sexual", fluidRow(
                           #   column(12, includeMarkdown("modulo_25_2013.md")),
                           #   column(12, dataTableOutput("modulo_25_2017"))
                           # )),
                           # 
                           # 
                           # tabPanel("Tema 13: Respuesta a módulo: Redes y participacion", fluidRow(
                           #   column(12, includeMarkdown("modulo_26_2013.md")),
                           #   column(12, dataTableOutput("modulo_26_2017"))
                           # )),
                           # 
                           # 
                           # tabPanel("Moódulo: Vivienda", fluidRow(
                           #   column(12, includeMarkdown("modulo_27_2013.md")),
                           #   column(12, dataTableOutput("modulo_27_2017"))
                           # )),
                           # 
                           # tabPanel("Módulo Ingresos II", fluidRow(
                           #   column(12, includeMarkdown("modulo_28_2013.md")),
                           #   column(12, dataTableOutput("modulo_28_2017"))
                           # )),
                           # 
                           # tabPanel("Módulo: Pobreza, quintiles y deciles", fluidRow(
                           #   column(12, includeMarkdown("modulo_29_2013.md")),
                           #   column(12, dataTableOutput("modulo_29_2017"))
                           # )),
                           # 
                           # tabPanel("Tema 14: Numero de personas en el hogar", fluidRow(
                           #   column(12, includeMarkdown("modulo_30_2013.md")),
                           #   column(12, dataTableOutput("modulo_30_2017"))
                           # )),
                           # 
                           # tabPanel("Módulo: Variables creadas(Educacion)", fluidRow(
                           #   column(12, includeMarkdown("modulo_31_2013.md")),
                           #   column(12, dataTableOutput("modulo_31_2017"))
                           # )),
                           # 
                           # tabPanel("ódulo: Variables creadas(Condicion de actividad)", fluidRow(
                           #   column(12, includeMarkdown("modulo_31_2013.md")),
                           #   column(12, dataTableOutput("modulo_32_2017"))
                           # )),
                           # 
                           # tabPanel("Módulo: Variables creadas(Indicadores de calidad de la vivienda)", fluidRow(
                           #   column(12, includeMarkdown("modulo_31_2013.md")),
                           #   column(12, dataTableOutput("modulo_33_2017"))
                           # )),
                           # 
                           # tabPanel("Módulo: Variables creadas(Indicadores de allegamiento)", fluidRow(
                           #   column(12, includeMarkdown("modulo_31_2013.md")),
                           #   column(12, dataTableOutput("modulo_34_2017"))
                           # )),
                           # 
                           # tabPanel("Módulo: Variables creadas(Hacinamiento)", fluidRow(
                           #   column(12, includeMarkdown("modulo_31_2013.md")),
                           #   column(12, dataTableOutput("modulo_35_2017"))
                           # )),
                           # 
                           # tabPanel("Módulo: Variables creadas(Carencias)", fluidRow(
                           #   column(12, includeMarkdown("modulo_31_2013.md")),
                           #   column(12, dataTableOutput("modulo_36_2017"))
                           # )),
                           # 
                           # 
                           # 
                           # 
                           # 
                           tabPanel(" ")),
                
                navbarMenu("Variables e indicadores de pobreza",
                           tabPanel("Introducción",
                                    fluidRow(column(9, includeMarkdown("about_veip.md")))),
                           "----",
                           
                           tabPanel("Pobreza por ingresos",
                                    fluidRow(column(9, includeMarkdown("about_pobporin_vei.txt")),
                                             column(3,  tableOutput("contents12")))),
                           
                           tabPanel("Pobreza multidimensional",
                                    tabsetPanel(
                                        tabPanel("Redes y cohesión social", fluidRow(column(9, includeMarkdown("pmredes.md")))),
                                        tabPanel("Educación",  fluidRow(column(9, includeMarkdown("pm_educacion.txt")))),
                                        tabPanel("Salud", fluidRow(column(9, includeMarkdown("pm_salud.txt")))),
                                        tabPanel("Trabajo y seguridad social", fluidRow(column(9, includeMarkdown("pm_ts.txt")))),
                                        tabPanel("Vivienda y entorno", fluidRow(column(9, includeMarkdown("pm_vivienda.txt"))))
                                    )),
                           
                           
                           tabPanel("Pobreza multidimensional 4d y 5d",
                                    fluidRow(column(9, includeMarkdown("about_pobmul4d5d_vei.txt")),
                                             column(3,  tableOutput("contents14")))),
                           
                           tabPanel(" ")),
                
                navbarMenu("Indicadores",
                           tabPanel("Introducción",
                                    fluidRow(column(9, includeMarkdown("about_intro_mds.txt")))),
                           "----",
                           "",
                           
                           tabPanel("Variables",
                                    tabsetPanel(
                                        tabPanel("numper", fluidRow(column(9, includeMarkdown("var_numper.txt")))),
                                        tabPanel("asiste",  fluidRow(column(9, includeMarkdown("var_asiste.txt")))),
                                        tabPanel("esc", fluidRow(column(9, includeMarkdown("var_esc.txt")))),
                                        tabPanel("educ", fluidRow(column(9, includeMarkdown("var_educ.txt")))),
                                        tabPanel("depen", fluidRow(column(9, includeMarkdown("var_depen.txt")))),
                                        tabPanel("activ", fluidRow(column(9, includeMarkdown("var_activ.txt")))),
                                        tabPanel("indmat", fluidRow(column(9, includeMarkdown("var_indmat.txt")))),
                                        tabPanel("indsan", fluidRow(column(9, includeMarkdown("var_indsan.txt")))),
                                        tabPanel("calglobviv", fluidRow(column(9, includeMarkdown("var_calglobviv.txt")))),
                                        tabPanel("iae", fluidRow(column(9, includeMarkdown("var_iae.txt")))),
                                        tabPanel("iai", fluidRow(column(9, includeMarkdown("var_iai.txt")))),
                                        tabPanel("hacinamiento", fluidRow(column(9, includeMarkdown("var_hacinamiento.txt"))))
                                        
                                    )),
                           
                           tabPanel(" ")),
                
                navbarMenu("Frecuencias de respuestas por campo",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),

                           tabPanel("Frecuencias por módulo I: Registro de residentes.", fluidRow(
                               column(12, includeMarkdown("info_2006_frec.md")),
                               
                               # debe cargarse el ddl con las cabeceras del rango I:
                               selectInput("ptabla", "Seleccione pregunta:", c(data_2017_modulo_I_colnames)),
                               column(12, dataTableOutput("prueba_tabla"))
                           )),  
                           
                           tabPanel("Frecuencias por módulo II: Registro de residentes.", fluidRow(
                             column(12, includeMarkdown("info_2006_frec.md")),
                             selectInput("ptabla2", "Seleccione pregunta:", c(data_2017_modulo_II_colnames)),
                             column(12, dataTableOutput("prueba_tabla2"))
                           )),
                           
                           tabPanel("Frecuencias por módulo III: Registro de residentes.", fluidRow(
                             column(12, includeMarkdown("info_2006_frec.md")),
                             selectInput("ptabla3", "Seleccione pregunta:", c(data_2017_modulo_III_colnames)),
                             column(12, dataTableOutput("prueba_tabla3"))
                           )),
                           
                           tabPanel("Frecuencias por módulo IV: Registro de residentes.", fluidRow(
                             column(12, includeMarkdown("info_2006_frec.md")),
                             selectInput("ptabla4", "Seleccione pregunta:", c(data_2017_modulo_IV_colnames)),
                             column(12, dataTableOutput("prueba_tabla4"))
                           )),
                           
                           

                           tabPanel("Frecuencias por módulo III: Registro de residentes.", tableOutput("modulo_III_2017")),
                           tabPanel("Frecuencias por módulo IV: Registro de residentes.", tableOutput("modulo_IV_2017")),
                           tabPanel("Frecuencias por módulo V: Registro de residentes.", tableOutput("modulo_V_2017")),
                           tabPanel("Frecuencias por módulo VI: Registro de residentes.", tableOutput("modulo_VI_2017")),
                           tabPanel("Frecuencias por módulo VII: Registro de residentes.", tableOutput("modulo_VII_2017")),
                           tabPanel("Frecuencias por módulo VIII: Registro de residentes.", tableOutput("modulo_VIII_2017")),
                        
                           tabPanel("Tabla trabajo", tableOutput("contents_trabajo")),
                           "----",
                           "",
                           tabPanel("Diagramas de Caja y bigotes y de Densidad para la variable Edad", plotOutput("plot1")),
                           tabPanel("  ")),
                
                
                
                navbarMenu("Estadísticas y gráficas",
                           tabPanel("Promedios", fluidRow(
                             column(12, includeMarkdown("info_2006_prom.md")),
                             selectInput("ptabla_promedios_2015", "Seleccione variable:", c(dataset2015_col)),
                             column(12, verbatimTextOutput("promedios_2015"))
                           )),
                           
                           
                           
                           tabPanel("Diagrama de caja y bigotes", fluidRow(
                             
                             column(12, includeMarkdown("info_2006_cyb.md")),
                             
                             selectInput("ptabla_cyb_2015", "Seleccione la variable:", c(dataset2015_col)),
                             
                             downloadButton("plot_cyb_2015", "Descargar"),
                             
                             column(12, plotOutput("cyb_2015"))
                           ))
                ),
                
                
                
                
                navbarMenu("Tablas de contingencia",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           tabPanel("Tablas de contingencia de 2x2",fluidRow(
                                 column(12, includeMarkdown("info_ponderacion.md")),
                               
                               column(12,
                                                                                    selectInput("ptabla2017_primerav", "ingrese primera variable:", c(datos_df_exp)),
                                                                                    selectInput("ptabla2017_segundav", "ingrese segunda variable:", c(datos_df_exp)),
                                                                                    verbatimTextOutput("tabla_d_c"),
                                                                                    verbatimTextOutput("tabla_d_c_ponderadas")
                                                                                    ))
                                    ),

                           
                           tabPanel("Pearson's Chi-squared test",fluidRow(
                               column(12, includeMarkdown("info_Chi-squared.md")),
                               column(12,
                                                                                 selectInput("ptabla2017_primerav_chi", "ingrese primera variable:", c(datos_df_exp)),
                                                                                 selectInput("ptabla2017_segundav_chi", "ingrese segunda variable:", c(datos_df_exp)),
                                                                                 verbatimTextOutput("tabla_chi"))))
                ) ,
                
                
                
                
                
                
                
                
                
                
                
                navbarMenu("Tablas de contingencia > 2x2",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           tabPanel("Tablas de contingencia > 2x2",fluidRow(
                               selectInput("nada", "Identifique la variable:", c(data_2017_colnames)),
                               column(7,
                                                                                   selectInput("ptabla2017_primeravx", "ingrese primera variable:", c(datos_df_exp)),
                                                                                   selectInput("ptabla2017_segundavx", "ingrese segunda variable:", c(datos_df_exp)),
                                                                                   selectInput("ptabla2017_terceravx", "ingrese tercera variable:", c(datos_df_exp)),
                                                                                   selectInput("ptabla2017_cuartavx", "ingrese cuarta variable:", c(datos_df_exp)),
                                                                                   downloadButton("boton_ttcc_mayor_2", "Descargar"),
                                                                                   verbatimTextOutput("tabla_d_c_generalizada") %>% withSpinner(color="#0dc5c1"),
                                   
                                   downloadButton("boton_ttcc_mayor_2_pon", "Descargar"),
                                   #   tableOutput("tabla_d_c_generalizada") %>% withSpinner(color="#0dc5c1")))),
                                   verbatimTextOutput("tabla_d_c_generalizada_pon") %>% withSpinner(color="#0dc5c1")
                                   )
                               )),

                           tabPanel("Cochran–Mantel–Haenszel",fluidRow(column(12,
                                                                              selectInput("ptabla2017_primerav", "ingrese primera variable:", c(datos_df_exp)),
                                                                              selectInput("ptabla2017_segundav", "ingrese segunda variable:", c(datos_df_exp)),
                                                                              selectInput("ptabla2017_tercerav", "ingrese tercera variable:", c(datos_df_exp)),
                                                                              verbatimTextOutput("tabla_chi_generalizada"))))
                ),
                
                
                
                
                
                
                
                
                navbarMenu("Tablas de contingencia > 2x2 para promedios",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           tabPanel("Tablas de contingencia > 2x2",fluidRow(column(7,
                                                                                   selectInput("ptabla2017_primeravx_prom", "ingrese primera variable:", c(datos_df_exp)),
                                                                                   selectInput("ptabla2017_segundavx_prom", "ingrese segunda variable:", c(datos_df_exp)),
                                                                                   selectInput("ptabla2017_terceravx_prom", "ingrese tercera variable:", c(datos_df_exp)),
                                                                                   
                                                                                   selectInput("ptabla2017_cuartavx", "ingrese cuarta variable:", c(datos_df_exp)),
                                                                                   
                                                                                   downloadButton("boton_ttcc_mayor_2_prom", "Descargar"),
                                                                                   
                                                                                   verbatimTextOutput("tabla_d_c_generalizada_prom")))),
                           
                           tabPanel("Cochran–Mantel–Haenszel",fluidRow(column(12,
                                                                              selectInput("ptabla2017_primerav_prom", "ingrese primera variable:", c(datos_df_exp)),
                                                                              selectInput("ptabla2017_segundav_prom", "ingrese segunda variable:", c(datos_df_exp)),
                                                                              selectInput("ptabla2017_tercerav_prom", "ingrese tercera variable:", c(datos_df_exp)),
                                                                              verbatimTextOutput("tabla_chi_generalizada_prom"))))
                ),
                
                navbarMenu("Diccionario de variables",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           
                           tabPanel("exp",fluidRow(
                               column(12, includeMarkdown("info_rpubs.md")),
                               
                               column(12 )))
                ),
                
                navbarMenu("Filtros agrupados por categoría",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           tabPanel("Promedios agrupados por categoría",fluidRow(column(12, includeMarkdown("info_papc.md")),
                                                                                 column(12,
                                                                                   selectInput("primero_papc_2017", "ingrese primera variable:", c(datos_df_exp)),
                                                                                   selectInput("segundo_papc_2017", "ingrese segunda variable:", c(datos_df_exp)),

                                                                                   downloadButton("boton_tabla_papc_2017", "Descargar"),
                                                                                   
                                                                                   tableOutput("tabla_papc_2017"))))
                ),
                
                navbarMenu("Análisis de series en el tiempo",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           
                           tabPanel("exp",fluidRow(
                               column(12, includeMarkdown("info_rpubs.md")),
                               
                               column(12 )))
                ),
                
                navbarMenu("Análisis de algunas tablas de contingencia",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),


                           tabPanel("exp",fluidRow(
                               column(12, includeMarkdown("info_rpubs.md")),
                               
                               column(12 )))
                )
                
            )
        }
        
        
        
        
    })
    
    ####################### descargas de ttcc hacia csv: ##################################
    
    ################ 2006 #################
    
    
    ##############################
    
    output$boton_ttcc_mayor_2 <- downloadHandler(
        filename = function() {
            paste("tabla_ttcc.csv", "csv", sep=".")
        },
        content = function(file) {
            d <- input$ptabla2017_primeravx
            e <- input$ptabla2017_segundavx
            f <- input$ptabla2017_terceravx
            g <- input$ptabla2017_cuartavx

            preguntaseternas2001_ab <- mydata_2017_1()
            
            primera_variable <- preguntaseternas2001_ab[,d]
            segunda_variable <- preguntaseternas2001_ab[,e] 
            tercera_variable <- preguntaseternas2001_ab[,f] 
            cuarta_variable <- preguntaseternas2001_ab[,g] 
            cross_tab = table(primera_variable, segunda_variable, tercera_variable, cuarta_variable)
            write.csv(cross_tab, file)
        }
    )
    
    output$boton_ttcc_mayor_2_pon <- downloadHandler(
        filename = function() {
            paste("tabla_ttcc_pon.csv", "csv", sep=".")
        },
        content = function(file) {
            d <- input$ptabla2017_primeravx
            e <- input$ptabla2017_segundavx
            f <- input$ptabla2017_terceravx
            g <- input$ptabla2017_cuartavx
            
            preguntaseternas2001_ab_pon <- mydata_2017_1()
            
            primera_variable <- preguntaseternas2001_ab_pon[,d]
            segunda_variable <- preguntaseternas2001_ab_pon[,e] 
            tercera_variable <- preguntaseternas2001_ab_pon[,f] 
            cuarta_variable <- preguntaseternas2001_ab_pon[,g] 
            
            #       cross_tab = xtabs(preguntaseternas2001_ab$expc ~ unlist(preguntaseternas_sub2001_a) + unlist(preguntaseternas_sub2001_b)+unlist(preguntaseternas_sub2001_c)+unlist(preguntaseternas_sub2001_d),aggregate(preguntaseternas2001_ab$expc ~ unlist(preguntaseternas_sub2001_a)+unlist(preguntaseternas_sub2001_b)+unlist(preguntaseternas_sub2001_c)+unlist(preguntaseternas_sub2001_d),preguntaseternas2001_ab,mean))
            cross_tab_pon = table(preguntaseternas2001_ab$expc ~ unlist(primera_variable)+unlist(segunda_variable)+unlist(tercera_variable)+unlist(cuarta_variable))
            write.csv(cross_tab_pon, file)
            
        }
    )
    
    
    
    
  ########################################################
    
    output$boton_ttcc_2013 <- downloadHandler(
      filename = function() {
        paste("tabla_ttcc.csv", "csv", sep=".")
      },
      content = function(file) {
        
        
        a <- input$ptabla2013_primerav
        b <- input$ptabla2013_segundav
        
        
        #dataset2013_react
        preguntaseternas2001_ab <- dataset2013_react()
        preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,a]
        preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,b] 
        cross_tab = xtabs(~ unlist(preguntaseternas_sub2001_a) + unlist(preguntaseternas_sub2001_b), preguntaseternas2001_ab)
        write.csv(cross_tab, file)
        
      }
    )
    
    
    output$boton_ponderadas_2013 <- downloadHandler(
      filename = function() {
        paste("tabla_ponderada.csv", "csv", sep=".")
      },
      content = function(file) {
        
        a <- input$ptabla2013_primerav
        b <- input$ptabla2013_segundav
        
        
        #dataset2013_react
        preguntaseternas2001_ab <- dataset2013_react()
        preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,a]
        preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,b] 
        cross_tab = xtabs(expc ~ unlist(preguntaseternas_sub2001_a) + unlist(preguntaseternas_sub2001_b), preguntaseternas2001_ab)
        
        write.csv(cross_tab, file)
        
      }
    )
    
    

    
    
    
    
    
    #################################################################
    #################################################################
    ###########################   ahora!   ##########################
    #################################################################
    #################################################################
    
    
    

    
    output$boton_tabla_papc_2017 <- downloadHandler(
        filename = function() {
            paste("tabla", "csv", sep=".")
        },
        content = function(file) {
            
            a <- input$primero_papc_2017
            b <- input$segundo_papc_2017
            
            base_del_2006 <- mydata_2017_1()
            
            base_del_2006_terr <- base_del_2006[,b]
            
            base_del_2006_terr[is.na(base_del_2006_terr)] <- 0
            
            base_del_2006_cat <- base_del_2006[,a]
            
            promedios <- aggregate(list(base_del_2006_terr), list(base_del_2006_cat), mean)
            

            write.csv(promedios, file)
            

        }
    )
    

    
    output$tabla_papc_2017<-renderTable({
        
        a <- input$primero_papc_2017
        b <- input$segundo_papc_2017
        
        base_del_2006 <- mydata_2017_1()
        
        base_del_2006_terr <- base_del_2006[,b]
        
        base_del_2006_terr[is.na(base_del_2006_terr)] <- 0
        
        base_del_2006_cat <- base_del_2006[,a]
        
        promedios <- aggregate(list(base_del_2006_terr), list(base_del_2006_cat), mean)
    })
    #################################################
    
    output$promedios_filtros_mn<-renderTable({
      
      a <- input$nivel_filtro_mn
      b <- input$categoria_filtro_mn
      
      base_del_2009 <- mydata_educacion_3000()
      
      
      
      
      base_del_2009_terr <- base_del_2009[,b]
      
      base_del_2009_terr[is.na(base_del_2009_terr)] <- 0
      
      
      
      base_del_2009_cat <- base_del_2009[,a]
      
      promedios <- data.frame(aggregate(base_del_2009_terr, base_del_2009_cat, mean))
      
      return((promedios))
      
    })
    
    #################################################
    
    output$promedios_filtros_11mn<-renderTable({
      
      a <- input$nivel_filtro_11mn
      b <- input$categoria_filtro_11mn
      
      base_del_2011 <- mydata_educacion_3000()
      
      
      
      
      base_del_2011_terr <- base_del_2011[,b]
      
      base_del_2011_terr[is.na(base_del_2011_terr)] <- 0
      
      
      
      base_del_2011_cat <- base_del_2011[,a]
      
      promedios <- data.frame(aggregate(base_del_2009_terr, base_del_2009_cat, mean))
      
      return((promedios))
      
    })
    
    ################################   2017  #######################
    
    #### base de datos total:
    
    mydata_educacion_exp <- reactive({
        data <- dataset2017[, 1:804]
        return(data)
    })    
    
    ################################   2017  #######################
    
    
    
    # mydata_educacion_exp2 <- reactive({
    #     data <- alerta
    #     return(data)
    # })
    # 
    # 
    # mydata_2017_1 <- reactive({
    #     data <- dataset
    #     return(data)
    # })
    # 
    
    
    # mydata_educacion_1000 <- reactive({
    #     datos_dfe <- datos_df_1000[, 1:32]
    #     datos_dfe
    #     return(datos_dfe)
    # })
    
    
    
    
    
    
    mydata_educacion_2000 <- reactive({
        datos_dfe <- datos_df_2000[, 1:5]
        datos_dfe 
        return(datos_dfe)
    })
    
    mydata_educacion_3000 <- reactive({
        datos_dfe <- datos_df_casen_2009_mil_mn[, 1:34]
        datos_dfe 
        return(datos_dfe)
    })
    
    mydata_educacion_4000 <- reactive({
        datos_dfe <- datos_df_casen_2011_mil_mn[, 1:34]
        datos_dfe 
        return(datos_dfe)
    })
    
    mydata_educacion_5000 <- reactive({
        datos_dfe <- datos_df_casen_2011_mil_ymt[, 1:24]
        datos_dfe 
        return(datos_dfe)
    })
    
    mydata_educacion_6000 <- reactive({
      datos_dfe <- datos_df_casen_2013_mil[, 1:600]
      datos_dfe 
      return(datos_dfe)
    })
    
    
    ###############################################   2006 #################################
    
    # El objeto sin subindice representa a la totalidad:
    dataset2006_react <- reactive({
        data <- dataset2006
        return(data)
    })
    
    dataset2006_react_1 <- reactive({
        data <- dataset2006[, 1:9] #registro de personas
        return(data)
    })
    
    dataset2006_react_2 <- reactive({
        data <- dataset2006[, 10:12]#posesiones y comunicacion
        return(data)
    })
    
    
    dataset2006_react_3 <- reactive({
        data <- dataset2006[, 13:45]#vias de comunicacion
        return(data)
    })
    
    
    dataset2006_react_4 <- reactive({
        data <- dataset2006[, 46:81]#educacion
        return(data)
    })
    
    
    dataset2006_react_5 <- reactive({
        data <- dataset2006[, 82:114]#trabajo
        return(data)
    })
    
    
    dataset2006_react_6 <- reactive({
        data <- dataset2006[, 115:165]#ingresos
        return(data)
    })
    
    
    dataset2006_react_7 <- reactive({
        data <- dataset2006[, 166:221]#salud
        return(data)
    })
    
    dataset2006_react_8 <- reactive({
        data <- dataset2006[, 222:227]#discapacidad
        return(data)
    })
    
    dataset2006_react_9 <- reactive({


       data <- dataset2006[, 228:229]
       data <- data.frame(data$t3)
        return(data)
    })
    
    dataset2006_react_10 <- reactive({
        data <- dataset2006[, 229:232]#etnia
        return(data)
    })
    
    dataset2006_react_11 <- reactive({
        data <- dataset2006[, 233:237]#residencia
        return(data)
    })
    
    dataset2006_react_12 <- reactive({
      data <- dataset2006[, 238:243]#cultura
      return(data)
    })
    
    dataset2006_react_13 <- reactive({
      data <- dataset2006[, 244:255]#situacion pasada
      return(data)
    })
    
    dataset2006_react_14 <- reactive({
      data <- dataset2006[, 256:312]#vivienda
      return(data)
    })
    
    dataset2006_react_15 <- reactive({
      data <- dataset2006[, 313:315]#actividades en escolaridad y trabajo
      return(data)
    })
    
    dataset2006_react_16 <- reactive({
      data <- dataset2006[, 316:318]#subsidios
      return(data)
    })
    
    dataset2006_react_17 <- reactive({
      data <- dataset2006[, 319:322]#pobreza
      return(data)
    })
    
    dataset2006_react_18 <- reactive({
      data <- dataset2006[, 323:335]#pobreza
      return(data)
    })
    
    dataset2006_react_19 <- reactive({
      data <- dataset2006[, 336:341]#pobreza
      return(data)
    })
    
    dataset2006_react_20 <- reactive({
      data <- dataset2006[, 342:348]#pobreza
      return(data)
    })
    
    ###############################################   2009 #################################
    
    # El objeto sin subindice representa a la totalidad:
    dataset2009_react <- reactive({
        data <- dataset2009
        return(data)
    })
    
    dataset2009_react_1 <- reactive({
        data <- dataset2009[, 1:10]#IDENTIFICACIÓN
        return(data)
    })
    
    dataset2009_react_2 <- reactive({
      data <- dataset2009[, 11:16]#MÓDULO Expancion
      
      return(data)
    })
    
    dataset2009_react_3 <- reactive({
      data <- dataset2009[, 17:24]#MÓDULO RESIDENTES
      
      return(data)
    })
    
    dataset2009_react_31 <- reactive({
      data <- dataset2009[, 25:27]#Residentes: situacion en la vivienda
      
      return(data)
    })
    dataset2009_react_32 <- reactive({
      data <- dataset2009[, 28:38]#Residentes: Patrimonio
      
      return(data)
    })
    dataset2009_react_33 <- reactive({
      data <- dataset2009[, 39:41]#Residentes: Lugar de uso tecnologias
      
      return(data)
    })
    dataset2009_react_34 <- reactive({
      data <- dataset2009[, 42:50]#Residentes: Lugar de uso del internet
      
      return(data)
    })
    
    
    # dataset2009_react_3 <- reactive({
    #     data <- dataset2009[, 25:50]#trabajo
    #     return(data)
    # })
    
    
    dataset2009_react_4 <- reactive({
        data <- dataset2009[, 51:87]#Educacion
        return(data)
    })
    
    
    dataset2009_react_5 <- reactive({
        data <- dataset2009[, 88:120]#salud
        return(data)
    })
    
    
    dataset2009_react_6 <- reactive({
        data <- dataset2009[, 121:136]#salud
        return(data)
    })
    
    
    dataset2009_react_7 <- reactive({
        data <- dataset2009[, 137:201]#
        return(data)
    })
    
    dataset2009_react_8 <- reactive({
        data <- dataset2009[, 202:212]#discapacidad
        return(data)
    })
    
    dataset2009_react_9 <- reactive({
        data <- dataset2009[, 213:214]#Expansiones
        return(data)
    })
    #VARIABLES CREADAS A PARTIR DE LA ENCUESTA
    
    dataset2009_react_10 <- reactive({
        data <- dataset2009[, 214:216]#
        return(data)
    })
    
    dataset2009_react_11 <- reactive({
      data <- dataset2009[, 217:220]#Informacion
      return(data)
    })
    
    dataset2009_react_12a <- reactive({
      data <- dataset2009[, 221:223]#Informacion
      return(data)
    })
    
    dataset2009_react_12 <- reactive({
      data <- dataset2009[, 308:350]#Indicadores
      return(data)
    })
    
    dataset2009_react_13 <- reactive({
      data <- dataset2009[, 224:236]#Indicadores
      return(data)
    })
    
    
    dataset2009_react_14a <- reactive({
      data <- dataset2009[, 237:245]#Indicadores
      return(data)
    })
    
    dataset2009_react_14 <- reactive({
      data <- dataset2009[, 246:296]#Indicadores
      return(data)
    })
    
    
    
    dataset2009_react_15 <- reactive({
      data <- dataset2009[, 297:299]#Educacion2
      return(data)
    })
    dataset2009_react_16 <- reactive({
      data <- dataset2009[, 300:302]#Indicadores
      return(data)
    })
    dataset2009_react_17 <- reactive({
      data <- dataset2009[, 303:307]#Indicadores
      return(data)
    })
    dataset2009_react_18 <- reactive({
      data <- dataset2009[, 308:311]#Indicadores
      return(data)
    })
    dataset2009_react_19 <- reactive({
      data <- dataset2009[, 312:341]#Indicadores
      return(data)
    })
    dataset2009_react_20 <- reactive({
      data <- dataset2009[, 342:350]#Indicadores
      return(data)
    })
    dataset2009_react_21 <- reactive({
      data <- dataset2009[, 351:357]#Indicadores
      return(data)
    })
    ###############################################   2011 #################################
    
    #la base de datos total:
    dataset2011_react <- reactive({
        data <- dataset2011
        return(data)
    })
    
    dataset2011_react_1 <- reactive({
      data <- dataset2011[, 1:7]
      return(data)
    })
    
    dataset2011_react_1b <- reactive({
      data <- dataset2011[, 8:9]
      return(data)
    })
    
    dataset2011_react_2 <- reactive({
        data <- dataset2011[, 10:13]
        return(data)
    })
    
    
    dataset2011_react_3 <- reactive({
        data <- dataset2011[, 14:27]
        return(data)
    })
    
    
    dataset2011_react_4 <- reactive({
        data <- dataset2011[, 28:77]
        return(data)
    })
    
    
    dataset2011_react_5 <- reactive({
        data <- dataset2011[, 78:109]
        return(data)
    })
    
    # Chile Solidario
    dataset2011_react_7 <- reactive({
        data <- dataset2011[, 110:111]
        return(data)
    })
    
    
    # Ingresos:
    # dataset2011_react_6 <- reactive({
    #     data <- dataset2011[, 111:127]
    #     return(data)
    # })
    # 
    

    
    dataset2011_react_8 <- reactive({
        data <- dataset2011[, 111:127] #ingresos
        return(data)
    })
    
    # salud
    dataset2011_react_9 <- reactive({
        data <- dataset2011[, 128:190]
        return(data)
    })
    
    
    ### ahora ###########
    
    #tema discapacidad
    dataset2011_react_10 <- reactive({
        data <- dataset2011[, 191:209]
        return(data)
    })

    # quien responde al modulo salud
    dataset2011_react_11 <- reactive({
        data <- dataset2011[, 210:211]
        return(data)
    })
    
    # migracion:
    dataset2011_react_12 <- reactive({
        data <- dataset2011[, 211:217]
        return(data)
    })
    
    ### ahora ###########   
    

    # 
    # dataset2011_react_13 <- reactive({
    #   data <- dataset2011[, 1:27]
    #   return(data)
    # })
    # 
    # dataset2011_react_14 <- reactive({
    #   data <- dataset2011[, 28:75]
    #   return(data)
    # })
    # 
    # 
    # dataset2011_react_15 <- reactive({
    #   data <- dataset2011[, 76:118]
    #   return(data)
    # })
    # 
    # 
    # dataset2011_react_16 <- reactive({
    #   data <- dataset2011[, 119:145]
    #   return(data)
    # })
    
    # autobiografia
    dataset2011_react_17 <- reactive({
      data <- dataset2011[, 218:222]
      return(data)
    })
    
    # Entia
    dataset2011_react_18 <- reactive({
      data <- dataset2011[, 223:225]
      return(data)
    })
    
    # participacion
    dataset2011_react_19 <- reactive({
      data <- dataset2011[, 226:227]
      return(data)
    })
    
    
    
    # Residentes: patrimonio:
    dataset2011_react_20 <- reactive({
      data <- dataset2011[, 227:236]
      return(data)
    })
    
    ### ahora: 
    
    # tema internet
    dataset2011_react_21 <- reactive({
      data <- dataset2011[, 237:252]
      return(data)
    })
    
    # telefono movil
    dataset2011_react_22 <- reactive({
      data <- dataset2011[, 253:254]
      return(data)
    })
    
    # tema satisfaccion con la vida
    dataset2011_react_23 <- reactive({
      data <- dataset2011[, 254:255]
      return(data)
    })
    
    # tema quien responde al modulo residentes
    dataset2011_react_24 <- reactive({
      data <- dataset2011[, 255:256]
      return(data)
    })
    
    # Modulo 8: vivienda
    dataset2011_react_25 <- reactive({
      data <- dataset2011[, 256:310]
      return(data)
    })
    
    # Tema lugar de la entrevista
    dataset2011_react_26 <- reactive({
      data <- dataset2011[, 311:312]
      return(data)
    })
    
    # vvcc: educacion
    dataset2011_react_27 <- reactive({
      data <- dataset2011[, 312:315]
      return(data)
    })
    
    
    
    
    # vvcc: empleo: aca no despliega:
    dataset2011_react_28 <- reactive({
      data <- dataset2011[, 316:320]
      return(data)
    })
    
    
    
    
    
    # vvcc linea de pobreza
    dataset2011_react_29 <- reactive({
      data <- dataset2011[, 321:325]
      return(data)
    })
    
    # vvcc: ingresos del trabajo:
    dataset2011_react_30 <- reactive({
      data <- dataset2011[, 326:329]
      return(data)
    })
    
    # vvcc: subsidios monetarios
    dataset2011_react_31 <- reactive({
      data <- dataset2011[, 330:365]
      return(data)
    })
    
    # vvcc: Otros ingresos
    dataset2011_react_32 <- reactive({
      data <- dataset2011[, 366:378]
      return(data)
    })
    
    # vvcc: Indices e indicadores
    dataset2011_react_33 <- reactive({
      data <- dataset2011[, 379:383]
      return(data)
    })
    
    # vvcc: Indice de hacinamiento
    dataset2011_react_34 <- reactive({
      data <- dataset2011[, 384:385]
      return(data)
    })
    
    # Fecha:
    dataset2011_react_35 <- reactive({
      data <- dataset2011[, 385:388]
      return(data)
    })
    
    
    ###############################################   2013 #################################
    
    #la base de datos total:
    dataset2013_react <- reactive({
      data <- dataset2013
      return(data)
    })
    
    dataset2013_react_1 <- reactive({
      data <- dataset2013[, 1:14]
      return(data)
    })
    
    dataset2013_react_2 <- reactive({
      data <- dataset2013[, 15:18]#educacion
      return(data)
    })
    
    
    dataset2013_react_3 <- reactive({
      data <- dataset2013[, 19:75]#trabajo
      return(data)
    })
    
    
    dataset2013_react_4 <- reactive({
      data <- dataset2013[, 76:121]#ingresos
      return(data)
    })
    
    
    dataset2013_react_5 <- reactive({
      data <- dataset2013[, 122:145]#salud
      return(data)
    })
    
    
    dataset2013_react_6 <- reactive({
      data <- dataset2013[, 146:204]#residentes
      return(data)
    })
    
    
    dataset2013_react_7 <- reactive({
      data <- dataset2013[, 205:231]#vivienda
      return(data)
    })
    
    dataset2013_react_8 <- reactive({
      data <- dataset2013[, 232:233]#ingresos
      return(data)
    })
    
    dataset2013_react_9 <- reactive({
      data <- dataset2013[, 233:244]#Expansiones
      return(data)
    })
    
    dataset2013_react_10 <- reactive({
      data <- dataset2013[, 245:249]#Informacion
      return(data)
    })
    
    dataset2013_react_11 <- reactive({
      data <- dataset2013[, 250:252]#Informacion
      return(data)
    })
    
    dataset2013_react_12 <- reactive({
      data <- dataset2013[, 253:254]#Indicadores
      return(data)
    })
    ##################################################################
    dataset2013_react_13 <- reactive({
      data <- dataset2013[, 254:269]#Residentes:patrimonio
      return(data)
    })
    
    dataset2013_react_14 <- reactive({
      data <- dataset2013[, 270:280]#Internet
      return(data)
    })
    
    dataset2013_react_15 <- reactive({
      data <- dataset2013[, 281:282]#Indicadores
      return(data)
    })
    
    dataset2013_react_16 <- reactive({
      data <- dataset2013[, 282:283]#Indicadores
      return(data)
    })
    
    dataset2013_react_17 <- reactive({
      data <- dataset2013[, 283:286]#Indicadores
      return(data)
    })
    
    dataset2013_react_18 <- reactive({
      data <- dataset2013[, 287:288]#Indicadores
      return(data)
    })
    
    dataset2013_react_19 <- reactive({
      data <- dataset2013[, 288:372]#
      return(data)
    })
    
    dataset2013_react_20 <- reactive({
      data <- dataset2013[, 373:374]#
      return(data)
    })
    
    dataset2013_react_21 <- reactive({
      data <- dataset2013[, 374:576]#
      return(data)
    })
    
    dataset2013_react_22 <- reactive({
      data <- dataset2013[, 577:579]#
      return(data)
    })
    
    dataset2013_react_23 <- reactive({
      data <- dataset2013[, 580:581]#Vaianza
      return(data)
    })
    
    dataset2013_react_24 <- reactive({
      data <- dataset2013[, 582:583]#vivienda
      return(data)
    })
    
    
    dataset2013_react_25 <- reactive({
      data <- dataset2013[, 583:588]#
      return(data)
    })
    
    dataset2013_react_26 <- reactive({
      data <- dataset2013[, 589:590]#
      return(data)
    })
    
    dataset2013_react_27 <- reactive({
      data <- dataset2013[, 590:593]#
      return(data)
    })
    
    dataset2013_react_28 <- reactive({
      data <- dataset2013[, 594:595]#
      return(data)
    })
    
    dataset2013_react_29 <- reactive({
      data <- dataset2013[, 595:597]#
      return(data)
    })
    
    dataset2013_react_30 <- reactive({
      data <- dataset2013[, 598:599]#
      return(data)
    })
    
    dataset2013_react_31 <- reactive({
      data <- dataset2013[, 599:600]#
      return(data)
    })
    
    
    
    ###############################################   2015 #################################
    
    #la base de datos total:
    dataset2015_react <- reactive({
      data <- dataset2015
      return(data)
    })
    
    dataset2015_react_1 <- reactive({
      data <- dataset2015[, 1:23]#Identificaión
      return(data)
    })
    
    dataset2015_react_2 <- reactive({
      data <- dataset2015[, 24:81]#educación
      return(data)
    })
    
    
    dataset2015_react_3 <- reactive({
      data <- dataset2015[, 82:127]#Empleo
      return(data)
    })
    
    
    dataset2015_react_4 <- reactive({
      data <- dataset2015[, 128:280]#ingresos
      return(data)
    })
    
    
    dataset2015_react_5 <- reactive({
      data <- dataset2015[, 281:339]#salud
      return(data)
    })
    
    
    dataset2015_react_6 <- reactive({
      data <- dataset2015[, 340:380]#Tema 1: Discapacidad
      return(data)
    })
    
    
    dataset2015_react_7 <- reactive({
      data <- dataset2015[, 381:393]#Migración 
      return(data)
    })
    
    dataset2015_react_8 <- reactive({
      data <- dataset2015[, 394:396]#Tema 2: Etnias 
      return(data)
    })
    
    dataset2015_react_9 <- reactive({
      data <- dataset2015[, 397:409]#Tema 3: Redes y Participación
      return(data)
    })
    
    dataset2015_react_10 <- reactive({
      data <- dataset2015[, 410:430]#Modulo Discriminación
      return(data)
    })
    
    dataset2015_react_11 <- reactive({
      data <- dataset2015[, 431:435]#Tema 4: Autobiografia
      return(data)
    })
    
    dataset2015_react_12 <- reactive({
      data <- dataset2015[, 436:450]#Modulo: Residentes (Patrimonio)
      return(data)
    })
    
    dataset2015_react_13 <- reactive({
      data <- dataset2015[, 451:460]#Modulo: Resídentes (Internet)
      return(data)
    })
    
    dataset2015_react_14 <- reactive({
      data <- dataset2015[, 461:462]#Modulo: Resídentes (Telefono movil)
      return(data)
    })
    
    dataset2015_react_15 <- reactive({
      data <- dataset2015[, 462:465]#Tema 5: Orientación sexual
      return(data)
    })
    
    dataset2015_react_16 <- reactive({
      data <- dataset2015[, 466:467]#Tema 5: Respuesta en modulo residentes
      return(data)
    })
    
    dataset2015_react_17 <- reactive({
      data <- dataset2015[, 467:536]#Vivienda
      return(data)
    })
    
    dataset2015_react_18 <- reactive({
      data <- dataset2015[, 537:729]# Ingresos
      return(data)
    })
    
    dataset2015_react_19 <- reactive({
      data <- dataset2015[, 730:734]# Pobreza quintiles y deciles
      return(data)
    })
    
    dataset2015_react_20 <- reactive({
      data <- dataset2015[, 735:737]# Expansión
      return(data)
    })
    
    dataset2015_react_21 <- reactive({
      data <- dataset2015[, 738:739]# Tema 6: Comunas auto representadas
      return(data)
    })
    
    dataset2015_react_22 <- reactive({
      data <- dataset2015[, 739:740]# Varianzas
      return(data)
    })
    
    dataset2015_react_23 <- reactive({
      data <- dataset2015[, 741:743]#Fecha
      return(data)
    })
    
    dataset2015_react_24 <- reactive({
      data <- dataset2015[, 744:745]#Tema 6: Numero de personas en el hogar
      return(data)
    })
    
    
    dataset2015_react_25 <- reactive({
      data <- dataset2015[, 745:748]# Variables creadas: Educación
      return(data)
    })
    
    dataset2015_react_26 <- reactive({
      data <- dataset2015[, 749:750]# Variables creadas: Condición de actividad
      return(data)
    })
    
    dataset2015_react_27 <- reactive({
      data <- dataset2015[, 750:752]#Variables creadas: Indicadores de calidad de la vivienda
      return(data)
    })
    
    dataset2015_react_28 <- reactive({
      data <- dataset2015[, 753:754]# Variables creadas: Indicadores de allegamiento
      return(data)
    })
    
    dataset2015_react_29 <- reactive({
      data <- dataset2015[, 755:756]# Variables creadas: Hacinamiento
      return(data)
    })
    
    dataset2015_react_30 <- reactive({
      data <- dataset2015[, 756:774]# Carecias
      return(data)
    })
    
    dataset2015_react_31 <- reactive({
      data <- dataset2015[, 775:776]# Pobreza
      return(data)
    })
    
    
    
    
    ###############################################   2017 #################################
    
    #la base de datos total:
    dataset2017_react <- reactive({
      data <- dataset2017
      return(data)
    })
    
    dataset2017_react_1 <- reactive({
      data <- dataset2017[, 1:8]#Identificaión
      return(data)
    })
    
    dataset2017_react_2 <- reactive({
      data <- dataset2017[, 9:11]#Expansión
      return(data)
    })
    
    
    dataset2017_react_3 <- reactive({
      data <- dataset2017[, 12:13]#Varianzas
      return(data)
    })
    
    
    dataset2017_react_4 <- reactive({
      data <- dataset2017[, 14:16]#Tema 1: Fecha
      return(data)
    })
    
    
    dataset2017_react_5 <- reactive({
      data <- dataset2017[, 17:20]#Tema 2: Totales
      return(data)
    })
    
    
    dataset2017_react_6 <- reactive({
      data <- dataset2017[, 21:22]#Tema 3: Respuesta al módulo registro de residentes
      return(data)
    })
    
    
    dataset2017_react_7 <- reactive({
      data <- dataset2017[, 22:36]#Tema 4: Identificacion continuacion 
      return(data)
    })
    
    dataset2017_react_8 <- reactive({
      data <- dataset2017[, 37:42]#Tema 5: Discapacidad 
      return(data)
    })
    
    dataset2017_react_9 <- reactive({
      data <- dataset2017[, 43:101]#Módulo educacion
      return(data)
    })
    
    dataset2017_react_10 <- reactive({
      data <- dataset2017[, 102:150]#Módulo Empleo
      return(data)
    })
    
    dataset2017_react_11 <- reactive({
      data <- dataset2017[, 151:304]#Módulo Ingreso
      return(data)
    })
    
    dataset2017_react_12 <- reactive({
      data <- dataset2017[, 305:363]#Módulo: Salud
      return(data)
    })
    
    dataset2017_react_13 <- reactive({
      data <- dataset2017[, 364:396]#Tema 6: Discapacidad
      return(data)
    })
    
    dataset2017_react_14 <- reactive({
      data <- dataset2017[, 397:398]#Tema 7: Quien responde al modulo salud
      return(data)
    })
    
    dataset2017_react_15 <- reactive({
      data <- dataset2017[, 398:410]#Módulo: Migracion
      return(data)
    })
    
    dataset2017_react_16 <- reactive({
      data <- dataset2017[, 411:413]#Tema 8: Etnia
      return(data)
    })
    
    dataset2017_react_17 <- reactive({
      data <- dataset2017[, 414:426]#Tema 9: Redes y participacion
      return(data)
    })
    
    dataset2017_react_18 <- reactive({
      data <- dataset2017[, 427:434]# Tema 10: Preocupacion por la alimentacion
      return(data)
    })
    
    dataset2017_react_19 <- reactive({
      data <- dataset2017[, 435:464]# Módulo: Discriminacion
      return(data)
    })
    
    dataset2017_react_20 <- reactive({
      data <- dataset2017[, 466:469]# Tema 11: Auto-biografia
      return(data)
    })
    
    dataset2017_react_21 <- reactive({
      data <- dataset2017[, 470:481]# Módulo: Residentes(Patrimonio)
      return(data)
    })
    
    dataset2017_react_22 <- reactive({
      data <- dataset2017[, 482:493]# Módulo: Residentes(Internet)
      return(data)
    })
    
    dataset2017_react_23 <- reactive({
      data <- dataset2017[, 494:495]#Módulo: Residentes(Telefono movil)
      return(data)
    })
    
    dataset2017_react_24 <- reactive({
      data <- dataset2017[, 495:498]#Tema 12: Orientacion sexual
      return(data)
    })
    
    
    dataset2017_react_25 <- reactive({
      data <- dataset2017[, 499:500]#Tema 13: Respuesta a módulo: Redes y participacion
      return(data)
    })
    
    dataset2017_react_26 <- reactive({
      data <- dataset2017[, 500:573]# Moódulo: Vivienda
      return(data)
    })
    
    dataset2017_react_27 <- reactive({
      data <- dataset2017[, 574:766]#VMódulo Ingresos II
      return(data)
    })
    
    dataset2017_react_28 <- reactive({
      data <- dataset2017[, 767:771]# Módulo: Pobreza, quintiles y deciles
      return(data)
    })
    
    dataset2017_react_29 <- reactive({
      data <- dataset2017[, 772:773]#Tema 14: Numero de personas en el hogar
      return(data)
    })
    
    dataset2017_react_30 <- reactive({
      data <- dataset2017[, 773:776]#Módulo: Variables creadas(Educacion)
      return(data)
    })
    
    dataset2017_react_31 <- reactive({
      data <- dataset2017[, 777:778]#Módulo: Variables creadas(Condicion de actividad)
      return(data)
    })
    
    dataset2017_react_32 <- reactive({
      data <- dataset2017[, 778:780]#Módulo: Variables creadas(Indicadores de calidad de la vivienda)
      return(data)
    })
    dataset2017_react_33 <- reactive({
      data <- dataset2017[, 781:782]#Módulo: Variables creadas(Indicadores de allegamiento)
      return(data)
    })
    dataset2017_react_34 <- reactive({
      data <- dataset2017[, 783:784]#Módulo: Variables creadas(Hacinamiento)
      return(data)
    })
    dataset2017_react_35 <- reactive({
      data <- dataset2017[, 784:802]#Módulo: Variables creadas(Carencias)
      return(data)
    })
    dataset2017_react_36 <- reactive({
      data <- dataset2017[, 803:804]#Módulo: Variables creadas(Pobreza)
      return(data)
    })
    ###############################################
    
    
    
    
    
    
    mydata_educacion_7000 <- reactive({
        datos_dfe <- datos_df_casen_2015_mil[, 1:776]
        datos_dfe 
        return(datos_dfe)
    })
    
    mydata_educacion_8000 <- reactive({
       # datos_dfe <- datos_df_casen_2017_mil[, 1:16]
        datos_dfe <- datos_df_casen_2017_mil
        return(datos_dfe)
    })
    
    table2017_I <- reactive({
      data <- datos_df_casen_2017_mil[, 1:16]
      return(data)
    })
    
    table2017_Iedu <- reactive({
      data <- datos_df_casen_2017_mil[, 43:102]
      return(data)
    })
    
    table2017_Itrab <- reactive({
      data <- datos_df_casen_2017_mil[, 102:152]
      return(data)
    })
    
    table2017_Iing <- reactive({
      data <- datos_df_casen_2017_mil[, 152:305]
      return(data)
    })
    
    table2017_Isal <- reactive({
      data <- datos_df_casen_2017_mil[, 305:398]
      return(data)
    })
    
    table2017_Iid <- reactive({
      data <- datos_df_casen_2017_mil[, 398:499]
      return(data)
    })
    
    table2017_Iviv <- reactive({
      data <- datos_df_casen_2017_mil[, 500:808]
      return(data)
    })
    
    prueba_tablaedu <- reactive({
        datos_dfe <- datos_df_casen_2017_miledu[, 43:102]
        datos_dfe 
        return(datos_dfe)
    })
    

    
    ###################### carga de las tablas en su totalidad ################################
    ###########################################################################################
    
    ###################### despliegue total de la tabla 2006 ##################################

    output$table_2006 <- renderDataTable(dataset2006_react())
    
    ###########################################################################################
    
    output$table_2009 <- renderDataTable(dataset2009_react())
    
    ###########################################################################################
    
    output$table_2011 <- renderDataTable(dataset2011_react())
    
    ###########################################################################################
    
    output$table_2013 <- renderDataTable(dataset2013_react())
    
    ###########################################################################################
    
    output$table_2015 <- renderDataTable(dataset2015_react())
    
    ###########################################################################################
    
    
    output$table2009mn <- renderDataTable(mydata_educacion_3000())
    output$table2011mn <- renderDataTable(mydata_educacion_4000())
    output$table20ymt <- renderDataTable(mydata_educacion_5000())
    output$table2013 <- renderDataTable(mydata_educacion_6000())
    output$table2015 <- renderDataTable(mydata_educacion_7000())
    output$table2017 <- renderDataTable(mydata_educacion_8000())
    
    
    output$table2017_I <- renderDataTable(table2017_I())
    output$table2017_Iedu <- renderDataTable(table2017_Iedu())
    output$table2017_Itrab <- renderDataTable(table2017_Itrab())
    output$table2017_Iing <- renderDataTable(table2017_Iing())
    output$table2017_Isal <- renderDataTable(table2017_Isal())
    output$table2017_Iid <- renderDataTable(table2017_Iid())
    output$table2017_Iviv <- renderDataTable(table2017_Iviv())
    
    output$prueba_tablaedu <- renderDataTable(prueba_tablaedu())
    
    
    ########################################################################## 2006 modulos  ##########################################################################  
    output$modulo_1_2006 <- renderDataTable(dataset2006_react_1())
    output$modulo_2_2006 <- renderDataTable(dataset2006_react_2())
    output$modulo_3_2006 <- renderDataTable(dataset2006_react_3())
    output$modulo_4_2006 <- renderDataTable(dataset2006_react_4())
    output$modulo_5_2006 <- renderDataTable(dataset2006_react_5())
    output$modulo_6_2006 <- renderDataTable(dataset2006_react_6())
    output$modulo_7_2006 <- renderDataTable(dataset2006_react_7())
    output$modulo_8_2006 <- renderDataTable(dataset2006_react_8())
    output$modulo_9_2006 <- renderDataTable(dataset2006_react_9())
    output$modulo_10_2006 <- renderDataTable(dataset2006_react_10())
    output$modulo_11_2006 <- renderDataTable(dataset2006_react_11())
    output$modulo_12_2006 <- renderDataTable(dataset2006_react_12())
    output$modulo_13_2006 <- renderDataTable(dataset2006_react_13())
    output$modulo_14_2006 <- renderDataTable(dataset2006_react_14())
    output$modulo_15_2006 <- renderDataTable(dataset2006_react_15())
    output$modulo_16_2006 <- renderDataTable(dataset2006_react_16())
    output$modulo_17_2006 <- renderDataTable(dataset2006_react_17())
    output$modulo_18_2006 <- renderDataTable(dataset2006_react_18())
    output$modulo_19_2006 <- renderDataTable(dataset2006_react_19())
    output$modulo_20_2006 <- renderDataTable(dataset2006_react_20())
    
    ########################################################################## 2009 modulos  ##########################################################################  
    output$modulo_1_2009 <- renderDataTable(dataset2009_react_1())
    output$modulo_2_2009 <- renderDataTable(dataset2009_react_2())
    
    
    output$modulo_3_2009 <- renderDataTable(dataset2009_react_3())
    output$modulo_31_2009 <- renderDataTable(dataset2009_react_31())
    output$modulo_32_2009 <- renderDataTable(dataset2009_react_32())
    output$modulo_33_2009 <- renderDataTable(dataset2009_react_33())
    output$modulo_34_2009 <- renderDataTable(dataset2009_react_34())
    
    output$modulo_4_2009 <- renderDataTable(dataset2009_react_4())
    output$modulo_5_2009 <- renderDataTable(dataset2009_react_5())
    output$modulo_6_2009 <- renderDataTable(dataset2009_react_6())
    output$modulo_7_2009 <- renderDataTable(dataset2009_react_7())
    output$modulo_8_2009 <- renderDataTable(dataset2009_react_8())
    output$modulo_9_2009 <- renderDataTable(dataset2009_react_9())
    output$modulo_10_2009 <- renderDataTable(dataset2009_react_10())
    output$modulo_11_2009 <- renderDataTable(dataset2009_react_11())
    output$modulo_12a_2009 <- renderDataTable(dataset2009_react_12a())
    output$modulo_12_2009 <- renderDataTable(dataset2009_react_12())
    output$modulo_13_2009 <- renderDataTable(dataset2009_react_13())
    output$modulo_14_2009 <- renderDataTable(dataset2009_react_14())
    output$modulo_14a_2009 <- renderDataTable(dataset2009_react_14a())
    output$modulo_15_2009 <- renderDataTable(dataset2009_react_15())
    output$modulo_16_2009 <- renderDataTable(dataset2009_react_16())
    output$modulo_17_2009 <- renderDataTable(dataset2009_react_17())
    output$modulo_18_2009 <- renderDataTable(dataset2009_react_18())
    output$modulo_19_2009 <- renderDataTable(dataset2009_react_19())
    output$modulo_20_2009 <- renderDataTable(dataset2009_react_20())
    output$modulo_21_2009 <- renderDataTable(dataset2009_react_21())
    
    
    
    ########################################################################## 2011 modulos  ##########################################################################  
    output$modulo_1_2011 <- renderDataTable(dataset2011_react_1())
    output$modulo_1b_2011 <- renderDataTable(dataset2011_react_1b())
    output$modulo_2_2011 <- renderDataTable(dataset2011_react_2())
    output$modulo_3_2011 <- renderDataTable(dataset2011_react_3())
    output$modulo_4_2011 <- renderDataTable(dataset2011_react_4())
    output$modulo_5_2011 <- renderDataTable(dataset2011_react_5())
    # output$modulo_6_2011 <- renderDataTable(dataset2011_react_6())
    
    output$modulo_Chile_solidario_2011 <- renderDataTable(dataset2011_react_7()) # Chile solidario
    
    
    
    
    # Ingresos:
    output$modulo_7_2011 <- renderDataTable(dataset2011_react_8()) 
    
    
    
    output$modulo_8_2011 <- renderDataTable(dataset2011_react_9())
    
    
    
    output$modulo_Discapacidad_2011 <- renderDataTable(dataset2011_react_10())
    output$modulo_Quien_responde_al_modulo_Salud_2011 <- renderDataTable(dataset2011_react_11())
    output$modulo_Migracion_2011 <- renderDataTable(dataset2011_react_12())
    
    # aca voy
    

    

    
    output$modulo_Autobiografía_2011 <- renderDataTable(dataset2011_react_17())
    
    output$modulo_Etnia_2011 <- renderDataTable(dataset2011_react_18())
    
    output$modulo_Participacion_2011 <- renderDataTable(dataset2011_react_19())
    
    output$modulo_Residentes_Patrimonio_2011 <- renderDataTable(dataset2011_react_20())
    
    output$modulo_Internet_2011 <- renderDataTable(dataset2011_react_21())
    
    output$modulo_telefono_movil_2011 <- renderDataTable(dataset2011_react_22())
    
    output$modulo_Satisfaccion_con_la_vida_2011 <- renderDataTable(dataset2011_react_23())
    
    output$modulo_Quien_responde_al_modulo_residentes_2011 <- renderDataTable(dataset2011_react_24())
    
    
    output$modulo_9_2011 <- renderDataTable(dataset2011_react_25())
    
    output$modulo_Lugar_donde_se_produce_la_entrevista_2011 <- renderDataTable(dataset2011_react_26())
    
    output$vvcc_Educacion_2011 <- renderDataTable(dataset2011_react_27())
    
    output$vvcc_Empleo_2011 <- renderDataTable(dataset2011_react_28())
    
    
    output$vvcc_Linea_de_pobreza_2011 <- renderDataTable(dataset2011_react_29())
    
    output$vvcc_Ingresos_del_trabajo_2011 <- renderDataTable(dataset2011_react_30())
    
    output$vvcc_Subsidios_monetarios_2011 <- renderDataTable(dataset2011_react_31())
    
    output$vvcc_Otros_Ingresos_2011 <- renderDataTable(dataset2011_react_32())
    
    
    output$vvcc_Indices_2011 <- renderDataTable(dataset2011_react_33())
    
    output$vvcc_Indice_de_Hacinamiento_2011 <- renderDataTable(dataset2011_react_34())
    
    output$fecha_2011 <- renderDataTable(dataset2011_react_35())
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ########################################################################## 2013 modulos  ##########################################################################  
    output$modulo_1_2013 <- renderDataTable(dataset2013_react_1())
    output$modulo_2_2013 <- renderDataTable(dataset2013_react_2())
    output$modulo_3_2013 <- renderDataTable(dataset2013_react_3())
    output$modulo_4_2013 <- renderDataTable(dataset2013_react_4())
    output$modulo_5_2013 <- renderDataTable(dataset2013_react_5())
    output$modulo_6_2013 <- renderDataTable(dataset2013_react_6())
    output$modulo_7_2013 <- renderDataTable(dataset2013_react_7())
    output$modulo_8_2013 <- renderDataTable(dataset2013_react_8())
    output$modulo_9_2013 <- renderDataTable(dataset2013_react_9())
    output$modulo_10_2013 <- renderDataTable(dataset2013_react_10())
    output$modulo_11_2013 <- renderDataTable(dataset2013_react_11())
    output$modulo_12_2013 <- renderDataTable(dataset2013_react_12())
    output$modulo_13_2013 <- renderDataTable(dataset2013_react_13())
    output$modulo_14_2013 <- renderDataTable(dataset2013_react_14())
    output$modulo_15_2013 <- renderDataTable(dataset2013_react_15())
    output$modulo_16_2013 <- renderDataTable(dataset2013_react_16())
    output$modulo_17_2013 <- renderDataTable(dataset2013_react_17())
    output$modulo_18_2013 <- renderDataTable(dataset2013_react_18())
    output$modulo_19_2013 <- renderDataTable(dataset2013_react_19())
    output$modulo_20_2013 <- renderDataTable(dataset2013_react_20())
    output$modulo_21_2013 <- renderDataTable(dataset2013_react_21())
    output$modulo_22_2013 <- renderDataTable(dataset2013_react_22())
    output$modulo_23_2013 <- renderDataTable(dataset2013_react_23())
    output$modulo_24_2013 <- renderDataTable(dataset2013_react_24())
    output$modulo_25_2013 <- renderDataTable(dataset2013_react_25())
    output$modulo_26_2013 <- renderDataTable(dataset2013_react_26())
    output$modulo_27_2013 <- renderDataTable(dataset2013_react_27())
    output$modulo_28_2013 <- renderDataTable(dataset2013_react_28())
    output$modulo_29_2013 <- renderDataTable(dataset2013_react_29())
    output$modulo_30_2013 <- renderDataTable(dataset2013_react_30())
    output$modulo_31_2013 <- renderDataTable(dataset2013_react_31())
    ########################################################################## fin modulos 2013  ##########################################################################  
    
    ########################################################################## 2015 modulos  ##########################################################################  
    output$modulo_1_2015 <- renderDataTable(dataset2015_react_1())
    output$modulo_2_2015 <- renderDataTable(dataset2015_react_2())
    output$modulo_3_2015 <- renderDataTable(dataset2015_react_3())
    output$modulo_4_2015 <- renderDataTable(dataset2015_react_4())
    output$modulo_5_2015 <- renderDataTable(dataset2015_react_5())
    output$modulo_6_2015 <- renderDataTable(dataset2015_react_6())
    output$modulo_7_2015 <- renderDataTable(dataset2015_react_7())
    output$modulo_8_2015 <- renderDataTable(dataset2015_react_8())
    output$modulo_9_2015 <- renderDataTable(dataset2015_react_9())
    output$modulo_10_2015 <- renderDataTable(dataset2015_react_10())
    output$modulo_11_2015 <- renderDataTable(dataset2015_react_11())
    output$modulo_12_2015 <- renderDataTable(dataset2015_react_12())
    output$modulo_13_2015 <- renderDataTable(dataset2015_react_13())
    output$modulo_14_2015 <- renderDataTable(dataset2015_react_14())
    output$modulo_15_2015 <- renderDataTable(dataset2015_react_15())
    output$modulo_16_2015 <- renderDataTable(dataset2015_react_16())
    output$modulo_17_2015 <- renderDataTable(dataset2015_react_17())
    output$modulo_18_2015 <- renderDataTable(dataset2015_react_18())
    output$modulo_19_2015 <- renderDataTable(dataset2015_react_19())
    output$modulo_20_2015 <- renderDataTable(dataset2015_react_20())
    output$modulo_21_2015 <- renderDataTable(dataset2015_react_21())
    output$modulo_22_2015 <- renderDataTable(dataset2015_react_22())
    output$modulo_23_2015 <- renderDataTable(dataset2015_react_23())
    output$modulo_24_2015 <- renderDataTable(dataset2015_react_24())
    output$modulo_25_2015 <- renderDataTable(dataset2015_react_25())
    output$modulo_26_2015 <- renderDataTable(dataset2015_react_26())
    output$modulo_27_2015 <- renderDataTable(dataset2015_react_27())
    output$modulo_28_2015 <- renderDataTable(dataset2015_react_28())
    output$modulo_29_2015 <- renderDataTable(dataset2015_react_29())
    output$modulo_30_2015 <- renderDataTable(dataset2015_react_30())
    output$modulo_31_2015 <- renderDataTable(dataset2015_react_31())
    
    
    ########################################################################## 2017 modulos  ##########################################################################  
    output$modulo_1_2017 <- renderDataTable(dataset2017_react_1())
    output$modulo_2_2017 <- renderDataTable(dataset2017_react_2())
    output$modulo_3_2017 <- renderDataTable(dataset2017_react_3())
    output$modulo_4_2017 <- renderDataTable(dataset2017_react_4())
    output$modulo_5_2017 <- renderDataTable(dataset2017_react_5())
    output$modulo_6_2017 <- renderDataTable(dataset2017_react_6())
    output$modulo_7_2017 <- renderDataTable(dataset2017_react_7())
    output$modulo_8_2017 <- renderDataTable(dataset2017_react_8())
    output$modulo_9_2017 <- renderDataTable(dataset2017_react_9())
    output$modulo_10_2017 <- renderDataTable(dataset2017_react_10())
    output$modulo_11_2017 <- renderDataTable(dataset2017_react_11())
    output$modulo_12_2017 <- renderDataTable(dataset2017_react_12())
    output$modulo_13_2017 <- renderDataTable(dataset2017_react_13())
    output$modulo_14_2017 <- renderDataTable(dataset2017_react_14())
    output$modulo_15_2017 <- renderDataTable(dataset2017_react_15())
    output$modulo_16_2017 <- renderDataTable(dataset2017_react_16())
    output$modulo_17_2017 <- renderDataTable(dataset2017_react_17())
    output$modulo_18_2017 <- renderDataTable(dataset2017_react_18())
    output$modulo_19_2017 <- renderDataTable(dataset2017_react_19())
    output$modulo_20_2017 <- renderDataTable(dataset2017_react_20())
    output$modulo_21_2017 <- renderDataTable(dataset2017_react_21())
    output$modulo_22_2017 <- renderDataTable(dataset2017_react_22())
    output$modulo_23_2017 <- renderDataTable(dataset2017_react_23())
    output$modulo_24_2017 <- renderDataTable(dataset2017_react_24())
    output$modulo_25_2017 <- renderDataTable(dataset2017_react_25())
    output$modulo_26_2017 <- renderDataTable(dataset2017_react_26())
    output$modulo_27_2017 <- renderDataTable(dataset2017_react_27())
    output$modulo_28_2017 <- renderDataTable(dataset2017_react_28())
    output$modulo_29_2017 <- renderDataTable(dataset2017_react_29())
    output$modulo_30_2017 <- renderDataTable(dataset2017_react_30())
    output$modulo_31_2017 <- renderDataTable(dataset2017_react_31())
    output$modulo_32_2017 <- renderDataTable(dataset2017_react_32())
    output$modulo_33_2017 <- renderDataTable(dataset2017_react_33())
    output$modulo_34_2017 <- renderDataTable(dataset2017_react_34())
    output$modulo_35_2017 <- renderDataTable(dataset2017_react_35())
    output$modulo_36_2017 <- renderDataTable(dataset2017_react_36())
    ###################################################################################################################################################  
    
    ################################################################################
    ################# Set de funciones para las bases de datos #####################
    ################################################################################
    
    ########################################################################## 2006  ##########################################################################  
  
    output$tabla_d_c_generalizada_2006<-renderPrint({

        d <- input$p2006_primerav
        e <- input$p2006_segundav
        f <- input$p2006_tercerav
        g <- input$p2006_cuartav
        
        ab <-  dataset2006
      #  ab <- dataset2006_react()
    
        a <- ab[,d]
        b <- ab[,e] 
        c <- ab[,f] 
        d <- ab[,g] 
        
        cross_tab = table(a, b, c, d)

        return(cross_tab)
    })
    
    output$tabla_d_c_generalizada_pon_2006<-renderPrint({
        
        d <- input$p2006_primerav
        e <- input$p2006_segundav
        f <- input$p2006_tercerav
        g <- input$p2006_cuartav
        
        ab <- dataset2006
        
        a <- ab[,d]
        b <- ab[,e] 
        c <- ab[,f] 
        d <- ab[,g] 
        
        cross_tab = xtabs(ab$expc ~ unlist(a) + unlist(b)+unlist(c)+unlist(d),aggregate(ab$expc ~ unlist(a)+unlist(b)+unlist(c)+unlist(d),ab,mean))
        
        return(cross_tab)
    })
    
    
    output$cyb_2006 <- renderPlot({
        
        a <- input$ptabla_cyb
        b <- dataset2006_react()
        c <- b[,a]
        b %>%
            ggplot(aes(x = a, y = unlist(c), fill = a)) + geom_boxplot() + 
            scale_fill_manual(values=c("olivedrab2"))+
            theme(
                legend.position="none",
                plot.title = element_text(size=11)
            ) +
            ggtitle("Diagrama de caja y bigotes") +
            xlab("")
    })
    
    plot2006 <- reactive({
        df <- dataset2006_react()
        col <- input$ptabla_cyb
        p <-   ggplot(df, aes(0, df[,col])) +  geom_boxplot()
    })
    
    output$plot_cyb_2006 <- downloadHandler(
        filename = function() { paste(input$ptabla_cyb, '.png', sep='') },
        content = function(file) {
            device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
            ggsave(file, plot = plot2006(), device = device)
        }
    )
    
        output$promedios_2006<-renderPrint({
        a <- input$ptabla_promedios
        b <- dataset2006_react()
        p <- b[,a]
        o <- summary(p)
        return(o)
    })
    

        output$promedios_filtros_2006<-renderTable({
            a <- input$nivel_filtro
            b <- input$categoria_filtro
            
            data_2006 <- dataset2006_react()
            
            c <- data_2006[,a]
            d <- data_2006[,b]
            
            # base_del_2006_terr <- base_del_2006[,b]
            # base_del_2006_terr[is.na(base_del_2006_terr)] <- 0
            # base_del_2006_cat <- base_del_2006[,a]
            
            promedios_grupales <-aggregate(d, by=list(c), FUN = mean , na.rm = TRUE)
            # promedios_grupales <- aggregate(b, by=list(a), FUN = mean , na.rm = TRUE)
        }) 
        
        output$boton_ttcc_2006 <- downloadHandler(
            filename = function() {
                paste("ttcc_2006.csv", "csv", sep=".")
            },
            content = function(file) {
                
                d <- input$p2006_primerav
                e <- input$p2006_segundav
                f <- input$p2006_tercerav
                g <- input$p2006_cuartav
                
                ab <-  dataset2006
                #  ab <- dataset2006_react()
                
                a <- ab[,d]
                b <- ab[,e] 
                c <- ab[,f] 
                d <- ab[,g] 
                
                cross_tab = table(a, b, c, d)
                
                write.csv(cross_tab, file)
            }
        )
        
        output$boton_ttcc_2006_pon <- downloadHandler(
            filename = function() {
                paste("ttcc_2006_pon.csv", "csv", sep=".")
            },
            content = function(file) {
                
                
                d <- input$p2006_primerav
                e <- input$p2006_segundav
                f <- input$p2006_tercerav
                g <- input$p2006_cuartav
                
                ab <- dataset2006
                
                a <- ab[,d]
                b <- ab[,e] 
                c <- ab[,f] 
                d <- ab[,g] 
                
                cross_tab = xtabs(ab$expc ~ unlist(a) + unlist(b)+unlist(c)+unlist(d),aggregate(ab$expc ~ unlist(a)+unlist(b)+unlist(c)+unlist(d),ab,mean))
                
                
                write.csv(cross_tab, file)
                
            }
        )
        
        
        
        
        
        
########################################################################## 2009  ##########################################################################  
        
        output$tabla_d_c_generalizada_2009<-renderPrint({
            
            d <- input$p2009_primerav
            e <- input$p2009_segundav
            f <- input$p2009_tercerav
            g <- input$p2009_cuartav
            
            ab <-  dataset2009
            
            a <- ab[,d]
            b <- ab[,e] 
            c <- ab[,f] 
            d <- ab[,g] 
            
            cross_tab = table(a, b, c, d)
            
            return(cross_tab)
        })
        
        
        output$tabla_d_c_generalizada_pon_2009<-renderPrint({
            
            d <- input$p2009_primerav
            e <- input$p2009_segundav
            f <- input$p2009_tercerav
            g <- input$p2009_cuartav
            
            ab <- dataset2009
            
            a <- ab[,d]
            b <- ab[,e] 
            c <- ab[,f] 
            d <- ab[,g] 
            
            cross_tab = xtabs(ab[,16] ~ unlist(a) + unlist(b)+unlist(c)+unlist(d),aggregate(ab[,16] ~ unlist(a)+unlist(b)+unlist(c)+unlist(d),ab,mean))
            
            return(cross_tab)
        }) 
        
        output$promedios_2009<-renderPrint({
            a <- input$input_promedios_2009
            b <- dataset2009_react()
            p<- b[,a]
            o <- summary(p)
            return(o)
            
        })
        
        output$cyb_2009 <- renderPlot({
            a <- input$input_cyb_2009
            b <- dataset2009_react()
            c <- b[,a]
            
            b %>% ggplot(aes(x = a, y = unlist(c), fill = a)) + geom_boxplot() + 
                scale_fill_manual(values=c("olivedrab2"))+
                theme(
                    legend.position="none",
                    plot.title = element_text(size=11)
                ) +
                ggtitle("Diagrama de caja y bigotes") + xlab("")
        })
        
        
        output$promedios_filtros_2009<-renderTable({
            
            a <- input$nivel_filtro_2009
            b <- input$categoria_filtro_2009
            
            data_2009 <- dataset2009_react()
            
            c <- data_2009[,a]
            d <- data_2009[,b]
            
            promedios_grupales <-aggregate(d, by=list(c), FUN = mean , na.rm = TRUE)
        }) 
        

        output$boton_ttcc_2009 <- downloadHandler(
            filename = function() {
                paste("ttcc_2009.csv", "csv", sep=".")
            },
            content = function(file) {
                
                d <- input$p2009_primerav
                e <- input$p2009_segundav
                f <- input$p2009_tercerav
                g <- input$p2009_cuartav
                
                ab <-  dataset2009
                
                a <- ab[,d]
                b <- ab[,e] 
                c <- ab[,f] 
                d <- ab[,g] 
                
                cross_tab = table(a, b, c, d)
                
                write.csv(cross_tab, file)
            }
        )
        
        output$boton_ttcc_2009_pon <- downloadHandler(
            filename = function() {
                paste("ttcc_2009_pon.csv", "csv", sep=".")
            },
            content = function(file) {
                
                
                d <- input$p2009_primerav
                e <- input$p2009_segundav
                f <- input$p2009_tercerav
                g <- input$p2009_cuartav
                
                ab <- dataset2009
                
                a <- ab[,d]
                b <- ab[,e] 
                c <- ab[,f] 
                d <- ab[,g] 
                
                cross_tab = xtabs(ab[,16]  ~ unlist(a) + unlist(b)+unlist(c)+unlist(d),aggregate(ab[,16]  ~ unlist(a)+unlist(b)+unlist(c)+unlist(d),ab,mean))
                
                
                write.csv(cross_tab, file)
                
            }
        )
        
        plot2009 <- reactive({
            df <- dataset2009_react()
            col <- input$input_cyb_2009
            p <-   ggplot(df, aes(0, df[,col])) +  geom_boxplot()
        })
        
        output$plot_cyb_2009 <- downloadHandler(
            filename = function() { paste(input$input_cyb_2009, '.png', sep='') },
            content = function(file) {
                device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
                ggsave(file, plot = plot2009(), device = device)
            }
        )
        

        

        ########################################################################## 2013  ######################################################################
        
        output$tabla_d_c_generalizada_2013<-renderPrint({
          
          d <- input$ptabla2013_primeravx
          e <- input$ptabla2013_segundavx
          f <- input$ptabla2013_terceravx
          g <- input$ptabla2013_cuartavx
          
          ab <- dataset2013_react()
          
          
          a <- ab[,d]
          b <- ab[,e] 
          c <- ab[,f] 
          d <- ab[,g] 
          
          cross_tab = table(a, b, c, d)
          
          return(cross_tab)
        })
        
        output$tabla_d_c_generalizada_2011<-renderPrint({
          
          d <- input$p2011_primerav
          e <- input$p2011_segundav
          f <- input$p2011_tercerav
          g <- input$p2011_cuartav
          
          ab <- dataset2011
          
          a <- ab[,d]
          b <- ab[,e] 
          c <- ab[,f] 
          d <- ab[,g] 
          
          cross_tab = table(a, b, c, d)
          
          return(cross_tab)
          
        }) 
        
        output$tabla_d_c_generalizada_pon_2011<-renderPrint({
          
          d <- input$p2011_primerav
          e <- input$p2011_segundav
          f <- input$p2011_tercerav
          g <- input$p2011_cuartav
          
          ab <- dataset2011
          
          a <- ab[,d]
          b <- ab[,e] 
          c <- ab[,f] 
          d <- ab[,g] 
          
          cross_tab = xtabs(ab[,11] ~ unlist(a) + unlist(b)+unlist(c)+unlist(d),aggregate(ab[,11] ~ unlist(a)+unlist(b)+unlist(c)+unlist(d),ab,mean))
          
          return(cross_tab)
        }) 
        
        output$promedios_2011<-renderPrint({
          a <- input$input_promedios_2011
          b <- dataset2011_react()
          p<- b[,a]
          o <- summary(p)
          return(o)
          
        })
        
        output$cyb_2011 <- renderPlot({
          a <- input$input_cyb_2011
          b <- dataset2011_react()
          c <- b[,a]
          
          b %>% ggplot(aes(x = a, y = unlist(c), fill = a)) + geom_boxplot() + 
            scale_fill_manual(values=c("olivedrab2"))+
            theme(
              legend.position="none",
              plot.title = element_text(size=11)
            ) +
            ggtitle("Diagrama de caja y bigotes") + xlab("")
        })
        
        
        output$promedios_filtros_2011<-renderTable({
          
          a <- input$nivel_filtro_2011
          b <- input$categoria_filtro_2011
          
          data_2011 <- dataset2011_react()
          
          c <- data_2011[,a]
          d <- data_2011[,b]
          
          promedios_grupales <-aggregate(d, by=list(c), FUN = mean , na.rm = TRUE)
        }) 
        
        
        output$boton_ttcc_2011 <- downloadHandler(
          filename = function() {
            paste("ttcc_2011.csv", "csv", sep=".")
          },
          content = function(file) {
            
            d <- input$p2011_primerav
            e <- input$p2011_segundav
            f <- input$p2011_tercerav
            g <- input$p2011_cuartav
            
            ab <-  dataset2011
            
            a <- ab[,d]
            b <- ab[,e] 
            c <- ab[,f] 
            d <- ab[,g] 
            
            cross_tab = table(a, b, c, d)
            
            write.csv(cross_tab, file)
          }
        )
        
        output$boton_ttcc_2011_pon <- downloadHandler(
          filename = function() {
            paste("ttcc_2011_pon.csv", "csv", sep=".")
          },
          content = function(file) {
            
            
            d <- input$p2011_primerav
            e <- input$p2011_segundav
            f <- input$p2011_tercerav
            g <- input$p2011_cuartav
            
            ab <- dataset2011
            
            a <- ab[,d]
            b <- ab[,e] 
            c <- ab[,f] 
            d <- ab[,g] 
            
            cross_tab = xtabs(ab[,16]  ~ unlist(a) + unlist(b)+unlist(c)+unlist(d),aggregate(ab[,16]  ~ unlist(a)+unlist(b)+unlist(c)+unlist(d),ab,mean))
            
            
            write.csv(cross_tab, file)
            
          }
        )
        
        plot2011 <- reactive({
          df <- dataset2011_react()
          col <- input$input_cyb_2011
          p <-   ggplot(df, aes(0, df[,col])) +  geom_boxplot()
        })
        
        output$plot_cyb_2011 <- downloadHandler(
          filename = function() { paste(input$input_cyb_2011, '.png', sep='') },
          content = function(file) {
            device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
            ggsave(file, plot = plot2011(), device = device)
          }
        )
        
        
        
        
        
        output$tabla_d_c_generalizada_2013_pon<-renderPrint({
          
          d <- input$ptabla2013_primeravx
          e <- input$ptabla2013_segundavx
          f <- input$ptabla2013_terceravx
          g <- input$ptabla2013_cuartavx
          
          ab <- dataset2013_react()
          
          a <- ab[,d]
          b <- ab[,e] 
          c <- ab[,f] 
          d <- ab[,g] 
          
          cross_tab = xtabs(ab$expc ~ unlist(a) + unlist(b)+unlist(c)+unlist(d),aggregate(ab$expc ~ unlist(a)+unlist(b)+unlist(c)+unlist(d),ab,mean))
          
          return(cross_tab)
        })
        
        
        output$tabla_2013_csv <- downloadHandler(
          filename = function() {
            paste("ttcc_2013.csv", "csv", sep=".")
          },
          content = function(file) {
            
            d <- input$ptabla2013_primeravx
            e <- input$ptabla2013_segundavx
            f <- input$ptabla2013_terceravx
            g <- input$ptabla2013_cuartavx
            
            ab <- dataset2013_react()
            
            a <- ab[,d]
            b <- ab[,e] 
            c <- ab[,f] 
            d <- ab[,g] 
            
            cross_tab = table(a, b, c, d)
            
            write.csv(cross_tab, file)
          }
        )
        
        output$tabla_2013_csv_pon <- downloadHandler(
          filename = function() {
            paste("ttcc_2013_pon.csv", "csv", sep=".")
          },
          content = function(file) {
            
            
            d <- input$ptabla2013_primeravx
            e <- input$ptabla2013_segundavx
            f <- input$ptabla2013_terceravx
            g <- input$ptabla2013_cuartavx
            
            ab <- dataset2013_react()
            
            a <- ab[,d]
            b <- ab[,e] 
            c <- ab[,f] 
            d <- ab[,g] 
            
            cross_tab = xtabs(ab[,578] ~ unlist(a) + unlist(b)+unlist(c)+unlist(d),aggregate(ab[,578]  ~ unlist(a)+unlist(b)+unlist(c)+unlist(d),ab,mean))
            
            write.csv(cross_tab, file)
            
          }
        )
        
        
        
        output$promedios_2013<-renderPrint({
          a <- input$ptabla_promedios_2013
          b <- dataset2013_react()
          p <- b[,a]
          o <- summary(p)
          return(o)
        })
        
        output$cyb_2013 <- renderPlot({
          
          a <- input$ptabla_cyb_2013
          b <- dataset2013_react()
          c <- b[,a]
          b %>%
            ggplot(aes(x = a, y = unlist(c), fill = a)) + geom_boxplot() + 
            scale_fill_manual(values=c("olivedrab2"))+
            theme(
              legend.position="none",
              plot.title = element_text(size=11)
            ) +
            ggtitle("Diagrama de caja y bigotes") +
            xlab("")
        })
        
        plot2013 <- reactive({
          df <- dataset2013_react()
          col <- input$ptabla_cyb_2013
          p <-   ggplot(df, aes(0, df[,col])) +  geom_boxplot()
        })
        
        
        
        output$plot_cyb_2013 <- downloadHandler(
          filename = function() { paste(input$ptabla_cyb_2013, '.png', sep='') },
          content = function(file) {
            device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
            ggsave(file, plot = plot2013(), device = device)
          }
        )
        
        
        
        output$promedios_filtros_2013<-renderTable({
          a <- input$nivel_filtro
          b <- input$categoria_filtro
          
          data_2013 <- dataset2013_react()
          
          c <- data_2013[,a]
          d <- data_2013[,b]
          
          
          
          promedios_grupales <-aggregate(d, by=list(c), FUN = mean , na.rm = TRUE)
          # promedios_grupales <- aggregate(b, by=list(a), FUN = mean , na.rm = TRUE)
        }) 
        
        
        
        ########################################################################## 2015  ######################################################################
        
        output$tabla_d_c_generalizada_2015<-renderPrint({
          
          d <- input$ptabla2015_primeravx
          e <- input$ptabla2015_segundavx
          f <- input$ptabla2015_terceravx
          g <- input$ptabla2015_cuartavx
          
          ab <- dataset2015_react()
          
          
          a <- ab[,d]
          b <- ab[,e] 
          c <- ab[,f] 
          d <- ab[,g] 
          
          cross_tab = table(a, b, c, d)
          
          return(cross_tab)
        })
        
        
        
        
        
        output$tabla_d_c_generalizada_2015_pon<-renderPrint({
          
          d <- input$ptabla2015_primeravx
          e <- input$ptabla2015_segundavx
          f <- input$ptabla2015_terceravx
          g <- input$ptabla2015_cuartavx
          
          ab <- dataset2015_react()
          
          a <- ab[,d]
          b <- ab[,e] 
          c <- ab[,f] 
          d <- ab[,g] 
          
          cross_tab = xtabs(ab$expc ~ unlist(a) + unlist(b)+unlist(c)+unlist(d),aggregate(ab$expc ~ unlist(a)+unlist(b)+unlist(c)+unlist(d),ab,mean))
          
          return(cross_tab)
        })
        
        
        output$tabla_2015_csv <- downloadHandler(
          filename = function() {
            paste("ttcc_2015.csv", "csv", sep=".")
          },
          content = function(file) {
            
            d <- input$ptabla2015_primeravx
            e <- input$ptabla2015_segundavx
            f <- input$ptabla2015_terceravx
            g <- input$ptabla2015_cuartavx
            
            ab <- dataset2015_react()
            
            a <- ab[,d]
            b <- ab[,e] 
            c <- ab[,f] 
            d <- ab[,g] 
            
            cross_tab = table(a, b, c, d)
            
            write.csv(cross_tab, file)
          }
        )
        
        output$tabla_2015_csv_pon <- downloadHandler(
          filename = function() {
            paste("ttcc_2015_pon.csv", "csv", sep=".")
          },
          content = function(file) {
            
            
            d <- input$ptabla2015_primeravx
            e <- input$ptabla2015_segundavx
            f <- input$ptabla2015_terceravx
            g <- input$ptabla2015_cuartavx
            
            ab <- dataset2015_react()
            
            a <- ab[,d]
            b <- ab[,e] 
            c <- ab[,f] 
            d <- ab[,g] 
            
            cross_tab = xtabs(ab[,578] ~ unlist(a) + unlist(b)+unlist(c)+unlist(d),aggregate(ab[,578]  ~ unlist(a)+unlist(b)+unlist(c)+unlist(d),ab,mean))
            
            write.csv(cross_tab, file)
            
          }
        )
        
        
        
        output$promedios_2015<-renderPrint({
          a <- input$ptabla_promedios_2015
          b <- dataset2015_react()
          p <- b[,a]
          o <- summary(p)
          return(o)
        })
        
        output$cyb_2015 <- renderPlot({
          
          a <- input$ptabla_cyb_2015
          b <- dataset2015_react()
          c <- b[,a]
          b %>%
            ggplot(aes(x = a, y = unlist(c), fill = a)) + geom_boxplot() + 
            scale_fill_manual(values=c("olivedrab2"))+
            theme(
              legend.position="none",
              plot.title = element_text(size=11)
            ) +
            ggtitle("Diagrama de caja y bigotes") +
            xlab("")
        })
        
        plot2015 <- reactive({
          df <- dataset2015_react()
          col <- input$ptabla_cyb_2015
          p <-   ggplot(df, aes(0, df[,col])) +  geom_boxplot()
        })
        
        
        
        output$plot_cyb_2015 <- downloadHandler(
          filename = function() { paste(input$ptabla_cyb_2015, '.png', sep='') },
          content = function(file) {
            device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
            ggsave(file, plot = plot2015(), device = device)
          }
        )
        
        
        
        output$promedios_filtros_2015<-renderTable({
          a <- input$nivel_filtro
          b <- input$categoria_filtro
          
          data_2015 <- dataset2015_react()
          
          c <- data_2015[,a]
          d <- data_2015[,b]
          
          
          
          promedios_grupales <-aggregate(d, by=list(c), FUN = mean , na.rm = TRUE)
          # promedios_grupales <- aggregate(b, by=list(a), FUN = mean , na.rm = TRUE)
        }) 
        
    ########################################################################## 2009 mn  ######################################################################
    
    
    
    output$cyb_2009_mn <- renderPlot({
        a9mn <- input$ptabla_cyb_2009_mn
        
        preguntaseternas2001_chi <- mydata_educacion_3000()
        preguntaseternas_sub2001_a9mn <- preguntaseternas2001_chi[,a9mn]
        
        preguntaseternas2001_chi %>%
            
            ggplot( aes(x = a9mn, y = unlist(preguntaseternas_sub2001_a9mn), fill = a9mn)) +
            
            
            
            
            geom_boxplot() +
            
            theme(
                legend.position="none",
                plot.title = element_text(size=11)
            ) +
            ggtitle("Basic boxplot") +
            xlab("")
        
        
    })
    
    output$promedios_2009_mn<-renderPrint({
        a9mn <- input$ptabla_promedios_2009_mn
        preguntaseternas2001_chi <- mydata_educacion_3000()
        preguntaseternas_sub2001_a9mn <- preguntaseternas2001_chi[,a9mn]
        b <- summary(preguntaseternas_sub2001_a9mn)
        return(b)
        
    })
    
    
    
    ########################################################################## 2013  ##########################################################################  
    
    
    output$tabla_d_c15<-renderPrint({
        a <- input$ptabla2015_primerav
        b <- input$ptabla2015_segundav
        preguntaseternas2001_ab <- mydata_educacion_6000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,a]
        preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,b] 
        cross_tab15 = xtabs(~ unlist(preguntaseternas_sub2001_a) + unlist(preguntaseternas_sub2001_b), preguntaseternas2001_ab)
        return(cross_tab15)
    })
    
    output$tabla_chi15<-renderPrint({
        a <- input$ptabla2015_primerav
        b <- input$ptabla2015_segundav
        preguntaseternas2001_ab <- mydata_educacion_6000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,a]
        preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,b] 
        cross_tab15 = xtabs(~ unlist(preguntaseternas_sub2001_a) + unlist(preguntaseternas_sub2001_b), preguntaseternas2001_ab)
        chicuadrado15 <- chisq.test(cross_tab15)
        return(chicuadrado15)
    })
    
    
    ########################################################################## 2015  ##########################################################################  
    
    
    output$tabla_d_c13<-renderPrint({
        a <- input$ptabla2013_primerav
        b <- input$ptabla2013_segundav
        preguntaseternas2001_ab <- mydata_educacion_7000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,a]
        preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,b] 
        cross_tab13 = xtabs(~ unlist(preguntaseternas_sub2001_a) + unlist(preguntaseternas_sub2001_b), preguntaseternas2001_ab)
        return(cross_tab13)
    })
    
    output$tabla_chi13<-renderPrint({
        a <- input$ptabla2013_primerav
        b <- input$ptabla2013_segundav
        preguntaseternas2001_ab <- mydata_educacion_7000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,a]
        preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,b] 
        cross_tab13 = xtabs(~ unlist(preguntaseternas_sub2001_a) + unlist(preguntaseternas_sub2001_b), preguntaseternas2001_ab)
        chicuadrado13 <- chisq.test(cross_tab13)
        return(chicuadrado13)
    })
    
    
    ########################################################################## 2017  ##########################################################################  

    output$tabla_d_c_generalizada<-renderPrint({
        #output$tabla_d_c_generalizada<-renderTable({
        d <- input$ptabla2017_primeravx
        e <- input$ptabla2017_segundavx
        f <- input$ptabla2017_terceravx
        g <- input$ptabla2017_cuartavx
        
        
        preguntaseternas2001_ab <- mydata_educacion_exp()
        
        
        preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,d]
        preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,e] 
        preguntaseternas_sub2001_c <- preguntaseternas2001_ab[,f] 
        preguntaseternas_sub2001_d <- preguntaseternas2001_ab[,g] 

         cross_tab = table(preguntaseternas_sub2001_a, preguntaseternas_sub2001_b, preguntaseternas_sub2001_c, preguntaseternas_sub2001_d)
        
        return(cross_tab)
    })
    
    
    
    output$tabla_d_c_generalizada_pon<-renderPrint({

        d <- input$ptabla2017_primeravx
        e <- input$ptabla2017_segundavx
        f <- input$ptabla2017_terceravx
        g <- input$ptabla2017_cuartavx

        
        preguntaseternas2001_ab <- mydata_educacion_exp()
        
        
        preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,d]
        preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,e] 
        preguntaseternas_sub2001_c <- preguntaseternas2001_ab[,f] 
        preguntaseternas_sub2001_d <- preguntaseternas2001_ab[,g] 
        

          cross_tab = xtabs(preguntaseternas2001_ab$expc ~ unlist(preguntaseternas_sub2001_a) + unlist(preguntaseternas_sub2001_b)+unlist(preguntaseternas_sub2001_c)+unlist(preguntaseternas_sub2001_d),aggregate(preguntaseternas2001_ab$expc ~ unlist(preguntaseternas_sub2001_a)+unlist(preguntaseternas_sub2001_b)+unlist(preguntaseternas_sub2001_c)+unlist(preguntaseternas_sub2001_d),preguntaseternas2001_ab,mean))
          
          
          return(cross_tab)
    })
    
    
    
    # 
    # output$tabla_d_c_generalizada<-renderPrint({
    #     d <- input$ptabla2017_primeravx
    #     e <- input$ptabla2017_segundavx
    #     f <- input$ptabla2017_terceravx
    #     g <- input$ptabla2017_cuartavx
    #     
    # 
    #     preguntaseternas2001_ab <- mydata_educacion_exp()
    #     
    #     
    #     preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,d]
    #     preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,e] 
    #     preguntaseternas_sub2001_c <- preguntaseternas2001_ab[,f] 
    #     preguntaseternas_sub2001_d <- preguntaseternas2001_ab[,g] 
    #     
    #     
    #     # cross_tab = xtabs(~ unlist(preguntaseternas_sub2001_a) + unlist(preguntaseternas_sub2001_b), preguntaseternas2001_ab)
    #     
    #     cross_tab = table(preguntaseternas_sub2001_a, preguntaseternas_sub2001_b, preguntaseternas_sub2001_c, preguntaseternas_sub2001_d)
    #     
    # 
    #     return(cross_tab)
    # })
    
    output$exptabla_d_c_generalizada<-renderTable({
        d <- input$expptabla2017_primeravx
        e <- input$expptabla2017_segundavx
        f <- input$expptabla2017_terceravx
        g <- input$expptabla2017_cuartavx
        
        preguntaseternas2001_ab <- mydata_educacion_exp2()
        

       # tabla_cro <- cro(alerta$Sexo, alerta$"Estado civil")
        
        preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,d]
        preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,e] 
        preguntaseternas_sub2001_c <- preguntaseternas2001_ab[,f] 
        preguntaseternas_sub2001_d <- preguntaseternas2001_ab[,g] 
        
        preguntaseternas_sub2001_a  <-  data.frame( preguntaseternas_sub2001_a )

        preguntaseternas_sub2001_b  <-  data.frame( preguntaseternas_sub2001_b )
        #fff <- table(preguntaseternas_sub2001_a, preguntaseternas_sub2001_b)
        
        # *** buena
       fff <- cro(preguntaseternas_sub2001_a, preguntaseternas_sub2001_b)
       # fff <- cro(preguntaseternas_sub2001_a, list(total(), preguntaseternas_sub2001_b %nest% preguntaseternas_sub2001_c))
        
        #fff = xtabs(unlist(preguntaseternas_sub2001_a) ~ unlist(preguntaseternas_sub2001_b) + unlist(preguntaseternas_sub2001_c))
        
        #tabla_cro <- cro_cpct(preguntaseternas_sub2001_a, list(total(), preguntaseternas_sub2001_b, preguntaseternas_sub2001_c, preguntaseternas_sub2001_d))
        
        # aca debe venir la tabla para consumo de Patricio:
        # cross_tab = table(preguntaseternas_sub2001_a, preguntaseternas_sub2001_b, preguntaseternas_sub2001_c, preguntaseternas_sub2001_d)
        
        return(fff)
    })
    
    
    
    #########################################################  2013
    output$tabla_d_c_2013<-renderPrint({
      a <- input$ptabla2013_primerav
      b <- input$ptabla2013_segundav
      ab <- dataset2013_react()
      col_a <- ab[,a]
      col_b <- ab[,b] 
      cross_tab = xtabs(~ unlist(col_a) + unlist(col_b), ab)
      return(cross_tab)
    })
    
    #################
    output$tabla_d_c_ponderadas<-renderPrint({
      a <- input$ptabla2013_primerav
      b <- input$ptabla2013_segundav
      ab <- dataset2013_react()
      col_a <- ab[,a]
      col_b <- ab[,b] 
      cross_tab = xtabs(ab$expc ~ unlist(col_a) + unlist(col_b), ab)
      return(cross_tab)
    })
    
    ###########################################################
    ###########################################################
    ###########################################################
    ###########################################################
    

    
    #################
 
    
    ###################################################################################
    #######################################    16    ############################################   
    ###################################################################################   
    
    
    #################################################################### 2017
    output$tabla_d_c<-renderPrint({
      a <- input$ptabla2017_primerav
      b <- input$ptabla2017_segundav
        
        
        #dataset2013_react
        preguntaseternas2001_ab <- mydata_educacion_exp()
        preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,a]
        preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,b] 
        cross_tab = xtabs(~ unlist(preguntaseternas_sub2001_a) + unlist(preguntaseternas_sub2001_b), preguntaseternas2001_ab)
        return(cross_tab)
    })
    
    # output$tabla_d_c_ponderadas <-renderPrint({
    #     a <- input$ptabla2017_primerav
    #     b <- input$ptabla2017_segundav
    #     preguntaseternas2001_ab <- mydata_educacion_exp()
    #     preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,a]
    #     preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,b] 
    #     cross_tab = xtabs(expc ~ unlist(preguntaseternas_sub2001_a) + unlist(preguntaseternas_sub2001_b), preguntaseternas2001_ab)
    #     return(cross_tab)
    # })
    # 
    
    
    
    output$tabla_chi<-renderPrint({
        a <- input$ptabla2017_primerav_chi
        b <- input$ptabla2017_segundav_chi
        preguntaseternas2001_ab <- mydata_educacion_exp()
        preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,a]
        preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,b] 
        cross_tab = xtabs(~ unlist(preguntaseternas_sub2001_a) + unlist(preguntaseternas_sub2001_b), preguntaseternas2001_ab)
        chicuadrado <- chisq.test(cross_tab)
        return(chicuadrado)
    })
    
    

    

    plotInput666 <- reactive({
        a <- input$ptabla_cyb
        preguntaseternas2001_chi <- mydata_educacion_1000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_chi[,a]
        
       p <-  preguntaseternas2001_chi %>%
            ggplot(aes(x = a, y = unlist(preguntaseternas_sub2001_a), fill = a)) + geom_boxplot()
    })

   
        
        output$downloadPlotaaa<- downloadHandler(

            filename = function() {paste(a, '.png', sep='')},
            content = function(file){

                ggsave(file,plotInput666())
            }
        )
    
    
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            "la_data.csv"
        },
        content = function(file) {
            write.table(mydata_educacion_1000(), file)
        }
    )
    
    ################################################################################################################################3
    
    
    
    output$downloadPlot2009 <- downloadHandler(
        filename = function(){paste("input$plot3",'.png',sep='')},
        content = function(file){
            ggsave(file,plot=ggplot(mydata_educacion_2000(), aes(mydata_educacion_2000()$"Ingreso Del Trabajo")) + geom_density())})
    
    
    output$downloadData2009 <- downloadHandler(
        filename = function() {
            "la_data.csv"
        },
        content = function(file) {
            write.table(mydata_educacion_2000(), file)
        }
    )
    
    #######################################################################################
    ## Despliegue de las tablas de frecuencia por pregunta
    #######################################################################################
    
    
    ################################################################################################################################3
    output$prueba_tabla <- renderDataTable({
        
        if(input$variable_anio == 2006)
        {
            a <- dataset2006_react()
            b <- a[,input$ptabla_2006]
            c = table(b)
            t = as.data.frame(c)
        } 
        
        else if(input$variable_anio == 2009)
        {
            a <- dataset2009_react()
            b <- a[,input$ptabla_2009]
            c = table(b)
            t = as.data.frame(c)
        } 
        
      else if(input$variable_anio == 2011)
      {
        a <- dataset2011_react()
        b <- a[,input$ptabla_2011]
        c = table(b)
        t = as.data.frame(c)
      } 
      
        else if(input$variable_anio == 20111)
        {
            preguntaseternas20111<- mydata_educacion_5000()
            preguntaseternas_sub20111 <- preguntaseternas20111[,input$ptabla20111]
            w2005 = table(preguntaseternas_sub20111)
            t = as.data.frame(w2005)
        } 
        else if(input$variable_anio == 2013)
        {
            preguntaseternas2013<- mydata_educacion_6000()
            preguntaseternas_sub2013 <- preguntaseternas2013[,input$ptabla2013]
            w2006 = table(preguntaseternas_sub2013)
            t = as.data.frame(w2006)
        } 
        else if(input$variable_anio == 2015)
        {
            preguntaseternas2015<- mydata_educacion_7000()
            preguntaseternas_sub2015 <- preguntaseternas2015[,input$ptabla2015]
            w2006 = table(preguntaseternas_sub2015)
            t = as.data.frame(w2006)
        } 
        else if(input$variable_anio == 2017)
        {
            preguntaseternas2017<- mydata_educacion_8000()
            preguntaseternas_sub2017 <- preguntaseternas2017[,input$ptabla]
            w2017 = table(preguntaseternas_sub2017)
            t = as.data.frame(w2017)
            
            
            
            
        } 
    })
    ################################################################################################################################3
    output$prueba_tabla2 <- renderDataTable({
      
      if(input$variable_anio == 2017)
      {
        preguntaseternas2017 <- mydata_educacion_8000()
        preguntaseternas_sub2017 <- preguntaseternas2017[,input$ptabla2]
        w2001 = table(preguntaseternas_sub2017)
        t = as.data.frame(w2001)
      } 
    })
    
    ################################################################################################################################3
    ####### Despliegue de frecuencia de respuesta por modulo 2013-2015-2017 ##############
    
    
    output$frecuencias_I <- renderDataTable({
      
      if(input$variable_anio == 2013)
      {
        a <- dataset2013_react_1()
        b <- a[,input$ptabla_2013_I]
        c = table(b)
        t = as.data.frame(c)
      } 
    })
    
    output$frecuencias_II <- renderDataTable({
      
      if(input$variable_anio == 2013)
      {
        a <- dataset2013_react_2()
        b <- a[,input$ptabla_2013_II]
        c = table(b)
        t = as.data.frame(c)
      } 
    })
    
    output$frecuencias_III <- renderDataTable({
      
      if(input$variable_anio == 2013)
      {
        a <- dataset2013_react_3()
        b <- a[,input$ptabla_2013_III]
        c = table(b)
        t = as.data.frame(c)
      } 
    })
    
    output$frecuencias_IV <- renderDataTable({
      
      if(input$variable_anio == 2013)
      {
        a <- dataset2013_react_4()
        b <- a[,input$ptabla_2013_IV]
        c = table(b)
        t = as.data.frame(c)
      } 
    })
    
    ###################################################2015
    
    output$frecuencias_2015_I <- renderDataTable({
      
      if(input$variable_anio == 2015)
      {
        a <- dataset2015_react_1()
        b <- a[,input$ptabla_2015_I]
        c = table(b)
        t = as.data.frame(c)
      } 
    })
    
    output$frecuencias_2015_II <- renderDataTable({
      
      if(input$variable_anio == 2015)
      {
        a <- dataset2015_react_2()
        b <- a[,input$ptabla_2015_II]
        c = table(b)
        t = as.data.frame(c)
      } 
    })
    
    output$frecuencias_2015_III <- renderDataTable({
      
      if(input$variable_anio == 2015)
      {
        a <- dataset2015_react_3()
        b <- a[,input$ptabla_2015_III]
        c = table(b)
        t = as.data.frame(c)
      } 
    })
    
    output$frecuencias_2015_IV <- renderDataTable({
      
      if(input$variable_anio == 2015)
      {
        a <- dataset2015_react_4()
        b <- a[,input$ptabla_2015_IV]
        c = table(b)
        t = as.data.frame(c)
      } 
    })
    
    
    
    ############################################
    ############################################
    ############################################
    
    
    
    
    output$prueba_tabla3 <- renderDataTable({
      
      if(input$variable_anio == 2017)
      {
        preguntaseternas2017 <- mydata_educacion_8000()
        preguntaseternas_sub2017 <- preguntaseternas2017[,input$ptabla3]
        w2001 = table(preguntaseternas_sub2017)
        t = as.data.frame(w2001)
      } 
    })
    
    ################################################################################################################################3
    output$prueba_tabla4 <- renderDataTable({
      
      if(input$variable_anio == 2017)
      {
        preguntaseternas2017 <- mydata_educacion_8000()
        preguntaseternas_sub2017 <- preguntaseternas2017[,input$ptabla4]
        w2001 = table(preguntaseternas_sub2017)
        t = as.data.frame(w2001)
      } 
    })
}

options(warn = oldw)

# Correr la aplicacion
shinyApp(ui = ui, server = server)


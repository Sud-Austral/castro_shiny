
# Casen
# Victor Enamorado - Christian Castro
# 18 de Noviembre del 2020
  
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

#rsconnect::deployApp('C:/Users/usuario/Desktop/shinycasen1')

#dataset <- read.csv('Casen_no_humano.csv')


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


######################### 2011 pendiente ##########################

###################################### 2013 ############################################

dataset2013 <- read.csv('mydata2013_sub.csv')
dataset2013_col <- colnames(dataset2013)

data_2013_3_5 <- dataset2013[, c(4,6)]
data_2013_1_2_colnames <- colnames(data_2013_3_5)

data_2013_5_348 <- dataset2013[, 7:348]
data_2013_5_348_colnames <- colnames(data_2013_5_348)

########################################################################################













##############################################################################
##############################################################################
##############################################################################
##############################################################################

dataset2011 <- read.csv('mydata2011_sub.csv')
dataset2011 <- dataset2011[1:1000,]
dataset2011_col <- colnames(dataset2011)


##################   2017 ##############################


dataset2017 <-  read.csv('mydata2017_sub.csv')
datos_df_exp <- colnames(dataset2017)

########################################################








#alerta <- read_xlsx("casen_2017_mil.xlsx")

# alerta <- read_csv("Casen_no_humano2.csv")

#dataset = read_sav("Casen_no_humano.csv")

#muy importante:
#datos_df_exp_casen_2017_6 <- colnames(alerta)

data_2017 <- read_xlsx("casen_2017_mil.xlsx")
data_2017_colnames <- colnames(data_2017)

# data_2017_modulo_I <- data_2017[, 1:3]
data_2017_modulo_I <- data_2017
data_2017_modulo_I_colnames <- colnames(data_2017_modulo_I)
data_2017_modulo_II <- data_2017[,43:102]
data_2017_modulo_II_colnames <- colnames(data_2017_modulo_II)
data_2017_modulo_III <- data_2017[,103:151]
data_2017_modulo_III_colnames <- colnames(data_2017_modulo_III)
data_2017_modulo_IV <- data_2017[,152:304]
data_2017_modulo_IV_colnames <- colnames(data_2017_modulo_IV)



# datos_df_1000 <- cbind(casen2017_1, casen2017_2)
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
                titlePanel(h1("Manual de interpretación de variables y análisis estadísticos de la CASEN")),
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
                           tabPanel("Primer módulo: Registro Residentes", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("modulo_1_2006"))
                           )),
                           
                           tabPanel("Segundo módulo (E): Educacion", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("modulo_2_2006"))
                           )),
                           
                           tabPanel("Tercer módulo (O): Trabajo", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("modulo_3_2006"))
                           )),
                           
                           tabPanel("Cuarto módulo (Y): Ingresos", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("modulo_4_2006"))
                           )),
                           
                           tabPanel("Quinto módulo (S): Salud", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("modulo_5_2006"))
                           )),
                           
                           tabPanel("Sexto módulo (R): Identidades, redes y participación", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("modulo_6_2006"))
                           )),
                           
                           tabPanel("Séptimo módulo (V): Vivienda y Entorno", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("modulo_7_2006"))
                           )),
                           tabPanel("Octavo módulo (V): Vivienda y Entorno", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("modulo_8_2006"))
                           )),
                           "----",
                           "",
                           tabPanel("______Submódulo: Hogares carentes: hh_d_asis-hh_d_seg", fluidRow(
                               column(12, includeMarkdown("hh_d_asis.md"))
                           )),
                           
                           tabPanel(" ")),
                
                navbarMenu("Tabla de contingencia > 2x2",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           tabPanel("Tablas de contingencia > 2x2",fluidRow(
                              # selectInput("nada", "Identifique la variable:", c(data_2017_colnames)),
                               column(12,

                                      selectInput("p2006_primerav", "ingrese primera variable:", c(dataset2006_col)),
                                      selectInput("p2006_segundav", "ingrese segunda variable:", c(dataset2006_col)),
                                      selectInput("p2006_tercerav", "ingrese tercera variable:", c(dataset2006_col)),
                                      selectInput("p2006_cuartav", "ingrese cuarta variable:", c(dataset2006_col)),
                                      downloadButton("boton_ttcc_2006", "Descargar"),
                                      verbatimTextOutput("tabla_d_c_generalizada_2006") %>% withSpinner(type = 5, color = "#e6460b", size = 0.5),
                                      
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
                           tabPanel("Primer módulo: Registro Residentes", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("modulo_1_2009"))
                           )),
                           
                           tabPanel("Segundo módulo (E): Educacion", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("modulo_2_2009"))
                           )),
                           
                           tabPanel("Tercer módulo (O): Trabajo", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("modulo_3_2009"))
                           )),
                           
                           tabPanel("Cuarto módulo (Y): Ingresos", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("modulo_4_2009"))
                           )),
                           
                           tabPanel("Quinto módulo (S): Salud", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("modulo_5_2009"))
                           )),
                           
                           tabPanel("Sexto módulo (R): Identidades, redes y participación", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("modulo_6_2009"))
                           )),
                           
                           tabPanel("Séptimo módulo (V): Vivienda y Entorno", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("modulo_7_2009"))
                           )),
                           tabPanel("Octavo módulo (V): Vivienda y Entorno", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("modulo_8_2009"))
                           )),
                           "----",
                           "",
                           tabPanel("______Submódulo: Hogares carentes: hh_d_asis-hh_d_seg", fluidRow(
                               column(12, includeMarkdown("hh_d_asis.md"))
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
                tabPanel("Introducción",
                         fluidRow(column(9, includeMarkdown("about_intro.md")))),
                tabPanel("Despliegue de la tabla",
                         fluidRow(column(9, includeMarkdown("about_pobporin_vei.txt")),
                                  column(12,  dataTableOutput("table2009mn")))),
                
                
                navbarMenu("Módulos",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_modulos.txt")))),
                           "----",
                           "",
                           tabPanel("Primer módulo: Registro Residentes", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("table2017_I"))
                           )),
                           
                           tabPanel("Segundo módulo (E): Educacion", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("table2017_Iedu"))
                           )),
                           
                           tabPanel("Tercer módulo (O): Trabajo", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("table2017_Itrab"))
                           )),
                           
                           tabPanel("Cuarto módulo (Y): Ingresos", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("table2017_Iing"))
                           )),
                           
                           tabPanel("Quinto módulo (S): Salud", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("table2017_Isal"))
                           )),
                           
                           tabPanel("Sexto módulo (R): Identidades, redes y participación", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("table2017_Iid"))
                           )),
                           
                           tabPanel("Séptimo módulo (V): Vivienda y Entorno", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("table2017_Iviv"))
                           )),
                           "----",
                           "",
                           tabPanel("______Submódulo: Hogares carentes: hh_d_asis-hh_d_seg", fluidRow(
                               column(12, includeMarkdown("hh_d_asis.md"))
                           )),
                           
                           tabPanel(" ")),
                
                
                
                
                tabPanel("Frecuencias por preguntas", fluidRow(
                    column(12, includeMarkdown("about_educacion.md.txt")),
                    selectInput("ptabla20091", "prueba tabla:", c(datos_df_casen_2009_mil_mn_preg)),
                    column(12, dataTableOutput("prueba_tabla"))
                )),
                
                navbarMenu("Promedios filtrados por grupo",
                           tabPanel("a nivel social", fluidRow(
                             column(12, includeMarkdown("info_2006_prom.md")),
                             selectInput("nivel_filtro_mn", "Seleccione unidad social:", c(data_2009_filtros_terr_ddl)),
                             selectInput("categoria_filtro_mn", "Seleccione atributo:", c(data_2009_filtros_cat_ddl)),
                             column(12, tableOutput("promedios_filtros_mn"))
                           ))),
                
                navbarMenu("Estadísticas y gráficas",
                           tabPanel("Promedios", fluidRow(
                               column(12, includeMarkdown("info_2006_prom.md")),
                               selectInput("ptabla_promedios_2009_mn", "prueba tabla:", c(datos_df_casen_2009_mil_mn_preg)),
                               column(12, verbatimTextOutput("promedios_2009_mn"))
                           )),
                           
                           tabPanel("Diagrama de caja y bigotes", fluidRow(
                               column(12, includeMarkdown("info_2006_cyb.md")),
                               selectInput("ptabla_cyb_2009_mn", "prueba tabla:", c(datos_df_casen_2009_mil_mn_preg)),
                               column(12, plotOutput("cyb_2009_mn"))
                           ))
                           
                )
            )
        }
        
        # cuatro anio
        
        # else if (user == 20110){
        #     navbarPage(
        #         
        #         br(),
        #         tabPanel("Introducción",
        #                  fluidRow(column(9, includeMarkdown("about_intro.md")))),
        #         tabPanel("Despliegue de la tabla",
        #                  fluidRow(column(9, includeMarkdown("about_pobporin_vei.txt")),
        #                           column(12,  dataTableOutput("table2011mn")))),
        #         tabPanel("Frecuencias por preguntas", fluidRow(
        #             column(12, includeMarkdown("about_educacion.md.txt")),
        #             selectInput("ptabla20110", "prueba tabla:", c(datos_df_casen_2011_mil_mn_preg)),
        #             column(12, dataTableOutput("prueba_tabla"))
        #         )),
        #         
        #         navbarMenu("Promedios filtrados por grupo",
        #                    tabPanel("a nivel social", fluidRow(
        #                      column(12, includeMarkdown("info_2006_prom.md")),
        #                      selectInput("nivel_filtro_11mn", "Seleccione unidad social:", c(data_2011_filtros_terr_ddl)),
        #                      selectInput("categoria_filtro_11mn", "Seleccione atributo:", c(data_2011_filtros_cat_ddl)),
        #                      column(12, tableOutput("promedios_filtros_11mn"))
        #                    ))),
        #         
        #         navbarMenu("Estadísticas y gráficas",
        #                    tabPanel("Promedios", fluidRow(
        #                        column(12, includeMarkdown("info_2006_prom.md")),
        #                        selectInput("ptabla_2011", "prueba tabla:", c(datos_df_casen_2011_mil_mn_preg)),
        #                        column(12, verbatimTextOutput("promedios_2011"))
        #                    )),
        #                    
        #                    tabPanel("Diagrama de caja y bigotes", fluidRow(
        #                        column(12, includeMarkdown("info_2006_cyb.md")),
        #                        selectInput("ptabla_cyb_2011", "prueba tabla:", c(datos_df_casen_2011_mil_mn_preg)),
        #                        column(12, plotOutput("cyb_2011"))
        #                    ))
        #                    
        #         )
        #     )
        # }
        # 
        # else if (user == 20111){
        #     navbarPage(
        #         
        #         br(),
        #         tabPanel("Introducción",
        #                  fluidRow(column(9, includeMarkdown("about_intro.md")))),
        #         tabPanel("Despliegue de la tabla",
        #                  fluidRow(column(9, includeMarkdown("about_pobporin_vei.txt")),
        #                           column(12,  dataTableOutput("table20ymt")))),
        #         tabPanel("Frecuencias por preguntas", fluidRow(
        #             column(12, includeMarkdown("about_educacion.md.txt")),
        #             selectInput("ptabla20111", "prueba tabla:", c(datos_df_casen_2011_mil_ymt_preg)),
        #             column(12, dataTableOutput("prueba_tabla"))
        #         )),
        #         navbarMenu("Estadísticas y gráficas",
        #                    tabPanel("Promedios", fluidRow(
        #                        column(12, includeMarkdown("info_2006_prom.md")),
        #                        selectInput("ptabla_2011_ymt", "prueba tabla:", c(datos_df_casen_2011_mil_ymt_preg)),
        #                        column(12, verbatimTextOutput("promedios_2011_ymt"))
        #                    )),
        #                    
        #                    tabPanel("Diagrama de caja y bigotes", fluidRow(
        #                        column(12, includeMarkdown("info_2006_cyb.md")),
        #                        selectInput("ptabla_cyb_2011_ymt", "Seleccione variable:", c(datos_df_casen_2011_mil_ymt_preg)),
        #                        column(12, plotOutput("cyb_2011_ymt"))
        #                    ))
        #                    
        #         )
        #     )
        # }
        
        
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
                
                
                navbarMenu("Módulos",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_modulos.txt")))),
                           "----",
                           "",
                           tabPanel("Primer módulo: Registro Residentes", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt")),
                             column(12, dataTableOutput("modulo_1_2013"))
                           )),
                           
                           tabPanel("Segundo módulo: Educacion", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt")),
                             column(12, dataTableOutput("modulo_2_2013"))
                           )),
                           
                           tabPanel("Tercer módulo: Trabajo", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt")),
                             column(12, dataTableOutput("modulo_3_2013"))
                           )),
                           
                           tabPanel("Cuarto módulo: Ingresos", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt")),
                             column(12, dataTableOutput("modulo_4_2013"))
                           )),
                           
                           tabPanel("Quinto módulo: Salud", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt")),
                             column(12, dataTableOutput("modulo_5_2013"))
                           )),
                           
                           tabPanel("Sexto módulo: Identidades Redes y Participacion", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt")),
                             column(12, dataTableOutput("modulo_6_2013"))
                           )),
                           
                           tabPanel("Septimo módulo: Vivienda", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt")),
                             column(12, dataTableOutput("modulo_7_2013"))
                           )),
                           
                           tabPanel("Octavo módulo: Ingresos", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt")),
                             column(12, dataTableOutput("modulo_8_2013"))
                           )),
                           
                           tabPanel("Noveno módulo: Expansiones", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt")),
                             column(12, dataTableOutput("modulo_9_2013"))
                           )),
                           
                           tabPanel("Decimo módulo", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt")),
                             column(12, dataTableOutput("modulo_10_2013"))
                           )),
                           
                           tabPanel("Onceavo módulo: Pobreza", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt")),
                             column(12, dataTableOutput("modulo_11_2013"))
                           )),
                           
                           tabPanel("Doceavo módulo: Indicadores", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt")),
                             column(12, dataTableOutput("modulo_12_2013"))
                           )),
                           
                           
                           tabPanel(" ")),
                
                navbarMenu("Cálculos propios de los Indicadores Casen",
                           tabPanel("Introducción",
                                    fluidRow(column(9, includeMarkdown("about_intro_cc.txt"))
                                    )),
                           "----",
                           "",
                           
                           tabPanel("Variables",
                                    fluidRow(column(9, includeMarkdown("about_variables_cc.txt")),
                                             column(3,  tableOutput("contents8")))),
                           
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
                
                navbarMenu("Estadísticas",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           tabPanel("Tabla educacion", tableOutput("contents_educacion")),
                           tabPanel("Tabla trabajo", tableOutput("contents_trabajo")),
                           "----",
                           "",
                           tabPanel("Diagramas de Caja y bigotes y de Densidad para la variable Edad", plotOutput("plot1")),
                           tabPanel("  ")),
                
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
                             column(7,
                                    selectInput("ptabla2013_primeravx", "ingrese primera variable:", c(dataset2013_col)),
                                    selectInput("ptabla2013_segundavx", "ingrese segunda variable:", c(dataset2013_col)),
                                    selectInput("ptabla2013_terceravx", "ingrese tercera variable:", c(dataset2013_col)),
                                    selectInput("ptabla2013_cuartavx", "ingrese cuarta variable:", c(dataset2013_col)),
                                    
                                    downloadButton("boton_ttcc_mayor_2_2013", "Descargar"),
                                    verbatimTextOutput("tabla_d_c_generalizada_2013") %>% withSpinner(color="#c50d78"),
                                    
                                    downloadButton("boton_ttcc_mayor_2_2013_pon", "Descargar"),
                                    verbatimTextOutput("tabla_d_c_generalizada_2013_pon") %>% withSpinner(color="#0e8c0e")
                                    
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
                
                navbarMenu("Promedios agrupados por categoría",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           tabPanel("Promedios agrupados por categoría",fluidRow(column(12, includeMarkdown("info_papc.md")),
                                                                                 column(12,
                                                                                        selectInput("primero_papc_2013", "ingrese primera variable:", c(datos_df_exp)),
                                                                                        selectInput("segundo_papc_2013", "ingrese segunda variable:", c(datos_df_exp)),
                                                                                        
                                                                                        downloadButton("boton_tabla_papc_2013", "Descargar"),
                                                                                        
                                                                                        tableOutput("tabla_papc_2013"))))
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
                
                tabPanel("Introducción",
                         fluidRow(column(9, includeMarkdown("about_intro.md")))),
                
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
                
                navbarMenu("Módulos",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_modulos.txt")))),
                           "----",
                           "",
                           tabPanel("Primer módulo", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               selectInput("ptabla2013", "prueba tabla:", c(datos_df_casen_2013_mil_preg)),
                               column(12, dataTableOutput("prueba_tabla"))
                           )),
                           tabPanel("Segundo módulo (E): Educacion ",
                                    fluidRow(column(6, includeMarkdown("about_educacion.md.txt")),
                                             column(3,  tableOutput("contents2")))),
                           
                           tabPanel("Tercer módulo (O): Trabajo ",
                                    fluidRow(column(6, includeMarkdown("about_trabajo.txt")),
                                             column(3,  tableOutput("contents3")))),
                           
                           tabPanel("Cuarto módulo (Y): Ingresos ",
                                    fluidRow(column(6, includeMarkdown("about_ingresos.txt")),
                                             column(3,  tableOutput("contents4")))),
                           
                           tabPanel("Quinto módulo (S): Salud ",
                                    fluidRow(column(6, includeMarkdown("about_salud.txt")),
                                             column(3,  tableOutput("contents5")))),
                           
                           
                           tabPanel("Sexto módulo (R): Identidades, redes y participación ",
                                    fluidRow(column(6, includeMarkdown("about_identidades.txt")),
                                             column(3,  tableOutput("contents6")))),
                           
                           tabPanel("Séptimo módulo (V): Vivienda y Entorno ",
                                    fluidRow(column(6, includeMarkdown("about_vivienda.txt")),
                                             column(3,  tableOutput("contents7")))),
                           
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
                
                navbarMenu("Estadísticas",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           tabPanel("Tabla educacion", tableOutput("contents_educacion")),
                           tabPanel("Tabla trabajo", tableOutput("contents_trabajo")),
                           "----",
                           "",
                           tabPanel("Diagramas de Caja y bigotes y de Densidad para la variable Edad", plotOutput("plot1")),
                           tabPanel("  ")),
                
                navbarMenu("Tablas de contingencia",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           
                           
                           selectInput("ptabla2015_primerav", "ingrese primera variable:", c()),
                           selectInput("ptabla2015_segundav", "ingrese segunda variable:", c()),
                           
                           tabPanel("Tablas de contingencia de 2x2",fluidRow(column(5, verbatimTextOutput("tabla_d_c15")))),
                           
                           tabPanel("Pearson's Chi-squared test",fluidRow(column(3, verbatimTextOutput("tabla_chi15"))))

                           
                            ) ,
                
                
                
                
                navbarMenu("Tablas de contingencia > 2x2",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           
                           
                           
                           
                           
                           tabPanel("Tablas de contingencia > 2x2",fluidRow(
                               selectInput("nada", "Identifique la variable:", c(data_2017_colnames)),
                               column(7,
                                      selectInput("ptabla2015_primeravx", "ingrese primera variable:", c(datos_df_exp)),
                                      selectInput("ptabla2015_segundavx", "ingrese segunda variable:", c(datos_df_exp)),
                                      selectInput("ptabla2015_terceravx", "ingrese tercera variable:", c(datos_df_exp)),
                                      
                                      selectInput("ptabla2015_cuartavx", "ingrese cuarta variable:", c(datos_df_exp)),
                                      
                                      downloadButton("boton_ttcc_mayor_2015", "Descargar"),
                                      #   tableOutput("tabla_d_c_generalizada") %>% withSpinner(color="#0dc5c1")))),
                                      verbatimTextOutput("tabla_d_c_generalizada_2015") %>% withSpinner(color="#0dc5c1")))),
                           
                           tabPanel("Cochran–Mantel–Haenszel",fluidRow(column(12,
                                                                              selectInput("ptabla2015_primerav", "ingrese primera variable:", c(datos_df_exp)),
                                                                              selectInput("ptabla2015_segundav", "ingrese segunda variable:", c(datos_df_exp)),
                                                                              selectInput("ptabla2015_tercerav", "ingrese tercera variable:", c(datos_df_exp)),
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
                
                navbarMenu("Promedios agrupados por categoría",
                           #    tabPanel("Tabla residentes", tableOutput("table_educacion_1000")),
                           
                           tabPanel("Promedios agrupados por categoría",fluidRow(column(12, includeMarkdown("info_papc.md")),
                                                                                 column(12,
                                                                                        selectInput("primero_papc_2015", "ingrese primera variable:", c(datos_df_exp)),
                                                                                        selectInput("segundo_papc_2015", "ingrese segunda variable:", c(datos_df_exp)),
                                                                                        
                                                                                        downloadButton("boton_tabla_papc_2015", "Descargar"),
                                                                                        
                                                                                        tableOutput("tabla_papc_2015"))))
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
                
                tabPanel("Introducción",
                         fluidRow(column(9, includeMarkdown("about_intro.md")))),
                
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
                
                
                
                
                
                navbarMenu("Módulos",
                           tabPanel("Introducción", fluidRow(column(9, includeMarkdown("intro_modulos.txt")))),
                           "----",
                           "",
                           tabPanel("Primer módulo: Registro Residentes", fluidRow(
                               column(12, includeMarkdown("about_educacion.md.txt")),
                               column(12, dataTableOutput("table2017_I"))
                           )),
                           
                           tabPanel("Segundo módulo (E): Educacion", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt")),
                             column(12, dataTableOutput("table2017_Iedu"))
                           )),
                           
                           tabPanel("Tercer módulo (O): Trabajo", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt")),
                             column(12, dataTableOutput("table2017_Itrab"))
                           )),
                           
                           tabPanel("Cuarto módulo (Y): Ingresos", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt")),
                             column(12, dataTableOutput("table2017_Iing"))
                           )),
                           
                           tabPanel("Quinto módulo (S): Salud", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt")),
                             column(12, dataTableOutput("table2017_Isal"))
                           )),
                           
                           tabPanel("Sexto módulo (R): Identidades, redes y participación", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt")),
                             column(12, dataTableOutput("table2017_Iid"))
                           )),
                           
                           tabPanel("Séptimo módulo (V): Vivienda y Entorno", fluidRow(
                             column(12, includeMarkdown("about_educacion.md.txt")),
                             column(12, dataTableOutput("table2017_Iviv"))
                           )),
                           "----",
                           "",
                           tabPanel("______Submódulo: Hogares carentes: hh_d_asis-hh_d_seg", fluidRow(
                               column(12, includeMarkdown("hh_d_asis.md"))
                           )),
                           
                           tabPanel(" ")),
                
                navbarMenu("Cálculos propios de los Indicadores Casen",
                           tabPanel("Introducción",
                                    fluidRow(column(9, includeMarkdown("about_intro_cc.txt"))
                                    )),
                           "----",
                           "",
                           
                           tabPanel("Variables",
                                    fluidRow(column(9, includeMarkdown("about_variables_cc.txt")),
                                             column(3,  tableOutput("contents8")))),
                           
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
                
                navbarMenu("Estadísticas",
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
                
                
                ########################################################
                ########################################################
                #######################  16   #################################
                
                
                
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
                
                navbarMenu("Promedios agrupados por categoría",
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
    
    
    output$boton_ttcc_mayor_2_2013 <- downloadHandler(
      filename = function() {
        paste("tabla_ttcc_2013.csv", "csv", sep=".")
      },
      content = function(file) {
        
        d <- input$ptabla2013_primeravx
        e <- input$ptabla2013_segundavx
        f <- input$ptabla2013_terceravx
        g <- input$ptabla2013_cuartavx
        
        
        preguntaseternas2001_ab <- dataset2013_react()
        
        
        preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,d]
        preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,e] 
        preguntaseternas_sub2001_c <- preguntaseternas2001_ab[,f] 
        preguntaseternas_sub2001_d <- preguntaseternas2001_ab[,g] 
        
        cross_tab = xtabs(unlist(preguntaseternas_sub2001_a)~unlist(preguntaseternas_sub2001_b)+unlist(preguntaseternas_sub2001_c),aggregate(unlist(preguntaseternas_sub2001_a)~unlist(preguntaseternas_sub2001_b)+unlist(preguntaseternas_sub2001_c),preguntaseternas2001_ab,mean))
        
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
        data <- dataset2006[, 1:18]
        return(data)
    })
    
    dataset2006_react_2 <- reactive({
        data <- dataset2006[, 19:75]#educacion
        return(data)
    })
    
    
    dataset2006_react_3 <- reactive({
        data <- dataset2006[, 76:118]#trabajo
        return(data)
    })
    
    
    dataset2006_react_4 <- reactive({
        data <- dataset2006[, 119:145]#ingresos
        return(data)
    })
    
    
    dataset2006_react_5 <- reactive({
        data <- dataset2006[, 146:232]#salud
        return(data)
    })
    
    
    dataset2006_react_6 <- reactive({
        data <- dataset2006[, 233:287]#residentes
        return(data)
    })
    
    
    dataset2006_react_7 <- reactive({
        data <- dataset2006[, 288:373]#vivienda
        return(data)
    })
    
    dataset2006_react_8 <- reactive({
        data <- dataset2006[, 374:576]#ingresos
        return(data)
    })
    
    dataset2006_react_9 <- reactive({
        data <- dataset2006[, 577:578]#Expansiones
        return(data)
    })
    
    dataset2006_react_10 <- reactive({
        data <- dataset2006[, 579:582]#Informacion
        return(data)
    })
    
    dataset2006_react_11 <- reactive({
        data <- dataset2006[, 583:588]#Informacion
        return(data)
    })
    
    dataset2006_react_12 <- reactive({
        data <- dataset2006[, 589:600]#Indicadores
        return(data)
    })
    
    ###############################################   2009 #################################
    
    # El objeto sin subindice representa a la totalidad:
    dataset2009_react <- reactive({
        data <- dataset2009
        return(data)
    })
    
    dataset2009_react_1 <- reactive({
        data <- dataset2009[, 1:18]
        return(data)
    })
    
    dataset2009_react_2 <- reactive({
        data <- dataset2009[, 19:75]#educacion
        return(data)
    })
    
    
    dataset2009_react_3 <- reactive({
        data <- dataset2009[, 76:118]#trabajo
        return(data)
    })
    
    
    dataset2009_react_4 <- reactive({
        data <- dataset2009[, 119:145]#ingresos
        return(data)
    })
    
    
    dataset2009_react_5 <- reactive({
        data <- dataset2009[, 146:232]#salud
        return(data)
    })
    
    
    dataset2009_react_6 <- reactive({
        data <- dataset2009[, 233:287]#residentes
        return(data)
    })
    
    
    dataset2009_react_7 <- reactive({
        data <- dataset2009[, 288:373]#vivienda
        return(data)
    })
    
    dataset2009_react_8 <- reactive({
        data <- dataset2009[, 374:576]#ingresos
        return(data)
    })
    
    dataset2009_react_9 <- reactive({
        data <- dataset2009[, 577:578]#Expansiones
        return(data)
    })
    
    dataset2009_react_10 <- reactive({
        data <- dataset2009[, 579:582]#Informacion
        return(data)
    })
    
    dataset2009_react_11 <- reactive({
        data <- dataset2009[, 583:588]#Informacion
        return(data)
    })
    
    dataset2009_react_12 <- reactive({
        data <- dataset2009[, 589:600]#Indicadores
        return(data)
    })
    
    ###############################################   2011 #################################
    
    
    #la base de datos total:
    dataset2011_react <- reactive({
        data <- dataset2011
        return(data)
    })
    
    dataset2011_react_1 <- reactive({
        data <- dataset2011[, 1:18]
        return(data)
    })
    
    dataset2011_react_2 <- reactive({
        data <- dataset2011[, 19:75]#educacion
        return(data)
    })
    
    
    dataset2011_react_3 <- reactive({
        data <- dataset2011[, 76:118]#trabajo
        return(data)
    })
    
    
    dataset2011_react_4 <- reactive({
        data <- dataset2011[, 119:145]#ingresos
        return(data)
    })
    
    
    dataset2011_react_5 <- reactive({
        data <- dataset2011[, 146:232]#salud
        return(data)
    })
    
    
    dataset2011_react_6 <- reactive({
        data <- dataset2011[, 233:287]#residentes
        return(data)
    })
    
    
    dataset2011_react_7 <- reactive({
        data <- dataset2011[, 288:373]#vivienda
        return(data)
    })
    
    dataset2011_react_8 <- reactive({
        data <- dataset2011[, 374:576]#ingresos
        return(data)
    })
    
    dataset2011_react_9 <- reactive({
        data <- dataset2011[, 577:578]#Expansiones
        return(data)
    })
    
    dataset2011_react_10 <- reactive({
        data <- dataset2011[, 579:582]#Informacion
        return(data)
    })
    
    dataset2011_react_11 <- reactive({
        data <- dataset2011[, 583:588]#Informacion
        return(data)
    })
    
    dataset2011_react_12 <- reactive({
        data <- dataset2011[, 589:600]#Indicadores
        return(data)
    })
    
    
    
    
    
    ###############################################   2013 #################################
    
    #la base de datos total:
    dataset2013_react <- reactive({
      data <- dataset2013
      return(data)
    })
    
    dataset2013_react_1 <- reactive({
      data <- dataset2013[, 1:18]
      return(data)
    })
    
    dataset2013_react_2 <- reactive({
      data <- dataset2013[, 19:75]#educacion
      return(data)
    })
    
    
    dataset2013_react_3 <- reactive({
      data <- dataset2013[, 76:118]#trabajo
      return(data)
    })
    
    
    dataset2013_react_4 <- reactive({
      data <- dataset2013[, 119:145]#ingresos
      return(data)
    })
    
    
    dataset2013_react_5 <- reactive({
      data <- dataset2013[, 146:232]#salud
      return(data)
    })
    
    
    dataset2013_react_6 <- reactive({
      data <- dataset2013[, 233:287]#residentes
      return(data)
    })
    
    
    dataset2013_react_7 <- reactive({
      data <- dataset2013[, 288:373]#vivienda
      return(data)
    })
    
    dataset2013_react_8 <- reactive({
      data <- dataset2013[, 374:576]#ingresos
      return(data)
    })
    
    dataset2013_react_9 <- reactive({
      data <- dataset2013[, 577:578]#Expansiones
      return(data)
    })
    
    dataset2013_react_10 <- reactive({
      data <- dataset2013[, 579:582]#Informacion
      return(data)
    })
    
    dataset2013_react_11 <- reactive({
      data <- dataset2013[, 583:588]#Informacion
      return(data)
    })
    
    dataset2013_react_12 <- reactive({
      data <- dataset2013[, 589:600]#Indicadores
      return(data)
    })
    
    ###############################################   2013
    
    
    
    
    
    
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
    
    output$table_2013 <- renderDataTable(dataset2013_react())
    
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
    
    
    ########################################################################## 2009 modulos  ##########################################################################  
    output$modulo_1_2009 <- renderDataTable(dataset2009_react_1())
    output$modulo_2_2009 <- renderDataTable(dataset2009_react_2())
    output$modulo_3_2009 <- renderDataTable(dataset2009_react_3())
    output$modulo_4_2009 <- renderDataTable(dataset2009_react_4())
    output$modulo_5_2009 <- renderDataTable(dataset2009_react_5())
    output$modulo_6_2009 <- renderDataTable(dataset2009_react_6())
    output$modulo_7_2009 <- renderDataTable(dataset2009_react_7())
    output$modulo_8_2009 <- renderDataTable(dataset2009_react_8())
    output$modulo_9_2009 <- renderDataTable(dataset2009_react_9())
    output$modulo_10_2009 <- renderDataTable(dataset2009_react_10())
    output$modulo_11_2009 <- renderDataTable(dataset2009_react_11())
    output$modulo_12_2009 <- renderDataTable(dataset2009_react_12())
    
    
    ########################################################################## 2011 modulos  ##########################################################################  
    output$modulo_1_2011 <- renderDataTable(dataset2011_react_1())
    output$modulo_2_2011 <- renderDataTable(dataset2011_react_2())
    output$modulo_3_2011 <- renderDataTable(dataset2011_react_3())
    output$modulo_4_2011 <- renderDataTable(dataset2011_react_4())
    output$modulo_5_2011 <- renderDataTable(dataset2011_react_5())
    output$modulo_6_2011 <- renderDataTable(dataset2011_react_6())
    output$modulo_7_2011 <- renderDataTable(dataset2011_react_7())
    output$modulo_8_2011 <- renderDataTable(dataset2011_react_8())
    output$modulo_9_2011 <- renderDataTable(dataset2011_react_9())
    output$modulo_10_2011 <- renderDataTable(dataset2011_react_10())
    output$modulo_11_2011 <- renderDataTable(dataset2011_react_11())
    output$modulo_12_2011 <- renderDataTable(dataset2011_react_12())
    
    
    
    
    
    
    
    
    
    
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
    ########################################################################## fin modulos 2013  ##########################################################################  
    
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
            #  ab <- dataset2006_react()
            
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
            
            # base_del_2006_terr <- base_del_2006[,b]
            # base_del_2006_terr[is.na(base_del_2006_terr)] <- 0
            # base_del_2006_cat <- base_del_2006[,a]
            
            promedios_grupales <-aggregate(d, by=list(c), FUN = mean , na.rm = TRUE)
            # promedios_grupales <- aggregate(b, by=list(a), FUN = mean , na.rm = TRUE)
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
                #  ab <- dataset2006_react()
                
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
        

        
    ########################################################################## 2009 ymt  ######################################################################
    
    
    

    

    
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
    
    
    
    
    ########################################################################## 2011 mn  ######################################################################
    
    output$cyb_2011 <- renderPlot({
        a <- input$ptabla_cyb_2011
        
        preguntaseternas2001_chi <- mydata_educacion_4000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_chi[,a]
        
        preguntaseternas2001_chi %>%
            
            ggplot( aes(x = a, y = unlist(preguntaseternas_sub2001_a), fill = a)) +
            
            
            
            
            geom_boxplot() +
            
            theme(
                legend.position="none",
                plot.title = element_text(size=11)
            ) +
            ggtitle("Basic boxplot") +
            xlab("")
        
        
    })
    
    
    
    
    
    output$promedios_2011<-renderPrint({
        a <- input$ptabla_2011
        preguntaseternas2001_chi <- mydata_educacion_4000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_chi[,a]
        b <- summary(preguntaseternas_sub2001_a)
        return(b)
        
    })
    
    
    ########################################################################## 2011 ymt  ######################################################################
    
    output$cyb_2011_ymt <- renderPlot({
        a <- input$ptabla_cyb_2011_ymt
        
        preguntaseternas2001_chi <- mydata_educacion_5000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_chi[,a]
        
        preguntaseternas2001_chi %>%
            
            ggplot( aes(x = a, y = unlist(preguntaseternas_sub2001_a), fill = a)) +
            geom_boxplot() +
            theme(
                legend.position="none",
                plot.title = element_text(size=11)
            ) +
            ggtitle("Basic boxplot") +
            xlab("")
    })
    
    
    
    
    
    output$promedios_2011_ymt<-renderPrint({
        a <- input$ptabla_2011_ymt
        preguntaseternas2001_chi <- mydata_educacion_5000()
        preguntaseternas_sub2001_a <- preguntaseternas2001_chi[,a]
        b <- summary(preguntaseternas_sub2001_a)
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
    output$tabla_d_c_generalizada_2013<-renderPrint({
      d <- input$ptabla2013_primeravx
      e <- input$ptabla2013_segundavx
      f <- input$ptabla2013_terceravx
      g <- input$ptabla2013_cuartavx
      
      
      preguntaseternas2001_ab <- dataset2013_react()
      
      
      preguntaseternas_sub2001_a <- preguntaseternas2001_ab[,d]
      preguntaseternas_sub2001_b <- preguntaseternas2001_ab[,e] 
      preguntaseternas_sub2001_c <- preguntaseternas2001_ab[,f] 
      preguntaseternas_sub2001_d <- preguntaseternas2001_ab[,g] 
      
      cross_tab = xtabs(unlist(preguntaseternas_sub2001_a)~unlist(preguntaseternas_sub2001_b)+unlist(preguntaseternas_sub2001_c),aggregate(unlist(preguntaseternas_sub2001_a)~unlist(preguntaseternas_sub2001_b)+unlist(preguntaseternas_sub2001_c),preguntaseternas2001_ab,mean))
      return(cross_tab)
    })
    #################
    output$tabla_d_c_generalizada_2013_pon<-renderPrint({
      d <- input$ptabla2013_primeravx
      e <- input$ptabla2013_segundavx
      f <- input$ptabla2013_terceravx
      g <- input$ptabla2013_cuartavx
      
      
      preguntaseternas2001_ab <- dataset2013_react()
      
      
      a <- preguntaseternas2001_ab[,d]
      b <- preguntaseternas2001_ab[,e] 
      c <- preguntaseternas2001_ab[,f] 
      d <- preguntaseternas2001_ab[,g] 
      #eeee <- preguntaseternas2001_ab$expc
      #cross_tab = xtabs(eeee ~ unlist(preguntaseternas_sub2001_a)+unlist(preguntaseternas_sub2001_b)+unlist(preguntaseternas_sub2001_c)+unlist(preguntaseternas_sub2001_d),aggregate(expc ~ unlist(preguntaseternas_sub2001_a)~unlist(preguntaseternas_sub2001_b)+unlist(preguntaseternas_sub2001_c),preguntaseternas2001_ab,mean))
      cross_tab = xtabs(expc~unlist(a)+unlist(b)+unlist(c)+unlist(d),aggregate(expc~unlist(a)+unlist(b)+unlist(c)+unlist(d),preguntaseternas2001_ab,mean))
      
      #cross_tab = xtabs(elements$expc ~ unlist(elements$hacinamiento) + unlist(elements$comuna)+unlist(elements$sexo) + unlist(elements$ecivil), elements)
      return(cross_tab)
    })
    
    
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
        
        else if(input$variable_anio == 20091)
        {
            preguntas20091 <- mydata_educacion_3000()
            preguntaseternas_sub2002 <- preguntas20091[,input$ptabla20091]
            w2003 = table(preguntaseternas_sub2002)
            t3 = as.data.frame(w2003)
        } 
        
        else if(input$variable_anio == 20110)
        {
            preguntas20110 <- mydata_educacion_4000()
            preguntaseternas_sub20110 <- preguntas20110[,input$ptabla20110]
            w2004 = table(preguntaseternas_sub20110)
            t = as.data.frame(w2004)
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


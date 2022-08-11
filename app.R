options(encoding = "UTF-8")
options(scipen=999)
########################################################################################### 
#pacotes 
###########################################################################################
library(shiny)
library(ggplot2)
library(shinydashboard)
library(tidyverse)
library(htmltools)
library(stringr)
library(DT)
library(plotly)
library(reshape2)
library(deSolve)
library(mondate)
library(readxl)

########################################################################################### 
#Dados 
###########################################################################################
# base <- read_delim("bases/calculo_fila_geral_01.csv", 
# 		   delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
# 		   trim_ws = TRUE)

# 
# base <- read_excel("bases/calculo_fila_geral_01.xlsx")
# 
# base$tx_retorno <- base$solic_retorno / base$solic_total
# base$tx_falta <- base$faltas / base$solic_total
# 
# procedimento <- unique(base$nome_procedimento) 
# procedimento <- as.list(strsplit(procedimento, ","))
# names(procedimento) <- unique(base$nome_procedimento)
########################################################################################### 
#Módulos 
###########################################################################################
source("modulo_sistema_regulacao.R")

########################################################################################### 
#UI
###########################################################################################
###########################################################################################
dbHeader <- dashboardHeader(title = "SMS - Florianópolis", 
			    tags$li(a(href = 'http://www.pmf.sc.gov.br/entidades/saude/index.php?cms=salas+de+situacao&menu=4&submenuid=152',
			    	  icon("power-off"),
			    	  title = "Sair"),
			    	class = "dropdown"),
			    tags$li(a(href = 'http://www.pmf.sc.gov.br/entidades/saude/index.php?cms=salas+de+situacao&menu=4&submenuid=152',
			    	  tags$img(src = 'logo_geinfo.png',
			    	  	 title = "Gerência de Inteligência e Informação", height = "30px"),
			    	  style = "padding-top:10px; padding-bottom:10px;padding-left:30px, padding-right:30px"),
			    	class = "dropdown"))


ui <- dashboardPage(
	########################################################################################### 
	dbHeader,
	########################################################################################### 
	dashboardSidebar(
		########################################################################################### 
		sidebarMenu(
			menuItem("Projeções",tabName = "projecoes", icon = icon("dashboard")),
			menuItem("Instruções", icon = icon("question-circle"),
				 href = "https://github.com/lpgarcia18/projecao_fila_saude"),
			menuItem("Código-fonte", icon = icon("code"), 
				 href = "https://github.com/lpgarcia18/projecao_fila_saude/blob/main/app.R"),
			menuItem("Licença de Uso", icon = icon("cc"), 
				 href = "https://github.com/lpgarcia18/projecao_fila_saude/blob/main/LICENSE")
			
		)
	),
	########################################################################################### 
	dashboardBody(
		tags$head(tags$style(HTML('
                          /* logo */
                          .skin-blue .main-header .logo {
                          background-color: rgb(255,255,255); color: rgb(14, 59, 79);
                          font-weight: bold;font-size: 20px;text-align: Right;
                          }

                          /* logo when hovered */

                          .skin-blue .main-header .logo:hover {
                          background-color: rgb(255,255,255);
                          }


                          /* navbar (rest of the header) */
                          .skin-blue .main-header .navbar {
                          background-color: rgb(255,255,255);
                          }

                          /* main sidebar */
                          .skin-blue .main-sidebar {
                          background-color: rgb(14, 59, 79);
                          }
                          

                          /* active selected tab in the sidebarmenu */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                          background-color: rgb(14, 59, 79);
                          color: rgb(255,255,255);font-weight: bold;font-size: 18px;
                          }

                          /* other links in the sidebarmenu */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                          background-color: rgb(14, 59, 79);
                          color: rgb(255,255,255);font-weight: bold;
                          }

                          /* other links in the sidebarmenu when hovered */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                          background-color: rgb(147,181,198);color: rgb(14, 59, 79);font-weight: bold;
                          }

                          /* toggle button color  */
                          .skin-blue .main-header .navbar .sidebar-toggle{
                          background-color: rgb(255,255,255);color:rgb(14, 59, 79);
                          }

                          /* toggle button when hovered  */
                          .skin-blue .main-header .navbar .sidebar-toggle:hover{
                          background-color: rgb(147,181,198);color:rgb(14, 59, 79);
                          }
                          
                          /* body */
                          .content-wrapper, .right-side {
                          background-color: rgb(147,181,198);
                         

#                           '))),
		tags$style(".small-box.bg-blue { background-color: rgb(18, 34, 59) !important; color: rgb(255,255,255) !important; };"
		),
		tags$style(".fa-check {color:#B5500C}"),
		tags$style(".fa-check-double {color:#B5500C}"),
		
		tabItems(
			########################################################################################### 
			#Proejeção de tempo
			###########################################################################################
			tabItem(tabName = "projecoes", h3("Projeção de Demanda por Procedimentos Especializados"),
				br(),
				hr(),
				br(),
				tabPanel("Projeção de Demanda",
					 sistema_regulacao_UI(id = "proj_demanda", 
					 		     base_serie = base_serie, 
					 		     base_situacao = base_situacao,
					 		     grupo_choice = grupo_demanda, 
					 		     procedimento_choice = procedimento_demanda,
					 		     tipo_proj_choice = tipo_proj)
				)
			)
		)
	)
)



########################################################################################### 
server <- function(input, output, session) {
	###########################################################################################
	
	sistema_regulacao_SERV(id = "proj_demanda", base_situacao, base_serie)
	
}

#Analisar o que tem demanda reprimida e capacidade sobrando (precisa dos dados do número de profissionais)


###########################################################################################
#Aplicação
###########################################################################################
shinyApp(ui, server)
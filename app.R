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
library(readr)
########################################################################################### 
#Módulos 
###########################################################################################
source("modulo_serie_temporal.R")

########################################################################################### 
#Dados 
###########################################################################################
# base <- read_delim("bases/calculo_fila_geral_01.csv", 
# 		   delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
# 		   trim_ws = TRUE)


base <- read_csv("bases/base_procedimento.csv")
proced_unico <- unique(base$procedimento) 
proced_unico <- sort(proced_unico)
proced_unico <- as.list(strsplit(proced_unico, ","))
names_proced_unico <- unique(base$procedimento)
names_proced_unico <- sort(names_proced_unico)
names(proced_unico) <- names_proced_unico
grupo_unico <- unique(base$grupo) 
grupo_unico <- sort(grupo_unico)
grupo_unico <- as.list(strsplit(grupo_unico, ","))
names_grupo_unico <- unique(base$grupo)
names_grupo_unico <- sort(names_grupo_unico)
names(grupo_unico) <- names_grupo_unico
min(base$mes_solicitacao)

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
			menuItem("Projeções",tabName = "projecao", icon = icon("dashboard")),
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
		tags$style(".box { background-color: rgb(255, 255, 255) !important; color: rgb(0, 0, ) !important; };"),
		
		tabItems(
			###########################################################################################
			#projeções
			###########################################################################################
			tabItem(tabName = "projecao", h2("Projeção de Demanda por Procedimentos de Saúde"),
				tabPanel("Séries Temporais",
					 serie_temporal_UI(id = "projecao_demanda",
					 		  banco = base,
					 		  grupo_choice = grupo_unico, 
					 		  procedimento_choice = proced_unico)
				)
			)
		)
	)
)

########################################################################################### 
server <- function(input, output, session) {
	###########################################################################################
	serie_temporal_SERV(id = "projecao_demanda", banco = base)
	
}

###########################################################################################
#AplicaÃ§Ã£o
###########################################################################################
shinyApp(ui, server)
			
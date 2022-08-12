setwd(getwd())

#######################################################################
##Descrição
#######################################################################
#'Esse módulo permite ao usuário receber uma série temporal, realizar transformação box-cox e escolher um tempo para predição
#'mostrando os dados usados, a predição, o sumário do modelo escolhido pelo sistema (o sistema escolhe entre modelos arima
#'e ets, o com o menor Mean Squared Error - MSE), o diagnóstico e os dados.
#'
#######################################################################
##Pacotes
#######################################################################
require(shiny)
require(readr)
require(tidyverse)
require(zoo)
require(fpp2)
require(DT)
require(plotly)


########################################################################################### 
#Dados 
###########################################################################################
base_serie <- read_csv("bases/base_procedimento.csv")

base_situacao <- read_csv("bases/situacao_procedimento.csv")

base_situacao <-  base_situacao %>% 
	mutate_all(replace_na, 0)


procedimento_demanda <- unique(base_situacao$procedimento) 
procedimento_demanda <- as.list(strsplit(procedimento_demanda, ","))
names(procedimento_demanda) <- unique(base_situacao$procedimento)

grupo_demanda <- unique(base_situacao$grupo) 
grupo_demanda <- as.list(strsplit(grupo_demanda, ","))
names(grupo_demanda) <- unique(base_situacao$grupo)

municipio_demanda <- unique(base_situacao$municipio) 
municipio_demanda <- as.list(strsplit(municipio_demanda, ","))
names(municipio_demanda) <- unique(base_situacao$municipio)

tipo_proj <- list("Máxima", "Média", "Mínima")
names(tipo_proj) <- c("Máxima", "Média", "Mínima")
#######################################################################
##UI
#######################################################################
#A função UI deve entra como argumento de um tabPanel
sistema_regulacao_UI <- function(id, base_situacao, base_serie, grupo_choice, procedimento_choice, tipo_proj_choice){
	ns <- NS(id)
	tagList(
		fluidRow(
			column(width = 12,
			       tabBox(title = "Seleção de Parâmetros", width=12, height = 320,
			              column(width = 4, 
			                     box(width = 12,collapsible = F,  
			                         selectInput(
			                         	inputId=ns("grupo_input"),
			                         	label="Grupo", 
			                         	choices= grupo_demanda,
			                         	selected = grupo_demanda[1]))),
			              column(width = 4, 
			                     box(width = 12,collapsible = F,  
			                         selectInput(
			                         	inputId=ns("procedimento_input"),
			                         	label="Procedimento", 
			                         	choices= procedimento_demanda,
			                         	selected = procedimento_demanda[1]))),
			              column(width = 4, 
			                     box(width = 12,collapsible = F,  
			                         selectInput(
			                         	inputId=ns("municipio_input"),
			                         	label="Municipio", 
			                         	choices= municipio_demanda,
			                         	selected = "Total"))),
			              column(width = 4, 
			                     box(width = 12,collapsible = F, 
				              #Selecionando lambda
				              textInput(inputId = ns("lambda_banco"), 
				              	  label = "Transformação de Box-Cox(lambda):",
				              	  value = "NULO"),
				              helpText("Lambda = NULO, sem transforação.",
				              	 br(),
				              	 "Lambda = 0, transformação logarítimica."))),
			              column(width = 4, 
			                     box(width = 12,collapsible = F,
			                         selectInput(inputId = ns("tipo_proj_input"),
			                         	   label = "Tipo de Projeção",
			                         	   choices= tipo_proj,
			                         	   selected = tipo_proj[1]))),
				       column(width = 4, 
				              box(width = 12,collapsible = F,
				                  numericInput(inputId = ns("tempo_input"),
				                  	     label = "Tempo para Controle (Mês)",
				                  	     value = 12)))
				       ))),
		fluidRow(
			column(12,
			       tabBox(title = "Predição de Solicitações", width=12,
			       tabsetPanel(type = "tabs",
			       	    #Gráfico da série temporal
			       	    tabPanel("Previsão de Demanda - Gráfico",
			       	    	 plotOutput(outputId = ns("serie_banco"), width = "100%", height = 330)),
			       	    tabPanel("Previsão de Demanda - Dados",	 
			       	    	 DT::dataTableOutput(ns("dados_prev"), width = "100%", height = 660)),
			       	    tabPanel("Decomposição", 
			       	    	 plotOutput(outputId = ns("decomposicao_banco"), width = "100%", height = 660),
			       	    	 plotOutput(outputId = ns("sazonal_banco"), width = "100%", height = 330)),
			       	    tabPanel("Diagnóstico",
			       	    	 verbatimTextOutput(ns("summary_previsao_banco")), 
			       	    	 plotOutput(outputId = ns("residuos_banco"), width = "100%", height = 660)),
			       	    tabPanel("Dados de Produção",
			       	    	 DT::dataTableOutput(ns("dados_real"), width = "100%", height = 660)))))),
		fluidRow(column(width = 12,
				tabBox(title = "Parâmetros", width=12, height = 300,
				       column(width = 4, 
				              tableOutput(outputId = ns("table_demanda"))),
				       column(width = 4, 
				              plotlyOutput(outputId = ns("pessoas_fila"),height = 250)),
				       column(width = 4, 
				              plotlyOutput(outputId = ns("fila_demanda"),height = 250))))),
		fluidRow(column(width = 12,
				tabBox(title = "Projeções", width= 12, height = 200,
				       column(width = 6, 
				              valueBoxOutput(outputId = ns("controle_demanda"), width = 12)),
				       column(width = 6, 
				              valueBoxOutput(outputId = ns("manutencao_demanda"), width = 12)))))

	)
}

#################################################################################
#Server
#################################################################################

sistema_regulacao_SERV <- function(id, base_situacao, base_serie){
	moduleServer(
		id,
		function(input, output, session)
		{
			######################################################################################
			#Inputs
			######################################################################################	
			grupo_sel <- reactive({input$grupo_input})
			procedimento_sel <- reactive({input$procedimento_input})
			municipio_sel <- reactive({input$municipio_input})
			tempo_para_controle <- reactive({input$tempo_input})
			tipo_proj_sel <- reactive({input$tipo_proj_input})
			projecao <- 13
			
			######################################################################################
			#Projeções de Solicitações
			######################################################################################	
			
			banco_preparado_proj <- reactive({
				
				producao_select  <- base_serie %>%
					subset(grupo == grupo_sel() & procedimento == procedimento_sel() & municipio == municipio_sel())
				producao_select
			})
			
			
			inicio <- reactive({
				a <- banco_preparado_proj()$mes_solicitacao
				a <- min(a)
				a
			})
			
			banco_pre <- reactive({         
				banco_prep <- banco_preparado_proj() %>%
					dplyr::group_by(mes_solicitacao) %>%
					dplyr::summarise(solicitacao = sum(solicitacao, na.rm = T))
				banco_prep <- banco_prep$solicitacao
				banco_prep <- ts(banco_prep, start = c(as.numeric(format(inicio(), format = "%Y")), as.numeric(format(inicio(), format = "%m"))), frequency = 12)
				banco_prep 
			})        
			
			
			
			lambda <-reactive({
				req(input$lambda_banco)
			})     
			
			
			banco_prev <-reactive({
				
				#Função para ETS
				ifelse(input$lambda_banco == "NULO",
				       funcets <- ets(banco_pre(), lambda = NULL),
				       funcets <- ets(banco_pre(), lambda = input$lambda_banco))
				fets <- function(x, h) {
					forecast(funcets, h = projecao)
				}
				
				##Função para ARIMA
				ifelse(input$lambda_banco == "NULO",
				       funcarima<- auto.arima(banco_pre(), lambda = NULL),
				       funcarima<- auto.arima(banco_pre(), lambda = input$lambda_banco))
				farima <- function(x, h) {
					forecast(funcarima, h = projecao)
				}
				
				## Compute CV errors for ETS as e1
				e1 <- tsCV(banco_pre(), fets, h=projecao)
				
				## Compute CV errors for ARIMA as e2
				e2 <- tsCV(banco_pre(), farima, h=projecao)
				
				## Find MSE of each model class
				g<-mean(e1^2, na.rm=TRUE)
				h<-mean(e2^2, na.rm=TRUE)
				
				ifelse(g > h, fit_banco <- funcets , fit_banco <- funcarima)
				"Intervalo de Confiança"
				forecast(fit_banco, projecao, level = c(60))
			})
			
			dados_fim <- reactive({
				VALOR <- as.data.frame(banco_prev())
				VALOR
			})
			
			
			######################################################################################
			#Projeções de Necessidade de Procedimentos
			######################################################################################	
			combo_output <- reactive({
				
				base_proced_selec <- base_situacao %>%
					subset(grupo == grupo_sel() & procedimento == procedimento_sel() & municipio == municipio_sel())
				
				#Selecionando os parâmetros
				##Situação dos procedimentos
				fila_ini <- base_proced_selec$fila[1]
				
				
				# Parâmetros para simulação
				# solicitacao <- 100
				# fila_ini <- 1000
				n_simula_proced <- 260
				iteracoes <- projecao
				
				procedimentos_controle <- cumprod(c(1, rep(1.05,n_simula_proced))) #Gerando valores fictícios de procedimento para controle, que vão aumentando de 5 em 5%
				
				#Projeção de demanda de primeira consulta antes de somar retornos e filas e de subtrair falta, para a formação da demanda acumulada. 
				solicitacao_tipo <- dados_fim() %>%
					dplyr::select(
						`Point Forecast`,
						`Lo 60`,
						`Hi 60`
					)
				solicitacao_tipo$`Point Forecast` <- as.integer(solicitacao_tipo$`Point Forecast`)
				solicitacao_tipo$`Lo 60` <- as.integer(solicitacao_tipo$`Lo 60`)
				solicitacao_tipo$`Hi 60` <- as.integer(solicitacao_tipo$`Hi 60`)
				names(solicitacao_tipo) <- c("Média", "Mínima", "Máxima") # Ajustando nomes da base para corresponder ao filtro
				solicitacao_proj <- solicitacao_tipo[,colnames(solicitacao_tipo) == tipo_proj_sel()] #Selecionando de acordo com o filtro
				
				
				tempo_entrada <- array(0, dim = c(iteracoes+1, n_simula_proced)) #array que receberá simulação de tempos
				tempo_atendido <- array(0, dim = c(iteracoes+1, n_simula_proced)) #array que receberá simulação de atendidos
				tempo_nao_atendido <- array(0, dim = c(iteracoes+1, n_simula_proced)) #array que receberá simulação de não atendicos
				fila <- array(0, dim = c(iteracoes+1, iteracoes+1, n_simula_proced)) #array que receberá simulação da fila
				fila[1,1,1] <- fila_ini
				demanda_acumulada <- array(0,dim = c(iteracoes+1, n_simula_proced)) #array que receberá simulação de demanda acumulada
				marcacao <- array(0,dim = c(iteracoes+1, n_simula_proced)) #array que receberá simulação de marcacao
				
				for(g in 1:n_simula_proced){
					for(h in 1:iteracoes){
						demanda <- solicitacao_proj[h] #Número de solicitações projetadas
						fila[h,h,g] <- demanda # Os valores de fila no futuro são iguais ao que há de solicitação + o que restar de fila - marcação. Esses dois últimos valores serão incluidos a seguir
						fila[1,1,g] <- solicitacao_proj[1] + fila_ini # O primeiro valor de fila é o número de solicitações no presente 
						demanda_acumulada[h,g] <- sum(fila[h,,g], na.rm = T) #Somando valores de todos os períodos da simulação
						marcacao[h,g] <- min((procedimentos_controle[g]), demanda_acumulada[h,g]) #Não é possível marcar mais que a demanda, por isso, está se usando a seguinte regra: se capacidade < demanda, usar capacidade; se capacidade > demanda, usar demanda
						for(i in 1:h){ # Não se subtrarir mais do que a fila total, por isso, Para fazer a subtração das marcações, usou-se a seguinte regra: se marcação < fila, usar marcacao; se marcação > fila, usar fila. Esse valor será usado para formar a demanda acumulada da próxima iteracao
							if(marcacao[h,g] > fila[h,i,g]){
								fila[h+1,i,g] <- 0
								marcacao[h,g] <- marcacao[h,g] - fila[h,i,g]
							} else{
								fila[h+1,i,g] <- fila[h,i,g] - marcacao[h,g]
								marcacao[h,g] <- 0
							}
							
						}
						
					}	
				}
				
				for(j in 1:n_simula_proced){	
					for(k in 1:iteracoes){
						for(l in k:iteracoes){
							tempo_entrada[k,j] <- tempo_entrada[k,j] + (l-k+1)*(fila[l,k,j]-fila[l+1,k,j])	#Para cada iteração soma o quanto tinha na fila com a di
						}
						tempo_entrada[k,j] <- tempo_entrada[k,j]/fila[k,k,j]
					}
				}
				
				#Menor número de profissionais em que o tempo para controle seja atingido
				#tempo para controle = número de meses até que o controle seja atingido
				procedimento_para_controle_int <- tempo_entrada[(tempo_para_controle()+1):nrow(tempo_entrada),]
				procedimento_para_controle_int <- procedimento_para_controle_int <= 1 &  procedimento_para_controle_int !=0
				procedimento_para_controle_int <- head(procedimento_para_controle_int,-1)
				procedimento_para_controle_int <- colMeans(procedimento_para_controle_int)
				procedimento_para_controle_int <- min(which(procedimento_para_controle_int == 1)) #Selecionando o número da simulação que gera controle 
				procedimento_para_controle <- procedimentos_controle[procedimento_para_controle_int] #Selecionando a quantos procedimentos essa simulação corresponde
				procedimento_para_controle <- round(procedimento_para_controle,0)
				#número de precedimentos para controle
				procedimento_para_controle
				#Para manter o controle, a capacidade deve ser igual à demanda + retorno - falta
				procedimento_manutencao <- round(sum(solicitacao_proj, na.rm = T)/projecao,0)
				
				#Curva de controle
				tempo_grafico <- tempo_entrada[,procedimento_para_controle_int]
				
				combo <- list(grupo_sel = grupo_sel(),
					      procedimento_sel = procedimento_sel(),
					      tempo_para_controle = tempo_para_controle(),
					      iteracoes = iteracoes,
					      base_proced_selec = base_proced_selec, 
					      tempo_grafico = tempo_grafico,
					      procedimento_para_controle = procedimento_para_controle,
					      procedimento_manutencao = procedimento_manutencao,
					      fila = fila,
					      procedimento_para_controle_int = procedimento_para_controle_int,
					      solicitacao_proj = solicitacao_proj)
				combo
				
			}) #fim da reactive
			
			
			
			######################################################################################
			#Outputs Necessidade
			######################################################################################	
			#Tabela 	
			output$table_demanda <- renderTable({
				combo <- combo_output()
				grupo <- combo$grupo_sel
				procedimento <- combo$procedimento_sel
				tabela <- combo$base_proced_selec
				tabela %>%
					select("Grupo" = grupo,
					       "Procedimento" = procedimento,
					       "Fila Atual" = fila
					)
			})	
			
			# #Gráfico - Tempo de espera
			output$fila_demanda <- renderPlotly({
				
				combo <- combo_output()
				tempo_controle <- combo$tempo_grafico
				tempo <- combo$iteracoes
				tempo_para_controle <- combo$tempo_para_controle
				grupo <- combo$grupo_sel
				procedimento <- combo$procedimento_sel
				#Base do gráfico
				out <- data.frame(Espera = round(head(tempo_controle,-1),1), Controle = c(1:tempo))
				
				
				graf_tempo <- ggplot(out, aes(Controle, Espera, group = 1))+
					geom_line()+
					geom_vline(xintercept = tempo_para_controle+1, color = "#CD6E10")+
					theme_light()
				
				ggplotly(graf_tempo)
				
				
				
			})
			
			
			# #Gráfico - Pessoas na fila
			output$pessoas_fila <- renderPlotly({
				
				combo <- combo_output()
				tempo <- combo$iteracoes
				tempo_para_controle <- combo$tempo_para_controle
				fila <- combo$fila
				procedimento_para_controle_int <- combo$procedimento_para_controle_int
				fila <- fila[1:tempo,1:tempo,procedimento_para_controle_int] #usando a tabela que foi selecionada em tempo,procedimento_para_controle_int
				fila <- fila[1:tempo,1] 
				mes <- row.names(dados_fim()) 
				solicitacao_proj <- combo$solicitacao_proj
				fila <- cbind(fila,mes,solicitacao_proj) %>% as.data.frame()
				fila$fila <- as.integer(as.character(fila$fila))
				fila$solicitacao_proj <- as.integer(as.character(fila$solicitacao_proj))
				fila$fila[1] <- fila$fila[1] - fila$solicitacao_proj[1]
				
				out <- data.frame(Fila = fila$fila, Controle = c(1:tempo))
				
				graf_tempo <- ggplot(out, aes(Controle, Fila))+
					geom_col()+
					geom_vline(xintercept = tempo_para_controle+1, color = "#CD6E10")+
					theme_light()
				
				ggplotly(graf_tempo)
			})
			
			
			#ValueBox
			output$controle_demanda <- renderValueBox({
				
				combo <- combo_output()
				prof_control <- combo$procedimento_para_controle
				grupo <- combo$grupo_sel
				procedimento <- combo$procedimento_sel
				
				valueBox(value = prof_control,
					 subtitle = "Procedimentos para Controle (Mês)",
					 icon = icon("check"),
					 color = "blue"
				)
			})
			
			#ValueBox
			output$manutencao_demanda <- renderValueBox({
				
				combo <- combo_output()
				prof_manu <- combo$procedimento_manutencao
				grupo <- combo$grupo_sel
				procedimento <- combo$procedimento_sel
				
				valueBox(value = prof_manu,
					 subtitle = "Procedimentos para Manutenção (Mês)",
					 icon = icon("check-double"),
					 color = "blue"
				)
			})
			
			######################################################################################
			#Outputs ARIMA
			######################################################################################	
			#Gráfico com previsão
			output$serie_banco <- renderPlot({
				autoplot(banco_prev())+
					xlab("Ano") +
					ylab("")+
					ggtitle("Série Temporal - Mensal")+
					theme_light()
			})
			
			#Gráfico dos resíduos
			output$residuos_banco <- renderPlot({
				with_theme <- function(expr) {
					orig <- theme_get()
					theme_set(theme_light())
					force(expr)
					theme_set(orig)
				}
				
				with_theme(checkresiduals(banco_prev()))
			})
			
			
			#Sumário
			output$summary_previsao_banco <- renderPrint({
				summary(banco_prev())
				checkresiduals(banco_prev())
			})
			
			
			#Gráfico dos resíduos
			output$residuos_banco <- renderPlot({
				with_theme <- function(expr) {
					orig <- theme_get()
					theme_set(theme_light())
					force(expr)
					theme_set(orig)
				}
				
				with_theme(checkresiduals(banco_prev()))
				
			})
			
			#Gráfico de decomposição 
			output$decomposicao_banco <- renderPlot({
				##Decomposição por STL
				stl(banco_pre(), s.window="periodic", robust=TRUE) %>% autoplot()+
					theme_light()
			})
			
			
			#Gráfico de sazolnalidade 
			output$sazonal_banco <- renderPlot({
				ggsubseriesplot(banco_pre())+
					ylab("Solicitação")+
					theme_light()
			})
			
			#Tabela de Dados
			output$dados_real <- DT::renderDataTable({
				tabela_real <- banco_preparado_proj() %>%
					dplyr::select(
						grupo,
						procedimento,
						mes_solicitacao,
						solicitacao
					)
				
				names(tabela_real) <- c(
					"Grupo",
					"Procedimento",
					"Mês de Solicitação",
					"Número de Solicitações"
				)
				tabela_real
			}, extensions = 'Buttons',
			options = list(
				"dom" = 'T<"clear">lBfrtip',
				buttons = list('copy', 'csv','excel', 'pdf', 'print'),
				pageLength = 10,
				searching = FALSE))
			
			
			output$dados_prev <- DT::renderDataTable({
				tabela_pred <- dados_fim() %>%
					dplyr::select(
						`Point Forecast`,
						`Lo 60`,
						`Hi 60`
					)
				tabela_pred$`Point Forecast` <- as.integer(tabela_pred$`Point Forecast`)
				tabela_pred$`Lo 60` <- as.integer(tabela_pred$`Lo 60`)
				tabela_pred$`Hi 60` <- as.integer(tabela_pred$`Hi 60`)
				names(tabela_pred) <- c(
					"Média",
					"Limite 20%",
					"Limite 80%"
				)
				
				tabela_pred
			}, extensions = 'Buttons',
			options = list(
				"dom" = 'T<"clear">lBfrtip',
				buttons = list('copy', 'csv','excel', 'pdf', 'print'),
				pageLength = 10,
				searching = FALSE))
		})
}


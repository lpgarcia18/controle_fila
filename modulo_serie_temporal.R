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


#######################################################################
##UI
#######################################################################
#A função UI deve entra como argumento de um tabPanel
serie_temporal_UI <- function(id, banco, grupo_choice, procedimento_choice){
	ns <- NS(id)
	tagList(
		fluidPage(
			hr(),
			fluidRow(
				column(12,
				       column(6,
				              #Selecionando o Grupo
				              selectInput(inputId = ns("grupo_proj"),
				              	    label = "Selecione o Grupo",
				              	    choices = grupo_choice,
				              	    selected = grupo_choice[1], 
				              	    multiple = TRUE)
				       ),
				       column(6,
				              #Selecionando o Procedimento
				              selectInput(inputId = ns("procedimento_proj"),
				              	    label = "Selecione a Procedimento",
				              	    choices = procedimento_choice,
				              	    selected = procedimento_choice[1], 
				              	    multiple = TRUE)
				       )
				)
			),
			fluidRow(
				column(12,
				       column(6,
					#Selecionando lambda
					textInput(inputId = ns("lambda_banco"), 
						  label = "Transformação de Box-Cox(lambda):",
						  value = "NULO"),
					helpText("Se nenhuma transformação for necessária,",
						 br(),
						 "manter Lambda = NULO, ou seja, sem transforação.",
						 br(),
						 "Lambda = 0 produz uma transformação logarítimica.")
					),
					column(6,
					#Selecionando período de previsão
					numericInput(inputId = ns("periodo_prev_banco"), 
						     label = "Período desejado para previsão:",
						     value = 12, 
						     min = 0)
					)
				)
			)
		),
		br(),
		hr(),
		br(),
		 fluidRow(
			column(12,
				tabsetPanel(type = "tabs",
					    #Gráfico da série temporal
					    tabPanel("Previsão de Demanda",
					    	 plotOutput(outputId = ns("serie_banco"), width = "100%", height = 330),
					    	 br(),
					    	 br(),
					    	 br(),
					    	 DT::dataTableOutput(ns("dados_prev"), width = "100%", height = 660)),
					    tabPanel("Decomposição", 
					    	 plotOutput(outputId = ns("decomposicao_banco"), width = "100%", height = 660),
					    	 plotOutput(outputId = ns("sazonal_banco"), width = "100%", height = 330)),
					    tabPanel("Diagnóstico",
					    	 verbatimTextOutput(ns("summary_previsao_banco")), 
					    	 plotOutput(outputId = ns("residuos_banco"), width = "100%", height = 660)),
					    tabPanel("Dados de Produção",
					    	 DT::dataTableOutput(ns("dados_real"), width = "100%", height = 660))
				)		    
			)
		)
	)
}

#################################################################################
#Server
#################################################################################

serie_temporal_SERV <- function(id, banco){
	moduleServer(
		id,
		function(input, output, session)
		{
			
			banco_preparado_proj <- reactive({
				
				producao_select  <- banco  %>%
					subset(banco$grupo %in% input$grupo_proj &
					       	banco$procedimento %in% input$procedimento_proj 
					)
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
					dplyr::summarise(quantidade = sum(quantidade, na.rm = T))
				banco_prep <- banco_prep$quantidade
				banco_prep <- ts(banco_prep, start = c(as.numeric(format(inicio(), format = "%Y")), as.numeric(format(inicio(), format = "%m"))), frequency = 12)
				banco_prep 
			})        
			
			
			
			lambda <-reactive({
				req(input$lambda_banco)
			})     
			
			
			periodo_prev_banco <-reactive({
				req(input$periodo_prev_banco)
			})
			
			
			
			banco_prev <-reactive({
				
				#Função para ETS
				ifelse(input$lambda_banco == "NULO",
				       funcets <- ets(banco_pre(), lambda = NULL),
				       funcets <- ets(banco_pre(), lambda = input$lambda_banco))
				fets <- function(x, h) {
					forecast(funcets, h = 1)
				}
				
				##Função para ARIMA
				ifelse(input$lambda_banco == "NULO",
				       funcarima<- auto.arima(banco_pre(), lambda = NULL),
				       funcarima<- auto.arima(banco_pre(), lambda = input$lambda_banco))
				farima <- function(x, h) {
					forecast(funcarima, h = 1)
				}
				
				## Compute CV errors for ETS as e1
				e1 <- tsCV(banco_pre(), fets, h=1)
				
				## Compute CV errors for ARIMA as e2
				e2 <- tsCV(banco_pre(), farima, h=1)
				
				## Find MSE of each model class
				g<-mean(e1^2, na.rm=TRUE)
				h<-mean(e2^2, na.rm=TRUE)
				
				ifelse(g > h, fit_banco <- funcets , fit_banco <- funcarima)
				"Intervalo de Confiança"
				forecast(fit_banco, periodo_prev_banco(), level = c(60,70))
			})
			
			#Gráfico com previsão
			output$serie_banco <- renderPlot({
				autoplot(banco_prev())+
					xlab("Ano") +
					ylab("")+
					ggtitle("Série Temporal - Mensal")
			})
			
			#Gráfico dos resíduos
			output$residuos_banco <- renderPlot({
				checkresiduals(banco_prev())
			})
			
			
			#Sumário
			output$summary_previsao_banco <- renderPrint({
				summary(banco_prev())
				checkresiduals(banco_prev())
			})
			
			
			#Gráfico dos resíduos
			output$residuos_banco <- renderPlot({
				checkresiduals(banco_prev())
			})
			
			#Gráfico de decomposição 
			output$decomposicao_banco <- renderPlot({
				##Decomposição por STL
				stl(banco_pre(), s.window="periodic", robust=TRUE) %>% autoplot()
			})
			
			
			#Gráfico de sazolnalidade 
			output$sazonal_banco <- renderPlot({
				ggsubseriesplot(banco_pre())+
					ylab("Quantidade")
			})
			
			#Tabela de Dados
			
			output$dados_real <- DT::renderDataTable({
				tabela_real <- banco_preparado_proj() %>%
					dplyr::select(
						grupo,
						procedimento,
						mes_solicitacao,
						quantidade
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
				pageLength = 100,
				searching = FALSE))
			
			
			dados_fim <- reactive({
				VALOR <- as.data.frame(banco_prev())
			})
			
			
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
				pageLength = 100,
				searching = FALSE))
			
			
		})
}

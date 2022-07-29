# Projeção de Demanda por Procedimentos Especializados para Controle de Filas em Saúde

## Introdução

A análise de recursos necessários para controle do tempo de filas para consultas e procedimentos em saúde pode auxiliar na prestação de serviços em tempo oportuno de forma a promover melhores resultados no cuidado em saúde. Esse controle é ainda fundamental para redução do desperdício dos recursos públicos, evitando-se que se haja contratação de serviços em excesso. Assim, essa aplicação tem como objetivo auxiliar os gestores de saúde na análise do número de rescursos necessários para atingir o controle no tempo de espera dos procedimentos e, em seguida, o número de recursos necessários para manter esse tempo sob controle.

## Objetivo
Esse instrumento pretende auxiliar gestores de saúde na estimativa de demanda por procedimentos especializados em saúde, de forma auxiliar a tomada de decisão quanto à necessidade de contratualização ou expansão de serviços próprios para controle de filas em saúde. 

## Método
A aplicação permite projetar a demanda futura por um procedimento especializado em saúde, analisando os resultados de dois modelos: ARIMA (ou SARIMA) e ETS. Toda a análise tem o mês como unidade de medida temporal.

## Como utilizar
Para utilização da aplicação, deve-se salvar na pasta bases a base de dados com a série histórica dos procedimentos especializados (nomeada como "base_procedimento", em formato .csv e com as seguintes colunas: grupo, procedimento, mes_solicitacao (no formato "%Y-%m-%d") e quantidade).

Os parâmetros para utilização são:
1) Selecione o Grupo - Possibilita a seleção de um grupo de procedimentos, para facilitar a busca por um procedimento específico.
2) Selecione o Procedimento - Permite a seleção de qual ou quais procedimentos devem ter sua demanda prevista.
3) Transformação de Box-Cox(lambda) - Permite a seleção de lambdas para a transformação da série histórica, se necessário. Lambada = NULO significa que não será aplicada nenhuma transformação. Lambda = 0 produz transformação logarítmica. 
4) Período desejado para previsão - Possibilita a definição de qual o número de meses de previsão necessários.


## Reutilização do código
A aplicação está licenciada sob GLP 3.0 (https://github.com/lpgarcia18/controle_fila/blob/main/LICENSE), de modo a permitir sua reutilização por outros municípios e o desenvolvimento conjuto. 

Sugestões e contribuições são muito bem vindas.


# Projeção de Demanda por Procedimentos Especializados para Controle de Filas em Saúde

## Introdução

A análise de recursos necessários para controle do tempo de filas para consultas e procedimentos em saúde pode auxiliar na prestação de serviços em tempo oportuno de forma a promover melhores resultados no cuidado em saúde. Esse controle é ainda fundamental para redução do desperdício dos recursos públicos, evitando-se que se haja contratação de serviços em excesso. Assim, essa aplicação tem como objetivo auxiliar os gestores de saúde na análise do número de rescursos necessários para atingir o controle no tempo de espera dos procedimentos e, em seguida, o número de recursos necessários para manter esse tempo sob controle.

## Objetivo
Esse instrumento pretende auxiliar gestores de saúde na estimativa de demanda por procedimentos especializados em saúde, de forma auxiliar a tomada de decisão quanto à necessidade de contratualização ou expansão de serviços próprios para controle de filas em saúde. 

## Método
A aplicação permite o cálculo da oferta necessária para: 1) controlar e para 2) manter o controle da fila por um determinado procedimento em saúde. Esses cálculos são feitos a partir da definição do 1) do procedimento a ser avaliado e do 2) tempo desejado para o controle. Toda a análise tem o mês como unidade de medida temporal.

## Como utilizar
Para utilização da aplicação, deve-se salvar na pasta bases a base de dados com a série histórica dos procedimentos especializados (nomeada como "base_procedimento", em formato .csv e com as seguintes colunas: grupo, procedimento, mes_solicitacao (no formato "%Y-%m-%d") e quantidade).

Os parâmetros para utilização são:


## Reutilização do código
A aplicação está licenciada sob GLP 3.0 (https://github.com/lpgarcia18/controle_fila/blob/main/LICENSE), de modo a permitir sua reutilização por outros municípios e o desenvolvimento conjuto. 

Sugestões e contribuições são muito bem vindas.


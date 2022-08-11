with proc as(
select 	
	CASE 
      WHEN p.ds_procedimento = 'CATETERISMO CARDIACO' THEN 'CATETERISMO CARDIACO - ADULTO'
      WHEN p.ds_procedimento = 'CATETERISMO CARDIACO EM PEDIATRIA' THEN 'CATETERISMO CARDIACO - PEDIATRIA'
      WHEN p.ds_procedimento = 'ECOCARDIOGRAFIA DE ESTRESSE' THEN 'ECOCARDIOGRAFIA DE ESTRESSE - ADULTO'
      WHEN p.ds_procedimento = 'ECOCARDIOGRAFIA TRANSESOFAGICA' THEN 'ECOCARDIOGRAFIA TRANSTORÁCICA - PEDIATRIA'
      WHEN p.ds_procedimento = 'ECOCARDIOGRAFIA TRANSTORÁCICA' THEN 'ECOCARDIOGRAFIA TRANSTORACICA - ADULTO'
      --WHEN p.ds_procedimento = 'ELETROCARDIOGRAMA' THEN 'ELETROCARDIOGRAMA - ADULTO' ##Esse não será usado, pois é dos laudos e duplica o número de exames
      WHEN p.ds_procedimento = 'ELETROCARDIOGRAMA (A)' THEN 'ELETROCARDIOGRAMA - ADULTO'
      WHEN p.ds_procedimento = 'MONITORAMENTO PELO SISTEMA HOLTER 24 HS (3 CANAIS)' THEN 'MONITORAMENTO PELO SISTEMA HOLTER 24 HS (3 CANAIS) - ADULTO'
      WHEN p.ds_procedimento = 'MONITORIZACAO AMBULATORIAL DE PRESSAO ARTERIAL' THEN 'MONITORIZACAO AMBULATORIAL DE PRESSAO ARTERIAL - ADULTO'
      WHEN p.ds_procedimento = 'TESTE DE ESFORCO / TESTE ERGOMETRICO' THEN 'TESTE DE ESFORCO / TESTE ERGOMETRICO - ADULTO'
	end as ds_procedimento_cardio,
	sa.dt_solicitacao,
	sa.dt_agendamento,	
	sa.dt_desejada, 
	sa.cd_solicitacao,
	p.cd_procedimento,
	--'teste' as cd_tp_procedimento,
	case
		when sa.status = 0 then 'nao_regulado'
		when sa.status = 1 then 'regulado'
		when sa.status = 2 then 'agendado'
		when sa.status = 4 then 'devolvido'		
		when sa.status = 6 then 'cancelado'
	end as status_regulacao, 
	case
		when agah.status = 1 then 'agendado'
		when agah.status = 2 then 'concluido'
		when agah.status = 3 then 'cancelado'
		when agah.status = 4 then 'falta'		
		when agah.status = 5 then 'remanejado'
	end as status_agenda, -- quem ficar nulo é porque ainda não foi regulado.	
	case 
		when uc.cd_municipio_residencia = 420540 then 'Florianópolis'
		when uc.cd_municipio_residencia <> 420540 then 'Outro'
	end as municipio,
	case 
		when sa.tp_consulta =0 then '1a consulta'
		when  sa.tp_consulta =1 then 'retorno'
	end as tp_consulta
	from
		procedimento p 
		inner join solicitacao_agendamento sa on p.cd_procedimento = sa.cd_procedimento 
		left join agenda_gra_ate_horario agah on sa.cd_solicitacao = agah.cd_solicitacao
		left join usuario_cadsus uc on sa.cd_usu_cadsus = uc.cd_usu_cadsus 
	where 
		p.ds_procedimento in ('CATETERISMO CARDIACO',
								'CATETERISMO CARDIACO EM PEDIATRIA',
								'ECOCARDIOGRAFIA DE ESTRESSE',
								'ECOCARDIOGRAFIA TRANSESOFAGICA',
								'ECOCARDIOGRAFIA TRANSTORACICA',
								--'ELETROCARDIOGRAMA', ##Esse não será usado, pois é dos laudos e duplica o número de exames
								'ELETROCARDIOGRAMA (A)',
								'MONITORAMENTO PELO SISTEMA HOLTER 24 HS (3 CANAIS)',
								'MONITORIZACAO AMBULATORIAL DE PRESSAO ARTERIAL',
								'TESTE DE ESFORCO / TESTE ERGOMETRICO'
								) 	
	), tipo_proc as(
	select 	
	CASE 
      WHEN tp.ds_tp_procedimento = 'SMS - CONSULTA EM CARDIOLOGIA - PRÉ-ANGIOPLASTIA' THEN 'CONSULTA EM CARDIOLOGIA - PRÉ-ANGIOPLASTIA - ADULTO'
      WHEN tp.ds_tp_procedimento = 'SMS - CONSULTA EM CARDIOLOGIA ADULTO' THEN 'CONSULTA EM CARDIOLOGIA - ADULTO'
      WHEN tp.ds_tp_procedimento = 'SMS - CONSULTA EM CARDIOLOGIA PEDIATRIA' THEN 'CONSULTA EM CARDIOLOGIA - PEDIATRIA'
      WHEN tp.ds_tp_procedimento = 'SMS - CONSULTA EM CIRURGIA CARDÍACA' THEN 'CONSULTA EM CIRURGIA CARDÍACA - ADULTO'
      WHEN tp.ds_tp_procedimento = 'SMS - ECOCARDIOGRAFIA DE ESTRESSE' THEN 'ECOCARDIOGRAFIA DE ESTRESSE - ADULTO'
      WHEN tp.ds_tp_procedimento = 'SMS - ECOCARDIOGRAFIA TRANSESOFÁGICA' THEN 'ECOCARDIOGRAFIA TRANSESOFÁGICA - ADULTO'
      WHEN tp.ds_tp_procedimento = 'SMS - ECOCARDIOGRAFIA TRANSTORÁCICA - ADULTO' THEN 'ECOCARDIOGRAFIA TRANSTORÁCICA - ADULTO'
      WHEN tp.ds_tp_procedimento = 'SMS - ECOCARDIOGRAFIA TRANSTORÁCICA - PEDIATRIA' THEN 'ECOCARDIOGRAFIA TRANSTORÁCICA - PEDIATRIA'
      WHEN tp.ds_tp_procedimento = 'SMS - ELETROCARDIOGRAMA - ADULTO' THEN 'ELETROCARDIOGRAMA - ADULTO'
      WHEN tp.ds_tp_procedimento = 'SMS - ELETROCARDIOGRAMA - PEDIATRIA' THEN 'ELETROCARDIOGRAMA - PEDIATRIA'
      --WHEN tp.ds_tp_procedimento = 'SES - ELETROCARDIOGRAMA - INFANTIL' THEN 'ELETROCARDIOGRAMA - PEDIATRIA'
      --WHEN tp.ds_tp_procedimento = 'SES - HOLTER 24 HORAS' THEN 'MONITORAMENTO PELO SISTEMA HOLTER 24 HS (3 CANAIS) - ADULTO'
      WHEN tp.ds_tp_procedimento = 'SMS - HOLTER 24 HORAS - ADULTO' THEN 'MONITORAMENTO PELO SISTEMA HOLTER 24 HS (3 CANAIS) - ADULTO'
      WHEN tp.ds_tp_procedimento = 'SMS - HOLTER 24 HORAS - PEDIATRIA' THEN 'MONITORAMENTO PELO SISTEMA HOLTER 24 HS (3 CANAIS) - PEDIATRIA'
      --WHEN tp.ds_tp_procedimento = 'SES MONITORIZACAO AMBULATORIAL DE PRESSAO ARTERIAL' THEN 'MONITORIZACAO AMBULATORIAL DE PRESSAO ARTERIAL - ADULTO'
      WHEN tp.ds_tp_procedimento = 'SMS - MONITORIZAÇÃO AMBULATORIAL DE PRESSÃO ARTERIAL - MAPA - ADULTO' THEN 'MONITORIZACAO AMBULATORIAL DE PRESSAO ARTERIAL - ADULTO'
      WHEN tp.ds_tp_procedimento = 'SMS - MONITORIZAÇÃO AMBULATORIAL DE PRESSÃO ARTERIAL - MAPA - PEDIATRIA' THEN 'MONITORIZACAO AMBULATORIAL DE PRESSAO ARTERIAL - PEDIATRIA'
      WHEN tp.ds_tp_procedimento = 'SMS - TESTE ERGOMETRICO - ADULTO' THEN 'TESTE DE ESFORCO / TESTE ERGOMETRICO - ADULTO'
      WHEN tp.ds_tp_procedimento = 'SMS - TESTE ERGOMETRICO - PEDIATRIA' THEN 'TESTE DE ESFORCO / TESTE ERGOMETRICO - ADULTO'
	end as ds_procedimento_cardio,
	sa.dt_solicitacao,
	sa.dt_agendamento,
	sa.dt_desejada, 
	sa.cd_solicitacao,
	tp.cd_tp_procedimento,
	case
		when sa.status = 0 then 'nao_regulado'
		when sa.status = 1 then 'regulado'
		when sa.status = 2 then 'agendado'
		when sa.status = 4 then 'devolvido'		
		when sa.status = 6 then 'cancelado'
	end as status_regulacao, 
	case
		when agah.status = 1 then 'agendado'
		when agah.status = 2 then 'concluido'
		when agah.status = 3 then 'cancelado'
		when agah.status = 4 then 'falta'		
		when agah.status = 5 then 'remanejado'
	end status_agenda, -- quem ficar nulo é porque ainda não foi regulado.
	case 
		when uc.cd_municipio_residencia = 420540 then 'Florianópolis'
		when uc.cd_municipio_residencia <> 420540 then 'Outro'
	end as municipio,
	case 
		when sa.tp_consulta =0 then '1a consulta'
		when  sa.tp_consulta =1 then 'retorno'
	end as tp_consulta
	from
	  tipo_procedimento tp 
	  inner join solicitacao_agendamento sa on tp.cd_tp_procedimento = sa.cd_tp_procedimento
	  left join agenda_gra_ate_horario agah on sa.cd_solicitacao = agah.cd_solicitacao 
	  left join usuario_cadsus uc on sa.cd_usu_cadsus = uc.cd_usu_cadsus 
	where 	
		tp.ds_tp_procedimento in ('SMS - CONSULTA EM CARDIOLOGIA - PRÉ-ANGIOPLASTIA',
								'SMS - CONSULTA EM CARDIOLOGIA ADULTO',
								'SMS - CONSULTA EM CARDIOLOGIA PEDIATRIA',
								'SMS - ECOCARDIOGRAFIA DE ESTRESSE',
								'SMS - CONSULTA EM CIRURGIA CARDÍACA',
								'SMS - ECOCARDIOGRAFIA TRANSESOFÁGICA',
								'SMS - ECOCARDIOGRAFIA TRANSTORÁCICA - ADULTO',
								'SMS - ECOCARDIOGRAFIA TRANSTORÁCICA - PEDIATRIA',
								'SMS - ELETROCARDIOGRAMA - ADULTO',
								'SMS - ELETROCARDIOGRAMA - PEDIATRIA',
								--'SES - ELETROCARDIOGRAMA - INFANTIL',
								'SES - HOLTER 24 HORAS',
								'SMS - HOLTER 24 HORAS - ADULTO',
								'SMS - HOLTER 24 HORAS - PEDIATRIA',
								--'SES MONITORIZACAO AMBULATORIAL DE PRESSAO ARTERIAL',
								'SMS - MONITORIZAÇÃO AMBULATORIAL DE PRESSÃO ARTERIAL - MAPA - ADULTO',
								'SMS - MONITORIZAÇÃO AMBULATORIAL DE PRESSÃO ARTERIAL - MAPA - PEDIATRIA',
								'SMS - TESTE ERGOMETRICO - ADULTO',
								'SMS - TESTE ERGOMETRICO - PEDIATRIA'		
		)
)

select 
	ds_procedimento_cardio,
	status_regulacao,
	status_agenda,
	municipio,
	tp_consulta, 
	date_trunc('month', dt_solicitacao) as mes_solicitacao,
	count(cd_solicitacao) as quantidade 
from 
	proc
where dt_desejada < current_date -- retirando solicitacoes futuras
group by ds_procedimento_cardio, status_agenda,	status_regulacao, municipio, tp_consulta, date_trunc('month', dt_solicitacao)

union 

select 
	ds_procedimento_cardio,
	status_regulacao,
	status_agenda,
	municipio,
	tp_consulta,
	date_trunc('month', dt_solicitacao) as mes_solicitacao,
	count(cd_solicitacao) as quantidade 
from 
	tipo_proc
where dt_desejada < current_date -- retirando solcitacoes futuras
group by ds_procedimento_cardio, status_agenda, status_regulacao, municipio, tp_consulta, date_trunc('month', dt_solicitacao)

;
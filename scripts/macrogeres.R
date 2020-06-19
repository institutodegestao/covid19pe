library(dplyr)
library(flexdashboard)
library(ggplot2)
library(plotly)
library(shiny)
library(zoo)

########## MACRO #############
########## CASOS MACRO #############

convert_macro <- read.csv2('base/conversao_unidades.csv', sep = ';')

srag_municipio <- merge(srag_municipio, convert_macro, by = 'municipio') 

srag_macro <- srag_municipio %>% group_by(data = data, macro = macrogeres) %>% summarise(DESCARTADO = sum(DESCARTADO), CONFIRMADO = sum(CONFIRMADO), SRAG = sum(SRAG))

srag_macro$semana <- epiweek(srag_macro$data)

srag_macro <- srag_macro %>% group_by(macro) %>%
  mutate(confMM7 = round(rollmean(x = CONFIRMADO, 7, align = "right", fill = NA),2))

srag_macro <- srag_macro %>% group_by(macro) %>%
  mutate(descfMM7 = round(rollmean(x = DESCARTADO, 7, align = "right", fill = NA),2))

srag_macro <- srag_macro %>% group_by(macro) %>%
  mutate(sragfMM7 = round(rollmean(x = SRAG, 7, align = "right", fill = NA),2))

srag_macro <- srag_macro %>% replace(is.na(.), 0)

########## OBITOS MACRO #############

obitos_municipio <- merge(obitos_municipio, convert_macro, by = 'municipio') 

obitos_macro <- obitos_municipio %>% group_by(data = data, macro = macrogeres) %>% summarise(DESCARTADO = sum(DESCARTADO), CONFIRMADO = sum(CONFIRMADO), SRAG = sum(SRAG))

obitos_macro$semana <- epiweek(obitos_macro$data)

obitos_macro <- obitos_macro %>% group_by(macro) %>%
  mutate(confMM7 = round(rollmean(x = CONFIRMADO, 7, align = "right", fill = NA),2))

obitos_macro <- obitos_macro %>% group_by(macro) %>%
  mutate(descfMM7 = round(rollmean(x = DESCARTADO, 7, align = "right", fill = NA),2))

obitos_macro <- obitos_macro %>% group_by(macro) %>%
  mutate(sragfMM7 = round(rollmean(x = SRAG, 7, align = "right", fill = NA),2))

obitos_macro <- obitos_macro %>% replace(is.na(.), 0)

########## SOLICITACOES MACRO #############

solicitacoes_municipios <- solicitacoes_municipios[,c('data', 'municipio', 'ENFERMARIA', 'UTI', 'total')]

solicitacoes_municipios <- merge(solicitacoes_municipios, convert_macro, by = c('municipio')) 

solicitacoes_macro <- solicitacoes_municipios %>% group_by(data = data, macro = macrogeres) %>% summarise(ENFERMARIA = sum(ENFERMARIA), UTI = sum(UTI), total = sum(total))

solicitacoes_macro$semana <- epiweek(solicitacoes_macro$data)

solicitacoes_macro <- solicitacoes_macro %>% group_by(macro) %>%
  mutate(geralMM7 = round(rollmean(x = total, 7, align = "right", fill = NA),2))

solicitacoes_macro <- solicitacoes_macro %>% group_by(macro) %>% mutate(enfMM7 = round(rollmean(x = ENFERMARIA, 7, align = "right", fill = NA),2))

solicitacoes_macro <- solicitacoes_macro %>% group_by(macro) %>%
  mutate(utiMM7 = round(rollmean(x = UTI, 7, align = "right", fill = NA),2))

solicitacoes_macro <- solicitacoes_macro %>% replace(is.na(.), 0)

######################## FILA TOTAL MACRO ######################

fila_total_municipios <- fila_total_municipios[,c('data', 'municipio', 'ENFERMARIA', 'UTI', 'total')]

fila_total_municipios <- merge(fila_total_municipios, convert_macro, by = c('municipio')) 

fila_total_macro <- fila_total_municipios %>% group_by(data = data, macro = macrogeres) %>% summarise(ENFERMARIA = sum(ENFERMARIA), UTI = sum(UTI), total = sum(total))

fila_total_macro$semana <- epiweek(fila_total_macro$data)

fila_total_macro <- fila_total_macro %>% group_by(macro) %>%
  mutate(geralMM7 = round(rollmean(x = total, 7, align = "right", fill = NA),2))

fila_total_macro <- fila_total_macro %>% group_by(macro) %>%
  mutate(enfMM7 = round(rollmean(x = ENFERMARIA, 7, align = "right", fill = NA),2))

fila_total_macro <- fila_total_macro %>% group_by(macro) %>%
  mutate(utiMM7 = round(rollmean(x = UTI, 7, align = "right", fill = NA),2))

fila_total_macro <- fila_total_macro %>% replace(is.na(.), 0)

######################## FILA NOVA MACRO ######################

fila_nova_municipios <- fila_nova_municipios[,c('data', 'municipio', 'ENFERMARIA', 'UTI', 'total')]

fila_nova_municipios <- merge(fila_nova_municipios, convert_macro, by = c('municipio')) 

fila_nova_macro <- fila_nova_municipios %>% group_by(data = data, macro = macrogeres) %>% summarise(ENFERMARIA = sum(ENFERMARIA), UTI = sum(UTI), total = sum(total))

fila_nova_macro$semana <- epiweek(fila_nova_macro$data)

fila_nova_macro <- fila_nova_macro %>% group_by(macro) %>%
  mutate(geralMM7 = round(rollmean(x = total, 7, align = "right", fill = NA),2))

fila_nova_macro <- fila_nova_macro %>% group_by(macro) %>%
  mutate(enfMM7 = round(rollmean(x = ENFERMARIA, 7, align = "right", fill = NA),2))

fila_nova_macro <- fila_nova_macro %>% group_by(macro) %>%
  mutate(utiMM7 = round(rollmean(x = UTI, 7, align = "right", fill = NA),2))

fila_nova_macro <- fila_nova_macro %>% replace(is.na(.), 0)

# # ##########
# # 
# # base_macro <- base_macro %>%
# #   group_by(macro) %>%
# #   mutate(solicitacoesMM7 = round(rollmean(x = solicitacoesDia, 7, align = "right", fill = NA),2))
# # 
# # base_macro <- base_macro %>%
# #   group_by(macro) %>%
# #   mutate(obitosMM7 = round(rollmean(x = obitosDia, 7, align = "right", fill = NA),2))
# 
# base_macro <- base_macro %>% replace(is.na(.), 0)

########## INTERNACOES #############
internacoes_macro <- read.csv2('resultado/internacoes_macro.csv', sep = ';')

internacoes_macro$data <- as.Date(internacoes_macro$data, format = "%Y-%m-%d")

internacoes_macro <- internacoes_macro %>% 
  group_by(macro) %>%
  mutate(intUTIMM7 = round(rollmeanr(x = UTI, 7, align = "right", fill = NA),2))

internacoes_macro <- internacoes_macro %>% group_by(macro) %>%
  mutate(intENFMM7 = round(rollmean(x = ENFERMARIA, 7, align = "right", fill = NA),2))

internacoes_macro <- internacoes_macro %>% group_by(macro) %>%
  mutate(intGLMM7 = round(rollmean(x = geral, 7, align = "right", fill = NA),2))

internacoes_macro <- internacoes_macro %>% replace(is.na(.), 0)

########## MACRO #############
########## LEITOS MACRO#############
leitos_macrogeres <- read.csv2('resultado/leitos_macrogeres.csv', sep = ';')

leitos_macrogeres$data <- as.Date(leitos_macrogeres$data, format = "%Y-%m-%d")

leitos_macrogeres$UTI_TX <- round(1- leitos_macrogeres$UTI_LIVRE / leitos_macrogeres$UTI_DSP,2)

leitos_macrogeres$ENFERMARIA_TX <- round(1- leitos_macrogeres$ENFERMARIA_LIVRE / leitos_macrogeres$ENFERMARIA_DSP,2)

leitos_macrogeres$GERAL_TX <- round(1- leitos_macrogeres$GERAL_LIVRE / leitos_macrogeres$GERAL_DSP,2)

leitos_macrogeres <- leitos_macrogeres %>% replace(is.na(.), 0)

leitos_macrogeres <- leitos_macrogeres %>% group_by(macrogeres) %>% mutate(txUTIMM7 = round(rollmean(x = UTI_TX, 7, align = "right", fill = NA),2))

leitos_macrogeres <- leitos_macrogeres %>% group_by(macrogeres) %>% mutate(txENFMM7 = round(rollmean(x = ENFERMARIA_TX, 7, align = "right", fill = NA),2))

leitos_macrogeres <- leitos_macrogeres %>% group_by(macrogeres) %>% mutate(txTOTALMM7 = round(rollmean(x = GERAL_TX, 7, align = "right", fill = NA),2))

leitos_macrogeres <- leitos_macrogeres %>% group_by(macrogeres) %>% mutate(dspUTIMM7 = round(rollmean(x = UTI_DSP, 7, align = "right", fill = NA),2))

leitos_macrogeres <- leitos_macrogeres %>% group_by(macrogeres) %>% mutate(dspENFMM7 = round(rollmean(x = ENFERMARIA_DSP, 7, align = "right", fill = NA),2))

leitos_macrogeres <- leitos_macrogeres %>% group_by(macrogeres) %>% mutate(dspTOTALMM7 = round(rollmean(x = GERAL_DSP, 7, align = "right", fill = NA),2))

leitos_macrogeres <- leitos_macrogeres %>% group_by(macrogeres) %>% mutate(totalUTIMM7 = round(rollmean(x = UTI_TOTAL, 7, align = "right", fill = NA),2))

leitos_macrogeres <- leitos_macrogeres %>% group_by(macrogeres) %>%  mutate(totalENFMM7 = round(rollmean(x = ENFERMARIA_TOTAL, 7, align = "right", fill = NA),2))

leitos_macrogeres <- leitos_macrogeres %>% group_by(macrogeres) %>% mutate(totalGERALMM7 = round(rollmean(x = GERAL_TOTAL, 7, align = "right", fill = NA),2))

leitos_macrogeres <- leitos_macrogeres %>% replace(is.na(.), 0)

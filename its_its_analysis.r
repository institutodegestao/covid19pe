library(its.analysis)

listaRMR <- c("RECIFE", "OLINDA", 'JABOATAO DOS GUARARAPES', 'CAMARAGIBE', 'SAO LOURENCO DA MATA')

obitos_pernambuco_its <- obitos_municipio %>% filter(municipio %in% listaRMR) %>% group_by(data = data) %>% summarise(DESCARTADO = sum(DESCARTADO), CONFIRMADO = sum(CONFIRMADO), SRAG = sum(SRAG))

obitos_pernambuco_its <- obitos_pernambuco_its %>%
  mutate(confMM7 = round(rollmean(x = CONFIRMADO, 7, align = "right", fill = NA),2))

obitos_pernambuco_its <- obitos_pernambuco_its %>%
  mutate(descfMM7 = round(rollmean(x = DESCARTADO, 7, align = "right", fill = NA),2))

obitos_pernambuco_its <- obitos_pernambuco_its %>%
  mutate(sragfMM7 = round(rollmean(x = SRAG, 7, align = "right", fill = NA),2))

obitos_pernambuco_its <- obitos_pernambuco_its %>% replace(is.na(.), 0)

obitos_pernambuco_its <- obitos_pernambuco_its %>% filter(data >= '2020-04-30') %>% filter(data <= '2020-06-22')

obitos_pernambuco_its$situacao <- ifelse(obitos_pernambuco_its$data >= '2020-06-08', 'reabertura', ifelse(obitos_pernambuco_its$data >= '2020-06-01', 'afastamento', ifelse(obitos_pernambuco_its$data >= '2020-05-16', 'lockdown', 'afastamento')))

plotly::plot_ly(data = obitos_pernambuco_its, mode = 'text', text =~situacao, textposition = 'middle right') %>%
  add_trace(
    x = ~data,
    y = ~confMM7,
    type = 'scatter', mode = 'lines',
    name = "MM7 - Srag",
    line = list(color = 'rgb(22, 96, 167)', dash = 'dash'),
    marker = list(color = 'rgb(22, 96, 167)')
  ) %>% 
  layout(
    title = 'Avaliacao Quarentena',
    yaxis = list(title = "Obitos"),
    xaxis = list(title = "Data"),
    hovermode = "compare",
    shapes = list(
      list(type = "rect",
           fillcolor = "blue", line = list(color = "blue"), opacity = 0.1, x0 = "2020-05-16", x1 = "2020-05-31", xref = "x", y0 = min(obitos_pernambuco_its$confMM7), y1 = max(obitos_pernambuco_its$confMM7), yref = "y"),
      list(type = "rect",
           fillcolor = "green", line = list(color = "green"), opacity = 0.1, x0 = "2020-06-08", x1 = "2020-06-22", xref = "x", y0 = min(obitos_pernambuco_its$confMM7), y1 = max(obitos_pernambuco_its$confMM7), yref = "y")
      )
    )

lockdown_ad <- obitos_pernambuco_its %>% filter(data <= '2020-06-07')

lockdown_ad$lockdown <- ifelse(lockdown_ad$data >= '2020-05-16' & lockdown_ad$data <= '2020-05-31', 1, 0)

lockdown_ad <- as.data.frame(lockdown_ad)

itsa.model(data=lockdown_ad, time="data", depvar="confMM7", interrupt_var = "lockdown", alpha=0.05, bootstrap=TRUE, Reps = 1000)

reabertura_ad <- obitos_pernambuco_its %>% filter(data >= '2020-05-16')

reabertura_ad$lockdown <- ifelse(reabertura_ad$data >= '2020-06-08' & reabertura_ad$data <= '2020-06-22', 1, 0)

reabertura_ad <- as.data.frame(reabertura_ad)

reabertura_ista <- itsa.model(data=reabertura_ad, time="data", depvar="confMM7", interrupt_var = "lockdown", alpha=0.05, bootstrap=TRUE, Reps = 1000)


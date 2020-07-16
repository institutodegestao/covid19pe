library(plotly)

obitos_comparacao <- read.csv2('resultado/obitos_comparacao.csv', sep = ';')

x <- hist(obitos_comparacao$dt_dif, probability = T)

fig <- plot_ly(x = obitos_comparacao$dt_dif, type = "histogram", histnorm = "probability")

fig

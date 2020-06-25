library(plotly)

obitos_comparacao <- read.csv2('resultado/obitos_comparacao.csv', sep = ';')

hist(obitos_comparacao$dt_dif)

fig <- plot_ly(x = obitos_comparacao$dt_dif, type = "histogram", histnorm = "probability")

fig

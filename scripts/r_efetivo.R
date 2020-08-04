library(dplyr)
library(ggplot2)
library(HDInterval)
library(lubridate)
library(plotly)
library(tidyverse)
library(zoo)

R_T_MAX = 12
r_t_range = seq(0, R_T_MAX, length = R_T_MAX*100 + 1)

GAMMA = 1/4

compute_likelihood <- function(cases) {
  likelihood <- cases %>% filter(casosNovosMM7 > 0) %>% mutate(r_t = list(r_t_range), lambda = map(lag(casosNovosMM7, 1), ~ .x * exp(GAMMA * (r_t_range - 1))), likelihood_r_t = map2(casosNovosMM7, lambda, dpois, log = TRUE)) %>% slice(-1) %>% select(-lambda) %>% unnest(c(likelihood_r_t, r_t))
}

compute_posterior <- function(likelihood) {
  likelihood %>% arrange(data) %>% group_by(r_t) %>% mutate(posterior = exp(zoo::rollapplyr(likelihood_r_t, 7, sum, partial = TRUE))) %>% group_by(data) %>% mutate(posterior = posterior / sum(posterior, na.rm = TRUE)) %>% mutate(posterior = ifelse(is.nan(posterior), 0, posterior)) %>% ungroup() %>% select(-likelihood_r_t)
}

estimate_rt <- function(posteriors){
  posteriors %>% group_by(data) %>% summarize(r_t_simulated = list(sample(r_t_range, 10000, replace = TRUE, prob = posterior)), r_t_most_likely = r_t_range[which.max(posterior)]) %>% mutate(r_t_lo = map_dbl(r_t_simulated, ~ hdi(.x)[1]), r_t_hi = map_dbl(r_t_simulated, ~ hdi(.x)[2])) %>% select(-r_t_simulated)
}

confirmados_municipio = read.csv2('resultado/confirmados_municipio.csv', sep = ';')

confirmados_municipio$data <- as.Date(confirmados_municipio$data, format = "%Y-%m-%d")

pe_confirmados <- confirmados_municipio %>% group_by(data) %>% summarise(casosNovos = sum(casosNovos))

pe_confirmados <- pe_confirmados %>%  mutate(casosNovosMM7 = ceiling(rollmean(x = casosNovos, 7, align = "right", fill = NA)))

pe_confirmados_rt <- pe_confirmados %>% compute_likelihood() %>% compute_posterior() %>% estimate_rt()

colnames(pe_confirmados_rt) <- c('data', 'rt', 'rt_min', 'rt_max')

# pe_rt_fato$cor <- ifelse(pe_rt_fato$rt_max >= 1, 'blue', 'green')

p <- ggplot(pe_confirmados_rt, aes(y=rt, x=data)) + geom_line(colour = 'blue1', linetype = "dashed") + geom_point(colour = 'blue') + geom_ribbon(aes(ymin=rt_min, ymax=rt_max, x=data), alpha = 0.2) + scale_colour_manual("", values="blue") + scale_fill_manual("", values="blue1") + geom_hline(yintercept=1, linetype = "dashed")

fig <- ggplotly(p) %>%
  plotly::layout(
    height = '380',
    title = paste('R0 ', round(rep_bas$R0[1],2)),
    yaxis = list(title = "Rt - Numero Reprodutivo Efetivo"),
    xaxis = list(title = ""),
    hovermode = "compare"
  )

fig

confirmados_municipio <- merge(confirmados_municipio, convert_macro, by = 'municipio')

confirmados_macro <- confirmados_municipio %>% group_by(data = data, macro = macrogeres) %>% summarise(casosNovos = sum(casosNovos))

confirmados_macro <- confirmados_macro %>% group_by %>% mutate(casosNovosMM7 = ceiling(rollmean(x = casosNovos, 7, align = "right", fill = NA)))

confirmados_geres <- confirmados_municipio %>% group_by(data = data, geres = geres) %>% summarise(casosNovos = sum(casosNovos))

confirmados_geres <- confirmados_geres %>% group_by %>% mutate(casosNovosMM7 = ceiling(rollmean(x = casosNovos, 7, align = "right", fill = NA)))

confirmados_mun_rt <- confirmados_municipio %>% group_by(data = data, municipio = municipio) %>% summarise(casosNovos = sum(casosNovos))

confirmados_mun_rt <- confirmados_mun_rt %>% group_by %>% mutate(casosNovosMM7 = ceiling(rollmean(x = casosNovos, 7, align = "right", fill = NA)))

# macro_confirmados_rt <- confirmados_macro %>% compute_likelihood() %>% compute_posterior() %>% estimate_rt()
# 
# colnames(macro_confirmados_rt) <- c('data', 'rt', 'rt_min', 'rt_max')
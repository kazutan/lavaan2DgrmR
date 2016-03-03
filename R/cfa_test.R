library(lavaan)

xx <- HolzingerSwineford1939

HS_model <- '
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
'

fit <- cfa(HS_model, data=xx)

## test create_graph_lavaan()

cfa_test <- create_graph_lavaan(fit)
render_graph(cfa_test)
# sem sample code from http://lavaan.ugent.be/tutorial/sem.html

library(lavaan)
model2 <- '
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'
fit2 <- sem(model2, data=PoliticalDemocracy)
summary(fit2, standardized=TRUE)

## test create_graph_lavaan()

sem_test <- create_graph_lavaan(fit2)
render_graph(sem_test)

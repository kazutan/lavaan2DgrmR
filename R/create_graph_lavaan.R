## 現時点ではかなりグチャグチャになる
## 多分DiagrammeR(GraphViz)側で修正が必要。rank=sameとか

create_graph_lavaan <- function(fit, ...) {
  library(Hmisc) #順番注意
  library(DiagrammeR)
  library(dplyr)
  
  # 必要データ取り出し
  d <- data.frame(fit@ParTable) %>% 
    select(lhs,op,rhs,est)
  #こっちもありかも
  #こっちを使うなら、推定値だけじゃなくてp値も持ってこれるので、
  #アスタリスクを自動で付けたりとかできるようになる
  #library(lavaan)
  #d <- fit %>% parameterEstimates() %>% 
  #  select(lhs,op,rhs,est)
  
  # make NDF
  latent <- d %>%
    filter(op == "=~") %>%
    select(nodes = lhs) %>%
    distinct %>%
    mutate(shape = "circle", rank = "same")
  observed <- d %>% 
    filter(op != "~1", lhs %nin% latent$nodes) %>%
    select(nodes = lhs) %>%
    distinct %>%
    mutate(shape = "square")
  nodes_df <- combine_nodes(latent, observed)
  # make EDF
  all_paths <- d %>%
    filter(op != "~1") %>%
    mutate(label = format(round(est, 2),nsmall=2)) %>%
    select(-est)
  loadings <- all_paths %>%
    filter(op == "=~") %>%
    mutate(from = lhs, to = rhs, style = "dashed") %>%
    select(from, to, style, label)
  regressions <- all_paths %>%
    filter(op == "~~") %>%
    mutate(to = lhs, from = rhs, style = "solid", dir = "both") %>%
    select(from, to, style, label, dir)
  predictions <- all_paths %>% 
    filter(op == "~") %>% 
    mutate(from = lhs, to = rhs, style = "solid") %>% 
    select(from, to, style)
  edges_df <- combine_edges(loadings, regressions, predictions)
  
  graph <- create_graph(
    nodes_df = nodes_df,
    edges_df = edges_df,
    graph_attrs = c("layout = 'dot'")
  )
  return(graph)
}
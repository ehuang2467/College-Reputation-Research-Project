source("code/fxs.R")
library(GGally)
nat = get_usnews("national")

for(v in vars){
  ggplot_nat = ggplot(nat, aes(nat %>% pull(v), log_rank), stat = "count") +
    labs(subtitle = "Dataset: US News National University Ranking",
         x = v, y = "log Rank")
  if(var_is_categorical[vars == v]){
    ggplot_nat + geom_boxplot()
      
  }
  else{
    ggplot_nat + geom_point()
  }
  
  ggsave(str_c("plots/", v, "_vs_log_rank_national.jpg"),
         width = 5, height = 5)
}

ggpairs(nat %>% select(vars))

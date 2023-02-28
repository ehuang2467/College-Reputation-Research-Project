library(rvest)
library(tidyverse)
state_list = c("Connecticut", "Maine", "Massachusetts", "Pennsylvania", "Rhode_Island", "Vermont")
html_base = "https://en.wikipedia.org/wiki/List_of_colleges_and_universities_in_"

state_tables_list = list()
for(i in 1:length(state_list)){
  html = paste(html_base, state_list[i], sep = "") %>% read_html()
  table = html %>% html_element("table") %>% html_table()
  state_tables_list[[state_list[i]]] = table
         
}

state_tables_list[["Massachusetts"]] = state_tables_list[["Massachusetts"]][,-7]
state_tables_list[["Pennsylvania"]] = state_tables_list[["Pennsylvania"]][,-3]


  copy = x
  colnames(copy) = c("School",
               "Location",
               "Control",
               "Type",
               "Enrollment",
               "Founded")
  return(copy)
})

states_combined = do.call(rbind, state_tables_list)

logit = function(x) {
  log(x / (1 - x))
}

table(states_combined$Type)
states_combined$Type_BMD = as.factor(ifelse(
  grepl("Baccalaureate|Master|Doctoral|research", states_combined$Type, ignore.case = T) & 
    !grepl("Associate", states_combined$Type, ignore.case = T),
  "Bac_Mast_Doc", "Other"))

states_combined$Type_Research = as.factor(ifelse(
  grepl("Doctoral|research", states_combined$Type, ignore.case = T),
  "research", "non_research"))

type = ifelse(states_combined$Type_BMD == "Bac_Mast_Doc", 1, 0)

type_regr = lm(type ~ as.numeric(Founded), data = states_combined, 
               subset = (!is.na(as.numeric(states_combined$Founded))))
summary(type_regr)
#the slope of founded implies that if a

is_research = ifelse(states_combined$Type_Research == "research", 1, 0)
research_regr = lm(is_research ~ as.numeric(Founded), data = states_combined, 
                   subset = (!is.na(as.numeric(states_combined$Founded))))
summary(research_regr)
#concl: an older founding date is highly significant in correlating with a Baccalaureate|Master|Doctoral institution

#next: subset to only BMD colleges, and regress founding on logit US News rank

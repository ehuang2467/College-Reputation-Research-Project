library(rvest)
library(geosphere)
library(RSelenium)
source("code/fxs.R")

#require: first arg is the name of the data set to prep
#this data set name will be used for the RDS object, and html list object
#second arg is T to scrape, or F to not scrape us news
args = commandArgs(trailingOnly=TRUE)

#remember to get the html from the us news table view

regions = c("north", "south", "west", "midwest")
data_names = c("national",
               "liberal",
               str_c("regcol_", regions),
               str_c("reguni_", regions)) %>% 
  str_c("usnews", ., "2022", sep = "_")

arg_name = args[1]
scrape_usnews = "scrape_usnews" %in% args
read_govt_scorecard = "read_govt_scorecard" %in% args
read_state_demos = "read_state_demos" %in% args
read_state_caps = "read_state_caps" %in% args

if(read_govt_scorecard){
  govt_scorecard =
    read_csv("data/Govt College Scorecard/Most-Recent-Cohorts-Institution.csv") %>%
    select(
      INSTNM,
      CITY,
      STABBR,
      CONTROL,
      CCBASIC,
      LOCALE,
      HBCU:RELAFFIL,
      LATITUDE:LONGITUDE,
      CURROPER,
      SATMTMID,
      SATVRMID,
      UNITID
    ) %>%
    mutate(State = state_abb_to_name(STABBR) ) %>%
    rename(School = INSTNM)
  
  write_rds(govt_scorecard, "data/govt_scorecard.RDS")
}
govt_scorecard = read_rds("data/govt_scorecard.RDS")

if(read_state_demos){
  #read in state demographics, turn race % into absolute count, join to usnews
  state_demos = read_csv("data/state_demographics.csv")
  state_demos = state_demos %>%
    rename("population_2014" = "Population.2014 Population") %>%
    rename_with(
      ~ .x %>% str_replace_all("Ethnicities\\.| Alone|,", "") %>%
        str_replace_all("[:punct:]|[:blank:]", "_") %>% str_c("_count"),
      contains("Ethnicities")
    )

  PR_demos = c(3325642, 3214, 151, 2189, 24, 3404, 3212625, 31715)

    
  state_demos = state_demos %>% 
    mutate(across("Black_count":"White_not_Hispanic_or_Latino_count", 
                  ~ (.x * population_2014/100) %>% round())) %>% 
    select(State, population_2014, 
           Black_count:White_not_Hispanic_or_Latino_count) %>% 
    rbind(c("Puerto Rico", PR_demos)) %>% 
    mutate(across(-State,~.x %>% as.numeric() %>% magrittr::divide_by(1e6)),
           sqrt_pop = sqrt(population_2014)) %>% 
    rename(pop = population_2014)
  write_rds(state_demos, "data/state_demos.RDS")
}
state_demos = read_rds("data/state_demos.RDS")

if(read_state_caps){
  state_capital_locations = read_csv("data/us-state-capitals.csv") %>% 
    rename(State = name, 
           capital_latitude = latitude, 
           capital_longitude = longitude,
           capital = description)
  
  state_cap_loc_nonstates = tribble(
    ~State,  ~capital, ~ capital_latitude, ~ capital_longitude,
    "District of Columbia", "District of Columbia", 38.904722, -77.016389,
    "Puerto Rico", "San Juan", 18.406389, -66.063889,
    "Virgin Islands", "Charlotte Amalie", 18.35,  -64.95
    
  )
  
  state_capital_locations = bind_rows(
    state_capital_locations, 
    state_cap_loc_nonstates)
  
  write_rds(state_capital_locations, "data/state_caps.RDS")
}
state_capital_locations = read_rds("data/state_caps.RDS")

#returns null, does all data prep as side effect for data name dn
data_prep = function(data_name){
  html_name = str_c("data/", data_name, "_html.txt")
  scrape_name = str_c("data/", data_name, "_scrape.RDS")
  final_name = str_c("data/", data_name, ".RDS")
  
  #should probably make into external csv file
  missing_founded = read_csv("data/missing_founded.csv",
                             col_types = cols(School = col_character(),
                                              Founded = col_double()))
  
  if(scrape_usnews){
    html = read_file(html_name) %>% 
      str_c("<!DOCTYPE html>\n<html>\n<body>\n", ., "\n</body>\n</html>")
    urls = read_html(html) %>%
      html_elements("h3") %>%
      html_elements("a") %>%
      html_attr("href") %>%
      str_c("https://www.usnews.com", .) 
    
    usnews_scraped = us_news_scraper(urls)
    
    write_rds(usnews_scraped, scrape_name)
  }
  
  usnews_scraped = read_rds(scrape_name)
  #fill in missing Founding values
  usnews_scraped = usnews_scraped %>% 
    mutate(Founded = fill_in_missing(School, Founded, missing_founded))
  if(usnews_scraped$Founded %>% is.na() %>% any()){
    cat(usnews_scraped %>% filter(is.na(Founded)) %>% select(School) %>% pull(),
        sep = "\n")
    stop("missing founded date")
  }
  
  usnews_scraped = left_join(usnews_scraped,
                             govt_scorecard %>% select(CONTROL:UNITID),
                             by = c("ID" = "UNITID"))
  
  usnews_final = usnews_scraped %>% 
    add_state_demos() %>% 
    add_rank_age() %>% 
    add_control() %>% 
    add_type() %>% 
    add_locale() %>% 
    add_special_purpose() %>% 
    add_off_mainland() %>% 
    add_at_capital() %>% 
    add_is_religious() %>% 
    add_SAT()

  write_rds(usnews_final, final_name)
  return(NULL)
}


if(arg_name != "all"){
  null = data_prep(arg_name)
} else{
  null = map(data_names, data_prep)
}



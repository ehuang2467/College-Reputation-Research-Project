suppressPackageStartupMessages({library(tidyverse); library(ordinal)})

#is as.numeric(x) if x is string of 4 digits, else is NA
num_or_na = function(x){
  if(!is.na(x) & str_detect(x, "[:digit:]{4}"))
    return(x %>% as.numeric())
  return(NA)
}

#is abb_vec with each entry converted from state/territory abbreviation to full name
#req: each entry in abb_vec is a valid state/territory abbreviation
state_abb_to_name = function(abb_vec) {
  territory_names = c("District of Columbia",
                      "American Somoa", 
                      "Guam", 
                      "Puerto Rico",
                      "Northern Mariana Islands",
                      "Micronesia",
                      "Marshall Islands",
                      "Palau",
                      "Virgin Islands")
  territory_abb = c("DC", "AS", "GU", "PR", "MP", "FM", "MH", "PW", "VI")
  state_name = c(state.name, territory_names)
  state_abb = c(state.abb, territory_abb)
  
  return(map_chr(abb_vec, ~state_name[state_abb == .x]))
}

#names and orig_values are equal length and are in order with each other
#fill_in_values is a named vector
#returns orig_values with values from fill_in_values filled in for positions
#such that names matches the names of fill_in_values
fill_in_missing = function(names, orig_values, fill_in){
  fill_in_values = fill_in$Founded
  fill_in_names = fill_in$School
  if(length(names) != length(orig_values))
    stop("invalid input for fill_in_missing")
  missing_names = names[is.na(orig_values)]
  if(length(missing_names) > 0){
    for(mn in missing_names){
      fill_in = ifelse(mn %in% fill_in_names,
                       fill_in_values[fill_in_names %in% mn],
                       NA)
      orig_values[names %in% mn] = fill_in
    }
  }
  return(orig_values)
}

#is tibble of School name, Founded date, unique ID, US News Rank string,
#City, State abbreviation, and State, for each usnews url in html_vec
us_news_scraper = function(html_vec, port_num = 4545L){
  #is a named list of the school information when Rsel browser is on specific
  #us news school page
  extractor = function(){
    source = remDr$getPageSource()[[1]] %>% 
      read_html() 
    name = source %>% 
      html_element("h1") %>% 
      html_text2()
    founding = source %>%
      html_element(".kQaJhn") %>%
      html_text() %>%
      str_extract("Founded.+Religious") %>%
      str_replace_all("Founded|Religious", "") %>%
      num_or_na()
    id = source %>% 
      html_elements("meta[property=data-school-xwalk-id]") %>% 
      html_attr("content") %>% 
      as.numeric()
    rank = source %>% 
      html_elements(
        "ul.RankList__List-sc-2xewen-0.ciVlaM.util__RankList-sc-1kd04gx-3.dteFWJ") %>% 
      html_element("li") %>% 
      html_element("a") %>% 
      html_element("div") %>% 
      html_text2()
    city = source %>% 
      html_element("span.Hide-kg09cx-0.fjAwZI") %>% 
      html_text2() %>% 
      str_extract("[:alpha:].+,") %>% 
      str_remove(",")
    state_ab = source %>% 
      html_element("span.Hide-kg09cx-0.fjAwZI") %>% 
      html_text2() %>% 
      str_sub(-2)
    
    school_info = list(School = name, 
                       Founded = founding, 
                       ID = id,
                       Rank = rank,
                       City = city,
                       State_ab = state_ab,
                       State = state_ab %>% state_abb_to_name())
    return(school_info)
  }
  prefs = list("profile.managed_default_content_settings.images" = 2L)
  cprof = list(chromeOptions = list(prefs = prefs))
  rD <- rsDriver(browser="chrome", 
                 port=port_num, 
                 verbose=T, 
                 chromever = "105.0.5195.19", 
                 geckover = NULL, 
                 phantomver = NULL,
                 extraCapabilities = cprof)
  remDr = rD[["client"]]
  wait_sec = 1
  remDr$setTimeout(type = "page load", milliseconds = wait_sec*1000)
  us_news_scrape = tibble(School = rep(NA, length(html_vec)),
                          Founded = rep(NA, length(html_vec)),
                          ID = rep(NA, length(html_vec)),
                          Rank = rep(NA, length(html_vec)),
                          City = rep(NA, length(html_vec)),
                          State_ab = rep(NA, length(html_vec)),
                          State = rep(NA, length(html_vec)))
  
  scrape_index = which(us_news_scrape$School %>% is.na())
  while (scrape_index %>% length() > 0) {
    for (i in scrape_index) {
      url = html_vec[i]
      remDr$deleteAllCookies()
      school_entry = tryCatch({
        tryCatch({
          print(url)
          remDr$deleteAllCookies()
          remDr$navigate(url)
          stop("navigate url done")
        },
        error = function(e) {
          print("stop loading")
          extracted = extractor()
          if ((map_dbl(extracted, ~length(.x)) == 1) %>% all() &
              (extracted$School == us_news_scrape$School) %>% sum(na.rm=T) == 0)
            return(extracted)
          else
            stop()
        })
      },
      error = function(e) {
        print("could not extract")
        remDr$deleteAllCookies()
        return(list(School = NA,
                    Founded = NA,
                    ID = NA,
                    Rank = NA,
                    City = NA,
                    State_ab = NA,
                    State = NA))
      })
      print("assignment")
      us_news_scrape[i,] = school_entry
      remDr$deleteAllCookies()
    }
    scrape_index = which(us_news_scrape$School %>% is.na())
  }
  if(us_news_scrape$School %>% unique() %>% length() != nrow(us_news_scrape))
    stop("repeated school")
  return(us_news_scrape)
}

#is the rank as numeric of y when y is string of form "#xxx"
#takes average when y is of form #xxx-xxx
rank_num = function(y){
  map_dbl(y, function(x){
    if(str_detect(x, "[:digit:]") == F)
      return(NA)
    x = x %>% 
      str_remove_all("#")
    if(str_detect(x, "-"))
      x %>% str_split("-") %>% .[[1]] %>% as.numeric() %>% mean()
    else
      x %>% as.numeric()
  })
}


#is control_vec with 1 -> Public, 2 -> Private, 3 -> For_profit (turned into Private), 
#as a factor (factor is unordered)
govt_control_to_factor = function(control_vec){
  c("Public", "Private", "Private")[control_vec] %>% factor()
}

#is type_vec with -3:0 -> NA, 1:14 -> Associate, 15:17 -> Doctoral, 
#18:20 -> Master, 21:32 -> Bachelor, 33 -> Tribal, as a factor
#(factor is ordered randomly)
govt_ccbasic_to_factor = function(type_vec){
  #assign errors to NA
  type_vec[!str_detect(type_vec, "[:digit:]")] = "0"
  type_vec = type_vec %>% as.numeric()
  #cut break intervals are (a,b]
  cut(type_vec, 
      breaks = c(-4, 0, 14, 17, 20, 32, 33),
      labels = c("NA", "Associate", "Doctoral", "Master", "Bachelor", "Tribal"),
      ordered_result = T) %>% 
    fct_relevel("NA", "Tribal", "Associate", "Bachelor", "Master", "Doctoral")
}

#is locale_vec with 11,21 -> Large_City, (12,13,22,23) -> Small_City,
#all else -> Rural/Town, as a factor (factor is ordered randomly)
govt_locale_to_factor = function(locale_vec){
  ifelse(locale_vec %in% c(11, 21),
         "Large_City",
         ifelse(locale_vec %in% c(12, 13, 22, 23),
                "Small_City",
                "Rural/Town"))
}

#is vector with entry True if any of the special_purpose columns in 
#usnews_data are "1", else is False
make_spec_purp_factor = function(usnews_data){
  special_purposes = c("HBCU", "TRIBAL", "MENONLY", "WOMENONLY")
  special_tib = usnews_data[,special_purposes]
  if(any(special_tib != "0" & special_tib != "1"))
    stop("unexpected values in make_spec_purp_factor")
  pmap_lgl(special_tib, ~ (.x == "1") %>% any())
}

meter_to_mile = function(x){
  x/1609.344
  #https://www.unitconverters.net/length/meters-to-miles.htm
}
add_state_demos = function(usnews_data){
  left_join(usnews_data, state_demos, by = "State")
}

#is vector of factors in which each rank in vector of ranks x is separated
#into intervals, with each successive interval being "interval" larger
#in range (ie 0 to 10, 10 to 30, 30 to 60, etc)
ord_factor_maker = function(x, interval = 10){
  breaks = 0
  while(max(breaks) < max(x, na.rm = T)){
    breaks = c(breaks, breaks[length(breaks)] + interval*length(breaks) )
  }
  a = cut(x, breaks = breaks, ordered_result = T) %>% fct_rev() 
  return(a)
}

add_rank_age = function(usnews_data){
  usnews_data %>% 
    mutate(Rank_num = rank_num(Rank),
           Age = (date() %>% str_sub(-4) %>% as.numeric()) - Founded,
           log_rank = log(Rank_num),
           Rank_fac = Rank_num %>% ord_factor_maker())
}
add_control = function(usnews_data){
  usnews_data %>% 
    rename(Control = CONTROL) %>% 
    mutate(Control = Control %>% govt_control_to_factor())
}
add_type = function(usnews_data){
  usnews_data %>% 
    rename(Type = CCBASIC) %>% 
    mutate(Type = Type %>% govt_ccbasic_to_factor())
}
add_locale = function(usnews_data){
  usnews_data %>% 
    rename(Locale = LOCALE) %>% 
    mutate(Locale = Locale %>% govt_locale_to_factor())
}
add_special_purpose = function(usnews_data){
  usnews_data %>% 
    mutate(SpecialPurpose = make_spec_purp_factor(usnews_data))
}
add_off_mainland = function(usnews_data){
  mainland_states = state.abb %>% 
    .[!state.abb %in% c("AK", "HI")] %>% 
    append("DC")
  usnews_data %>% 
    mutate(OffMainland = !(State_ab %in% mainland_states))
}
add_at_capital = function(usnews_data){
  usnews_data %>% 
    mutate(LONGITUDE = LONGITUDE %>% as.numeric(),
           LATITUDE = LATITUDE %>% as.numeric()) %>% 
    left_join(state_capital_locations,
              by = "State") %>% 
    mutate(dist_to_cap = distGeo(cbind(LONGITUDE, LATITUDE), 
                                 cbind(capital_longitude, capital_latitude)) %>% 
             meter_to_mile()) %>% 
    mutate(at_capital = dist_to_cap < 15)
}

add_is_religious = function(usnews_data){
  usnews_data %>% 
    mutate(is_religious = !(RELAFFIL %in% c("NULL", "-1", "-2")))
}

add_SAT = function(usnews_data){
  null2na = function(x) ifelse(x == "NULL", NA, x)
  usnews_data %>% 
    mutate(SATMTMID = SATMTMID %>% null2na() %>% as.numeric(),
           SATVRMID = SATVRMID %>% null2na() %>% as.numeric(),
           SAT = SATMTMID + SATVRMID,
           SAT_scaled = SAT %>% scale() %>% as.numeric())
}


#make a clm model, removing variables that would make a bad Hessian
#i.e. variables with low counts

#Note: seems like clm has hard time with Age^2, wants scale(Age)
#haven't implemented a fix for this yet
make_clm = function(data, y_var, x_vars, force = F, name = "", no_print = F){
  interaction_vars = x_vars %>% keep(~str_detect(.x, ":"))
  poly_vars = x_vars %>% keep(~str_detect(.x, "\\^"))
  x_vars = x_vars %>% discard(~str_detect(.x, ":|\\^"))
  x_vars = x_vars %>% 
    discard(~data %>% pull(.x) %>% unique() %>% length() == 1)
  model_formula = str_c(x_vars, collapse = " + ") %>% 
    str_c(y_var, ., sep = " ~ ") %>% 
    str_c(., interaction_vars, poly_vars, sep = " + ")
  if(force)
    return(clm(model_formula, data = data))
  return(tryCatch(clm(model_formula, data = data %>% as.data.frame()),
                  warning = function(w){
                    if(w %>% as.character() %>% str_detect("Hessian") %>% isFALSE())
                      return(clm(model_formula, data = data))
                    x_vars =
                      x_vars %>% 
                      discard(~ data %>% pull(.x) %>% is.na() %>% mean() > .20)
                    cat_x_vars = x_vars %>% 
                      discard(~ data %>% pull(.x) %>% class() == "numeric")
                    xvar_mins = cat_x_vars %>% 
                      map_dbl(~ data %>% pull(.x) %>% table() %>% min())
                    
                    weak_var = cat_x_vars %>% 
                      .[which.min(xvar_mins)]
                    weak_var_vec = weak_var %>% 
                      pull(data, .)
                    
                    if(weak_var_vec %>% unique() %>% length() > 2){
                      var_to_remove = 
                        names(table(weak_var_vec))[which.min(table(weak_var_vec))]
                      cat(str_c("Removing ", var_to_remove, "\n"))
                      data = data %>% filter(weak_var_vec != var_to_remove)
                    }
                    else{
                      x_vars = x_vars[-which(x_vars == weak_var)]
                      interaction_vars = interaction_vars %>% 
                        discard(~any(str_detect(.x, weak_var)))
                      poly_vars = poly_vars %>% 
                        discard(~any(str_detect(.x, weak_var)))
                      if(name == "" & !no_print){
                        cat(str_c("Removing ", weak_var, "\n"))
                      } else if(name != "" & !no_print){
                        cat(str_c("Removing ", weak_var, " from ", name, "\n"))
                      }
                    }
                    return(make_clm(data, y_var, c(x_vars, interaction_vars, poly_vars), name = name))
                  }))
}
printt = function(x){
  print(x)
  x
}


fac_diff = function(pred, obs, lvls){
  which2 = function(z){
    which_return = which(z, arr.ind = T)
    if(length(which_return) == 1)
      return(which_return)
    else
      return(NA)
  }

  map2_dbl(pred, obs, ~which2(.x == lvls) - which2(.y == lvls))
}

predict_safe = function(model, data, type = "class"){
  tryCatch(predict(model, newdata = data, type = type)[["fit"]],
           error = function(e){
             NA
           })
}
predict2 = function(model, newdata, type = "class", is_fac){
  return_preds = rep(NA, nrow(newdata))
  for(i in 1:nrow(newdata)){
    dat = newdata[i,]
    if(is_fac){
      return_preds[i] = predict_safe(model, dat, type = type) %>% as.character()
    }
    else{
      return_preds[i] = predict_safe(model, dat, type = type)
    }
  }
  return(return_preds)
}

#adds residuals and predictions form clm model to data
add_clm_res = function(data, model, response = "Rank_fac"){
  lvls = model["y.levels"] %>% .[[1]]
  is_fac = (response == "Rank_fac")
  data = data %>%
    mutate(pred = predict2(model, newdata = data, type = "class", is_fac),
           resid = fac_diff(pred, Rank_fac, lvls))

}

regions = c("north", "south", "west", "midwest")
data_names = c("national",
               "liberal",
               str_c("regcol_", regions),
               str_c("reguni_", regions))

usnews_string = function(data_name, year = "2022"){
  str_c("data/usnews_", data_name,"_", year, ".RDS")
}

get_usnews = function(data_name, year = "2022"){
  tryCatch(usnews_string(data_name, year) %>% read_rds(),
           error = function(e){
             read_rds(data_name)
           })
  
}

get_all_usnews = tibble(
  list_name = data_names,
  school_data = map(data_names, get_usnews)
)

vars = c("Age", "Control", "pop", "SpecialPurpose", "Locale",
         "OffMainland", "is_religious", "at_capital")

is_char_logic_fac = function(x) is.character(x) || is.logical(x) || is.factor(x)

var_is_categorical = vars %>% map_lgl(~get_usnews("national") %>% 
                                    pull(.x) %>% 
                                    is_char_logic_fac())

y_var = "Rank_fac"

model_formula = str_c(vars, collapse = " + ") %>% 
  str_c(y_var, ., sep = " ~ ")

model_formula_lm = str_c(vars, collapse = " + ") %>% 
  str_c("log_rank", ., sep = " ~ ")

all_us_combined = 
  get_all_usnews %>%
  mutate(sd = map2(list_name, 
                   school_data, 
                   ~.y %>% mutate(usnews_list = .x))) %>% 
  pull(sd) %>% 
  map(~.x %>% select(-Rank_fac)) %>% 
  bind_rows() %>% 
  mutate(usnews_type = 
           usnews_list %>% 
           str_remove_all("_.*") %>% 
           factor(levels = c("regcol", "reguni", "liberal", "national"), 
                  ordered = T) )

#Manual NA data fix using Google
all_us_combined[
  all_us_combined$School == "University of Saint Katherine","Type"] = "Bachelor"

#https://www.usnews.com/education/best-colleges/articles/ranking-category-definitions
#!!!!

#I'm pretty sure any non-Doctoral Type is a misclassification
all_us_combined[all_us_combined$usnews_list == "national","Type"] = "Doctoral"

all_us_combined[all_us_combined$usnews_list %>% str_detect("reguni"),"Type"] = 
  "Master"

all_us_combined[all_us_combined$usnews_list == "liberal","Type"] = "Bachelor"



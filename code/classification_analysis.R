source("code/fxs.R")
library(gridExtra)
#for analysis of classification as the response variable

all_us_combined %>% count(usnews_list, Type) %>% print(n=Inf)

#manual inspection of CCBASIC in govt scorecard
#seems inconsistent with the data online
  

type_model = make_clm(all_us_combined, "Type", vars)

type_model %>% summary()

usnews_type_model = make_clm(all_us_combined, "usnews_type", vars)

usnews_type_model %>% summary()

all_us_combined %>% 
  count(Control, Type)

#!!! These visualizations can be added to the report maybe?


type_public = all_us_combined %>% 
  filter(Control == "Public") %>% 
  count(Type) %>% 
  mutate(Type_prop = n/sum(n)) %>% 
  ggplot(aes(Type, Type_prop)) +
  geom_col() + 
  labs(subtitle = "Control: Public")

type_private = all_us_combined %>% 
  filter(Control == "Private") %>% 
  count(Type) %>% 
  mutate(Type_prop = n/sum(n)) %>% 
  ggplot(aes(Type, Type_prop)) +
  geom_col() + 
  labs(subtitle = "Control: Private")

grid.arrange(type_public, type_private)

all_us_combined %>% 
  ggplot(aes(Type, fill = Control, group = Control)) +
  geom_bar(position = position_dodge())

all_us_combined %>% 
  ggplot(aes(Type, fill = Locale, group = Locale)) +
  geom_bar(position = position_dodge())




all_us_combined %>% 
  ggplot(aes(Type, fill = SpecialPurpose, group = SpecialPurpose)) +
  geom_bar(position = position_dodge())


make_clm(all_us_combined %>% filter(Age > 150), "Type", vars) %>% 
  summary()

make_clm(all_us_combined %>% filter(Age <= 150), "Type", vars) %>% 
  summary()

make_clm(all_us_combined %>% filter(Age > 150), "usnews_type", vars) %>% 
  summary()

make_clm(all_us_combined %>% filter(Age <= 150), "usnews_type", vars) %>% 
  summary()

make_clm(all_us_combined %>% filter(Age <= 150), "usnews_type", vars) %>% 
  step()

step(clm(model_formula %>% str_replace("Rank_fac", "usnews_type"), 
    data = all_us_combined %>% filter(!is.na(Ra))))

all_us_combined %>% filter(Age > 150) %>% 
  ggplot(aes(Age, Type, color = Control)) +
  geom_jitter(height = .3)

all_us_combined %>% filter(Age <= 150) %>% 
  ggplot(aes(Age, Type, color = Control)) +
  geom_jitter(height = .3)

all_us_combined %>% filter(Age <= 150) %>% 
  ggplot(aes(Age, Type)) +
  geom_boxplot()

all_us_combined %>% filter(Age > 150) %>% 
  ggplot(aes(Age, usnews_type, color = Control)) +
  geom_jitter(height = .3)

all_us_combined %>% 
  ggplot(aes(Age, usnews_type, color = Control)) +
  geom_jitter(height = .3)

all_us_combined %>% 
  ggplot(aes(Age, usnews_type, color = Control)) +
  geom_boxplot()

all_us_combined %>% 
  ggplot(aes(Age, Control, color = Control)) +
  geom_boxplot() +
  geom_jitter(height = .3)

lm(Age ~ Control, data = all_us_combined) %>% summary()
t.test(all_us_combined %>% filter(Control == "Public") %>% pull(Age),
       all_us_combined %>% filter(Control == "Private") %>% pull(Age))





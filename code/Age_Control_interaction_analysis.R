source("code/fxs.R")

all_us_combined %>% 
  ggplot(aes(Age, Type, color = Control, group = Control)) +
  geom_jitter(height = .3) +
  geom_smooth(method = "lm")

all_us_combined %>% 
  ggplot(aes(Type, Age, color = Control)) +
  geom_boxplot()


all_us_combined %>% 
  filter(Age > 100) %>% 
  make_clm("Type", vars %>% c(., "Age:Control")) %>% 
  summary()

all_us_combined %>% 
  ggplot(aes(Age, usnews_type, color = Control, group = Control)) +
  geom_jitter(height = .3) +
  geom_smooth(method = "lm")

all_us_combined %>% 
  ggplot(aes(usnews_type, Age, color = Control)) +
  geom_boxplot()



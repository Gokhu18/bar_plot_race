library(tidyverse)

# read data
read.csv("data/1_raw/causas_muerte.csv", stringsAsFactors = F) %>% 
  as_tibble() %>% 
  select(1) -> causa_muerte
readr::read_csv("data/1_raw/causas_muerte.csv") %>% 
  mutate(causa1 = causa_muerte$causa) %>% 
  select(-1) %>% 
  select(causa1, everything()) -> causa_muerte 

# fix data
causa_muerte %>% 
  mutate(causa1 = c(
    substring(causa_muerte$causa1[1:10], 3),
    substring(causa_muerte$causa1[11:74], 4)
  )
  ) %>% 
  slice(2:68) %>% 
  filter(!(causa1 == "Resto de causas" | causa1 == "Causas mal definidas")) %>% # causas específicas
  mutate(causa1 = as.factor(causa1)) %>% 
  pivot_longer(-causa1, names_to = "años", values_to = "muertes") %>% 
  mutate(años = as.numeric(años)) %>% 
  arrange(años) -> causa_muerte 

save(causa_muerte, file = "data/2_processed/causa_muerte.Rdata")
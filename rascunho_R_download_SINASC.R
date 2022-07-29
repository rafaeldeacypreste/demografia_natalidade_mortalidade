library(elastic)




#---------------------------------------------------------------------------------
library(microdatasus)
library(tidyverse)


dados_sinasc <- fetch_datasus(
  year_start         = 2000,
  year_end           = 2021,
  uf                 = "PE",
  information_system = "SINASC"
)


dados_sinasc %>% 
  mutate(DTNASC = str_sub(DTNASC, -4, -1)) %>% 
  filter(DTNASC == 2020) %>% 
  count(CODMUNRES, IDADEMAE) %>% 
  filter(CODMUNRES == 260005)

dados_sinasc %>% 
  mutate(DTNASC = str_sub(DTNASC, -4, -1)) %>% 
  count(DTNASC, IDADEMAE)



# https://github.com/rfsaldanha/microdatasus/wiki/Conven%C3%A7%C3%B5es-SINASC

dados_sinasc <- process_sinasc(dados_sinasc)





dados_sim <- fetch_datasus(
  year_start         = 2000,
  year_end           = 2021,
  uf                 = "PE",
  information_system = "SIM-DO"
)

# https://github.com/rfsaldanha/microdatasus/wiki/Conven%C3%A7%C3%B5es-SIM

dados_sim <- process_sim(dados_sim)

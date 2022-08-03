#___ Passos Iniciais ----
setwd("C:\\Users\\thier\\Desktop\\Trabalho Demografia\\Banco Declaração de óbito 00-19")
pacman::p_load(read.dbc,foreign, tidyverse, LexisPlotR, lubridate)

#___ Banco de nascimentos ----
nasc <- data.frame(ano = as.character(c(2000,2001,2002,2003,2004,2005,2006,2007,2008,
                                        2009,2010,2011,2012,2013,2014,2015,2016,2017,
                                        2018,2019,2020)),
                   numero = c(58615,58588,56332,55105,54747,56866,55342,53214,52664,
                              50996,49424,50144,47962,46419,47941,49253,46986,48551,
                              49490,47933,45128))
DNBR <- read.dbf("DNBR20DA.dbf")
DNBRPI2020 <- DNBR %>% mutate(cod = str_sub(CODMUNRES,1,2)) %>%
  filter(cod == 22)
nrow(DNBRPI2020)

#___ Banco de mortos ----
# Arrumando banco de 2020
DOBR <- read.dbf("DOBR20DA.dbf")
DOPI2020 <- DOBR %>% mutate(cod = str_sub(CODMUNRES,1,2)) %>%
  filter(cod == 22)

# Lendo e juntando bancos de 2000 a 2020
DOPI <- read.dbc("DOPI2000.dbc")

for(i in 2000:2019){
  DODdescartavel <- read.dbc(paste0("DOPI",i,".dbc"))
  DOPI <- full_join(DOPI, DODdescartavel)
}
DOPI <- full_join(DOPI,DOPI2020)

# Limpando o banco
DOPI$ano_nasc <- str_sub(DOPI$DTNASC,5,8)
DOPI$ano_obt <- str_sub(DOPI$DTOBITO, 5,8)

tabela <- DOPI %>% mutate(idade_arrumada = case_when(str_sub(IDADE,1,1)<4 ~ 0,
                                                     str_sub(IDADE,1,1)==4 ~ 0+as.numeric(str_sub(IDADE,2)),
                                                     str_sub(IDADE,1,1)==5 ~ 100+as.numeric(str_sub(IDADE,2)))) %>%
  mutate(idade_certa = floor(as.numeric(difftime(dmy(DTOBITO), dmy(DTNASC), units = "days")/365))) %>%
  select(ano_nasc, ano_obt, idade_certa, IDADE) %>%
  filter(idade_certa < 5) %>%
  group_by(ano_nasc, ano_obt, idade_certa) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  distinct(idade_certa, ano_nasc, ano_obt, .keep_all = T) %>%
  arrange(ano_obt, desc(ano_nasc), idade_certa) %>%
  mutate(TRI = case_when(as.numeric(ano_obt) - as.numeric(ano_nasc) == as.numeric(idade_certa) ~ 0,
                         as.numeric(ano_obt) - as.numeric(ano_nasc) != as.numeric(idade_certa) ~1))

#___ Plotando diagrama de Lexis ----

diagrama_pi <- lexis_grid(year_start = 1995, year_end = 2021, age_start = 0, age_end = 5) + 
  annotate(geom="text", x=as.Date(paste0(tabela$ano_obt[tabela$TRI==1]
                                         ,"-05-07"))
           ,y=tabela$idade_certa[tabela$TRI==1]+0.75,
           label=c(tabela$n[tabela$TRI==1]), size = 4,
           color="black", fontface = "bold") +
  annotate(geom = "text", x= as.Date(paste0(nasc$ano,"-08-10")),
           y= 0.15,
           label=nasc$numero,size = 4 ,color = "red", fontface = "bold") +
  annotate(geom="text", x=as.Date(paste0(tabela$ano_obt[tabela$TRI==0]
                                         ,"-10-01"))
           ,y=tabela$idade_certa[tabela$TRI==0]+0.4,
           label=c(tabela$n[tabela$TRI==0]), size = 4,
           color="black", fontface = "bold") +
  labs(title = "Diagrama de Lexis",
       subtitle = "- Óbitos e Nascimentos vivos de 2000 a 2020, Piauí",
       caption = "Fonte: Elaboração própria, dados DATASUS, 2000-2020",
       x = 'Ano',
       y = 'Idade')
diagrama_pi

#___ Calculando probabilidades de sobrevivência ----

prob_sob_5 <- function(mort, fec, coorte){
  return(1 - (sum(mort$n[mort$ano_nasc == coorte]))/fec$numero[fec$ano == coorte])
}
for (i in 2000:2015){
  a <- prob_sob_5(tabela,nasc,i)
  print(paste0(a," ",i))
}

prob_sob_1(tabela,nasc,2000)


prob_sob_1 <- function(mort, fec, coorte){
  mort <- filter(mort, idade_certa == 0)
  return(1 - (sum(mort$n[mort$ano_nasc == coorte]))/fec$numero[fec$ano == coorte])
}

for (i in 2000:2019){
  c <- prob_sob_1(tabela,nasc,i)
  print(paste0(c," ",i))
}
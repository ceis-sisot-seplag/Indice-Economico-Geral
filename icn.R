#### carrega pacotes e recursos necessarios para o calculo #####

require(tidyverse)
library(readxl)

source("funcoes.R")

anos = "2021"
dados <- read_delim("icn/consulta35656087.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

### carregar os dados ####

rais2020 <- read_excel("C:/Users/lucascastelo/Downloads/rais2020.xlsx")
rais2021 <- read_excel("C:/Users/lucascastelo/Downloads/rais2021.xlsx")
rais2023 <- read_excel("C:/Users/lucascastelo/Downloads/rais2023.xlsx")
rais2022 <- read_excel("C:/Users/lucascastelo/Downloads/consulta12769704h.xlsx")

cat = read_excel("categoria2.xlsx", sheet = "Plan1")

### verificar presença de cnaes nao categorizadas ####

rais2023 %>% mutate(cnae = `CNAE 2.0 Subclasse`) %>% select(cnae) %>% 
  left_join(cat,by = c("cnae" = "subclasse")) %>%
  filter(is.na(setor)) %>% select(1)

#### transformacao da tabela para formato trabalhavel #####


dados2 = get(paste0("rais",anos)) %>% 
  pivot_longer(cols = 2:ncol(.),
               names_to = "municipio",
               values_to = "valores") %>% 
  mutate(muncod = str_sub(municipio, end = 6),
         munname =  str_sub(municipio, start = 11),
         classcod = str_sub(`CNAE 2.0 Subclasse`, end = 7),
         classname = str_sub(`CNAE 2.0 Subclasse`, start = 9),
         cnae = `CNAE 2.0 Subclasse`) %>% select(-1)


#### juncao da tabela com a categorização por setor e segmento ####

base = dados2 %>% 
  left_join(cat,by = c("cnae" = "subclasse")) %>% 
  aggregate(valores ~ municipio + classe + setor,.,sum) %>% 
  filter(!classe %in% c("NA","Total")) %>% 
  filter(municipio != "Total")

#### verificação da representação ####

base %>% count(classe)
base %>% count(setor)

#### começa do calculo dos indicadores ####
### quoeficiente locacional ###
## hhm ###
## participacao relativa ##

icn1 = base %>% 
  group_by(municipio) %>% 
  mutate(fj = sum(valores)) %>% 
  ungroup() %>% 
  group_by(classe) %>% 
  mutate(fimt = sum(valores)) %>% 
  ungroup() %>% 
  mutate(fmt = sum(valores)) %>% 
  mutate(ql = (valores/fj)/(fimt/fmt),
         hhm = ((valores/fimt)-(fj/fmt)),
         pr = (valores/fimt))

#### preparacao E calculo dos pesos por ACP ####  

icn2pcs = icn1 %>% select(classe,ql,hhm,pr)
segmento = icn2pcs$classe %>% unique

pesos = data.frame()
for (i in segmento) {
  pca = psych::principal(icn2pcs %>% filter(classe == i) %>% select(-1) %>% scale(),
                         rotate = "varimax", nfactors = 3, scores = F, oblique.scores = T)
  mav = as.data.frame(unclass(pca$Vaccounted)) #matrixautovalores
  ppvar = t(mav[2,]) #proporcao da variancia
  
  
  ld <-as.data.frame(unclass(pca$loadings)) #loadings
  mn = t(t(ld)/rowSums(t(ld))) #matrixnormal
  
  theta = mn%*%ppvar

  pesos = rbind(pesos,data.frame(classe = i,
                                 theta1 = theta[1],
                                 theta2 = theta[2],
                                 theta3 = theta[3]))
  
}

pesos

#### calculo do ICN ####

icn2 = icn1 %>% 
  left_join(pesos) %>% 
  mutate(icn = (ql*theta1) + (hhm*theta2) + (pr*theta3)
         )

icn2 %>% filter(icn > 30) # VERIFICA INCONSISTENCIAS

#### CATEGORIZAÇÃO DOS INDICADORES ####

icn3 = icn2 %>% 
  mutate(catSegmentos = case_when(
  ql<1 & hhm < 0 & pr >=0.1 ~ "Declinio",
  ql<1 & hhm < 0 & pr <0.1 ~ "Estagnado",
  ql>=1 & hhm >= 0 & pr >=0.1 ~ "Dinamico",
  ql>=1 & hhm >= 0 & pr < 0.1 ~ "Expansão",
  T ~ "Não Categorizado"
)) %>%
  group_by(classe) %>% 
  mutate(catICN = case_when(
    icn > mean(icn) & icn > 1 ~ "Especializado",
    icn > mean(icn) & icn < 1 ~ "Diferenciado",
    icn < mean(icn) ~ "Sem Grau de Especialização"
  )) %>% 
  ungroup() 

### NORMALIZACAO DOS INDICADORES ####

icnt = icn3 %>% count(municipio,catICN) %>% 
  pivot_wider(names_from = "catICN", values_from = "n") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(ISDE = (Diferenciado + Especializado) / 37) %>% 
  mutate(ISDEn = (ISDE - min(ISDE))/(max(ISDE)- min(ISDE)))

icni = icn3 %>% count(municipio,catSegmentos) %>% 
  pivot_wider(names_from = "catSegmentos", values_from = "n") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(ISDI = (Dinamico + Expansão) / 37) %>% 
  mutate(ISDIn = (ISDI - min(ISDI))/(max(ISDI)- min(ISDI)))

icn3 %>% count(municipio,catSegmentos) %>% 
  pivot_wider(names_from = "catSegmentos",values_from = "n")

#### exportacao #####

require(writexl)
tabs = list(ICN = icn3,
            ISDE = icnt,
            ISDI = icni)
write_xlsx(tabs, paste0("saida/icns",anos,".xlsx"))







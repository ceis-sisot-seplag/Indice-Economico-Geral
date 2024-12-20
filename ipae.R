### IPAE ### 

require(readxl)
require(tidyverse)
#sidrar::info_sidra(3939,wb = T)

# é carregado os dados do ibge, em seguida filtrado os segmentos que serão levados em conta
# apartir dai é calculado os pesos com os valores e produção e somas a produção para o valor
# do indicador final de cada segmento
#### IPP ####

vbp = readxl::read_excel("vbp2022.xlsx") %>% filter(Ano == anos) #planilha para calculo do valor do ipp
vbp = data.frame( "Tipo" = c("Bovino","Galináceos - total","Suíno - total"),
                 valor = c((vbp[1,2]+vbp[4,2])%>% pull,
                           (vbp[3,2]+vbp[5,2])%>% pull,
                           (vbp[2,2]) %>% pull)) %>% 
  mutate(peso = valor/sum(valor),
         valor = valor/1000) %>% 
  select(1,2,3)


ipp = sidrar::get_sidra(x = 3939,
                  variable = c(105),
                  period = anos,
                  geo = "City",
                  geo.filter = list(State = 51))

ipp2 = ipp %>% select(Município,Valor,`Tipo de rebanho`) %>%
  filter(`Tipo de rebanho` %in% c("Bovino",
                                  "Suíno - total",
                                  "Galináceos - total")) %>% 
  as_tibble() %>% 
  select(1,3,2) %>% 
  group_by(`Tipo de rebanho`) %>% 
  mutate(QtdRel = Valor/sum(Valor)) %>% 
  ungroup() %>% 
  left_join(vbp,by = c("Tipo de rebanho" = "Tipo")) %>% 
  mutate(val1 = QtdRel*peso) %>% 
  aggregate(val1 ~ Município,.,sum)
  
  




### isv ####

isv = sidrar::get_sidra(x = 291,
                        variable = c(142,143),
                        period = anos,
                        geo = "City",
                        geo.filter = list(State = 51))



isv = isv %>% 
  select(Município,Valor,Variável,`Tipo de produto da silvicultura`) %>% 
  filter(`Tipo de produto da silvicultura` %in% c("1.1.1 - Carvão vegetal de eucalipto",
                                                  "1.2.1 - Lenha de eucalipto",
                                                  "1.2.3 - Lenha de outras espécies",
                                                  "1.3.2 - Madeira em tora para outras finalidades"
                                                  )) %>% as_tibble() %>% 
  pivot_wider(names_from = "Variável",values_from = "Valor")
  



isv2 = isv %>% select(-4) %>% 
  group_by(`Tipo de produto da silvicultura`) %>% 
  mutate(QtdRel = `Quantidade produzida na silvicultura`/sum(`Quantidade produzida na silvicultura`,na.rm = T)) %>%
  ungroup() %>% 
  left_join(isv %>% 
              group_by(`Tipo de produto da silvicultura`) %>% 
              summarise(soma = sum(`Valor da produção na silvicultura`,na.rm = T)) %>% 
              ungroup() %>% 
              mutate(pesos = soma/sum(soma)) %>% 
              select(1,3)) %>% 
  select(-3) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(val2 = QtdRel*pesos) %>% 
  aggregate(val2 ~ Município,.,sum)

### ict ####

ict = sidrar::get_sidra(x = 1612,
                        variable = c(214,215),
                        period = anos,
                        geo = "City",
                        geo.filter = list(State = 51)) %>% 
  filter(`Produto das lavouras temporárias` %in% c("Abacaxi*",
                                                         "Algodão herbáceo (em caroço)",
                                                         "Amendoim (em casca)",
                                                         "Arroz (em casca)",
                                                         "Batata-doce",
                                                         "Cana-de-açúcar",
                                                         "Feijão (em grão)",
                                                         "Girassol (em grão)",
                                                         "Mamona (baga)",
                                                         "Mandioca",
                                                         "Melancia",
                                                         "Melão",
                                                         "Milho (em grão)",
                                                         "Soja (em grão)",
                                                         "Sorgo (em grão)",
                                                         "Tomate"
                                                          #,"Trigo (em grão)"
                                                         )) %>% 
  select(Município,Variável,Valor,`Produto das lavouras temporárias`)

ict %>% filter(Variável == "Valor da produção") %>% 
  group_by(`Produto das lavouras temporárias`) %>% 
  summarise(soma = sum(Valor,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pesos = soma/sum(soma)) %>% 
  select(1,3)

ict2 = ict %>% filter(Variável == "Quantidade produzida") %>% 
  group_by(`Produto das lavouras temporárias`) %>% 
  mutate(QtdRel = Valor/sum(Valor,na.rm = T)) %>%
  ungroup() %>%
  left_join(
    ict %>% filter(Variável == "Valor da produção") %>% 
      group_by(`Produto das lavouras temporárias`) %>% 
      summarise(soma = sum(Valor,na.rm = T)) %>% 
      ungroup() %>% 
      mutate(pesos = soma/sum(soma)) %>% 
      select(1,3)
  ) %>% 
  select(-3) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(val3 = QtdRel*pesos) %>% 
  aggregate(val3 ~ Município,.,sum)

### iev ####
  
iev =   sidrar::get_sidra(x = 289,
                          variable = c(144,145),
                          period = anos,
                          geo = "City",
                          geo.filter = list(State = 51)) %>% filter(`Tipo de produto extrativo` %in% c("1.3 - Castanha-do-pará",
                                                  "1.6 - Palmito",
                                                  "1.7 - Pequi (fruto)",
                                                  "2.1 - Ipecacuanha ou poaia (raiz)",
                                                  "3.2 - Hevea (látex coagulado)",
                                                  "7.1 - Carvão vegetal",
                                                  "7.2 - Lenha",
                                                  "7.3 - Madeira em tora",
                                                  "8.1 - Babaçu (amêndoa)",
                                                  "8.2 - Copaíba (óleo)",
                                                  "8.6 - Pequi (amêndoa)")) %>% 
  select(Município,Variável,Valor,`Tipo de produto extrativo`) %>% as_tibble()
  
iev2 = iev %>% filter(Variável == "Quantidade produzida na extração vegetal") %>% 
  group_by(`Tipo de produto extrativo`) %>% 
  mutate(QtdRel = Valor/sum(Valor,na.rm = T)) %>%
  ungroup() %>%
  left_join(
    iev %>% filter(Variável == "Valor da produção na extração vegetal") %>% 
      group_by(`Tipo de produto extrativo`) %>% 
      summarise(soma = sum(Valor,na.rm = T)) %>% 
      ungroup() %>% 
      mutate(pesos = soma/sum(soma)) %>% 
      select(1,3)
  ) %>% 
  select(-3) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(val4 = QtdRel*pesos) %>% 
  aggregate(val4 ~ Município,.,sum)

#### icp ####
### cafe total ###


icp = sidrar::get_sidra(x = 1613,
                          variable = c(214,215),
                          period = anos,
                          geo = "City",
                          geo.filter = list(State = 51)) %>% filter(`Produto das lavouras permanentes` %in% c("Açaí",
                                                         "Banana (cacho)",
                                                         "Borracha (látex coagulado)",
                                                         "Cacau (em amêndoa)",
                                                         "Café (em grão) Arábica",
                                                         "Café (em grão) Canephora",
                                                         "Castanha de caju",
                                                         "Coco-da-baía*",
                                                         "Goiaba",
                                                         "Guaraná (semente)",
                                                         "Laranja",
                                                         "Limão",
                                                         "Mamão",
                                                         "Manga",
                                                         "Maracujá",
                                                         "Palmito",
                                                         "Pimenta-do-reino",
                                                         "Tangerina",
                                                         "Urucum (semente)",
                                                         "Café (em grão) Total",
                                                         "Uva"
                                                         )) %>%
  select(Município,Variável,Valor,`Produto das lavouras permanentes`) %>% 
  as_tibble()

#### talvez uva e Café (em grão) Total

icp2 = icp %>% filter(Variável == "Quantidade produzida") %>% 
  group_by(`Produto das lavouras permanentes`) %>% 
  mutate(QtdRel = Valor/sum(Valor,na.rm = T)) %>%
  ungroup() %>%
  left_join(
    icp %>% filter(Variável == "Valor da produção") %>% 
      group_by(`Produto das lavouras permanentes`) %>% 
      summarise(soma = sum(Valor,na.rm = T)) %>% 
      ungroup() %>% 
      mutate(pesos = soma/sum(soma)) %>% 
      select(1,3)
  ) %>% 
  select(-3) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(val5 = QtdRel*pesos) %>% 
  aggregate(val5 ~ Município,.,sum)
 
### ipq ####
### com curimata,pirarucu,
### sem dourado
#sidrar::info_sidra(3940,wb = T)

ipq = sidrar::get_sidra(x = 3940,
                        variable = c(4146,215),
                        period = anos,
                        geo = "City",
                        geo.filter = list(State = 51)) %>% filter(`Tipo de produto da aquicultura` %in% c("Alevinos",
                                                       "Carpa",
                                                       "Curimatã, curimbatá",
                                                       "Jatuarana, piabanha e piracanjuba",
                                                       "Lambari",
                                                       "Matrinxã",
                                                       "Outros peixes",
                                                       "Pacu e patinga",
                                                       "Piau, piapara, piauçu, piava",
                                                       "Pintado, cachara, cachapira e pintachara, surubim",
                                                       "Pirapitinga",
                                                       "Tambacu, tambatinga",
                                                       "Tambaqui",
                                                       "Tilápia",
                                                       "Traíra e trairão",
                                                       "Tucunaré",
                                                       "Pirarucu"
                                                       )) %>% 
  select(Município,Variável,Valor,`Tipo de produto da aquicultura`) %>% 
  as_tibble()

ipq2 = ipq %>% filter(Variável == "Produção da aquicultura") %>% 
  group_by(`Tipo de produto da aquicultura`) %>% 
  mutate(QtdRel = Valor/sum(Valor,na.rm = T)) %>%
  ungroup() %>%
  left_join(
    ipq %>% filter(Variável == "Valor da produção") %>% 
      group_by(`Tipo de produto da aquicultura`) %>% 
      summarise(soma = sum(Valor,na.rm = T)) %>% 
      ungroup() %>% 
      mutate(pesos = soma/sum(soma)) %>% 
      select(1,3)
  ) %>% 
  select(-3) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(val6 = QtdRel*pesos) %>% 
  aggregate(val6 ~ Município,.,sum)

#### indicador ipae ####


ipae1 = ipp2 %>% rename(pp = !!names(.)[2]) %>% 
  left_join(ict2 %>% rename(ct = !!names(.)[2])) %>% 
  left_join(icp2 %>% rename(cp = !!names(.)[2])) %>% 
  left_join(iev2 %>% rename(ev = !!names(.)[2])) %>%
  left_join(isv2 %>% rename(sv = !!names(.)[2])) %>%
  left_join(ipq2 %>% rename(pq = !!names(.)[2]))

#### pesos por valor de produção #####

valor = c(ipq %>% filter(Variável == "Valor da produção") %>% select(Valor) %>% sum(na.rm = T),
ict %>% filter(Variável == "Valor da produção") %>% select(Valor) %>% sum(na.rm = T),
icp %>% filter(Variável == "Valor da produção") %>% select(Valor) %>% sum(na.rm = T),
iev %>% filter(Variável == "Valor da produção na extração vegetal") %>% select(Valor) %>% sum(na.rm = T),
isv %>% select("Valor da produção na silvicultura") %>% sum(na.rm = T),
vbp %>% select(valor) %>% sum() 
  )

pesos = data.frame(indicador = c("pq","ct","cp","ev","sv","pp"),valor = valor) %>% 
  mutate(peso = valor/sum(valor))

#### calculo ipae e normalizacao #####

ipaen = ipae1 %>% pivot_longer(cols = 2:ncol(.),
                       names_to = "indicador",
                       values_to = "coef") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  left_join(pesos %>% select(1,3)) %>% 
  mutate(ipae = coef*peso) %>% 
  aggregate(ipae ~ Município,.,sum) %>% 
  mutate(ipaen = (ipae - min(ipae))/(max(ipae) - min(ipae)))

### exportacao ####

require(writexl)
tabs = list(indicadores = ipae1 %>% left_join(ipaen),
            pesos = pesos)
write_xlsx(tabs, paste0("saida/ipae",anos,".xlsx"))


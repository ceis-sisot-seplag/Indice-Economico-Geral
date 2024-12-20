#### indice economico geral ####

 
indices = vafcal %>% 
  mutate(mun = municipio %>%
           norm_muni()) %>%
  select(mun,vafn) %>% 
  left_join(
    ipaen %>% mutate(mun = str_sub(end = -6,
                                   Município) %>% 
                       norm_muni()) %>% 
      select(mun,ipaen)
  ) %>% 
  left_join(
    icnt %>% 
      mutate(mun = str_sub(start = 11,municipio) %>%
               norm_muni()) %>% 
      select(mun,ISDEn)
  ) %>% 
  left_join(
    icni %>% 
      mutate(mun = str_sub(start = 11,municipio) %>%
               norm_muni()) %>% 
      select(mun,ISDIn)
  )

pca = psych::principal(indices %>% select(-1),
                       rotate = "varimax",nfactors = 4, scores = F)
mav = as.data.frame(unclass(pca$Vaccounted)) #matrixautovalores
ppvar = t(mav[2,]) #proporcao da variancia


ld <-as.data.frame(unclass(pca$loadings)) #loadings
mn = t(t(ld)/rowSums(t(ld))) #matrixnormal

theta = mn%*%ppvar

pesosieg = data.frame(ind = theta %>% rownames(),
                      pesos = c(theta[1],theta[2],theta[3],theta[4]))

ieg = indices %>% 
  pivot_longer(2:5,names_to = "ind",values_to = "valor") %>% 
  left_join(pesosieg) %>% 
  mutate(ieg = valor*pesos) %>% 
  aggregate(ieg ~ mun,.,sum) %>% 
  mutate(iegn = (ieg - min(ieg))/(max(ieg) - min(ieg)))
  
#teste = read_excel("teste.xlsx") %>% mutate(mun = municipio %>% norm_muni()) %>% 
  #arrange(mun)

#cor(ieg$iegn,teste$IEGn)
  #"#A7C7F7"

#teste = data.frame(mun = ieg$mun,script = ieg$iegn,calculo = teste$IEGn)


require(writexl)
tabs = list(indicadores = indices %>% 
              select(1,4,5,2,3) %>% 
              left_join(ieg),
            pesos = pesosieg)
write_xlsx(tabs, paste0("saida/iegIndices",anos,".xlsx"))


### unindo #### 

library(readxl)
library(writexl)
library(dplyr)

# Lista os arquivos a serem lidos
arquivos <- list.files(path = ".", pattern = "ieg20\\d{2}\\.xlsx", full.names = TRUE)

# Lê e combina os arquivos
dados_combinados <- arquivos[2:5]%>%
  lapply(function(arquivo) {
    # Extrair o ano do nome do arquivo
    ano <- gsub(".*ieg(\\d{4})\\.xlsx", "\\1", arquivo)
    
    # Ler os dados e adicionar a coluna de ano
    read_excel(arquivo) %>%
      mutate(Ano = as.numeric(ano))
  }) %>%
  bind_rows()
# Verifica os dados combinados
print(dados_combinados)

# Salva o resultado em um único arquivo Excel
write_xlsx(dados_combinados, "iegcombinados.xlsx")

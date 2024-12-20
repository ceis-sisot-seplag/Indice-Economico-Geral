

library(readxl)
vaf <- read_excel("INDICADORES ECONÔMICOS - LUCAS/VAF/ACYPR556 - Valores Utilizados para o Cálculo do Índice.docx.xlsx",
                  range = "a2:i143")
vaf


vaf2 <- read_excel("vafbase.xlsx",sheet = "base")

vafcal = vaf2 %>% filter(anoposterior == anos) %>% 
  mutate(vafpc = vaf/populacao,
         vafn = (vafpc - min(vafpc))/(max(vafpc) - min(vafpc)))

vaf2 %>% filter(municipio == "AGUA BOA")
openxlsx2::write_xlsx("vaf",paste0("saida/vaf",anos,".xlsx"))


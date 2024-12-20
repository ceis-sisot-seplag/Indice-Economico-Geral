norm_muni = function(nomes){
  nomes2 = data.frame(municipios2 = nomes)
  nomes2 = nomes2 %>% 
    mutate(municipios2 = stringi::stri_trans_general(municipios2,"Latin-ASCII") %>% 
             stringr::str_to_lower(),
           municipios2 = str_replace(municipios2,"`","'"),
           municipios2 = case_when(municipios2 == "santo antonio do leverger" ~ "santo antonio de leverger",
                                   municipios2 == "poxoreo" ~ "poxoreu",
                                   municipios2 == "santa carmen" ~ "santacarmem",
                                   municipios2 == "vila bela santissima trindade" ~ "vila bela da santissima trindade",
                                   municipios2 == "vila bela da ss trindade" ~ "vila bela da santissima trindade",
                                   municipios2 == "nossa sra do livramento" ~ "nossa senhora do livramento",
                                   municipios2 == "nova bandeirante" ~ "nova bandeirantes",
                                   municipios2 == "mirassol do oeste" ~ "mirassol d'oeste",
                                   municipios2 == "gloria do oeste" ~ "gloria d'oeste",
                                   municipios2 == "figueiropolis do oeste" ~ "figueiropolis d'oeste",
                                   municipios2 == "conquista do oeste" ~ "conquista d'oeste",
                                   TRUE ~ municipios2),
           municipios2 = str_replace(string = municipios2,pattern = "doeste",replacement ="d'oeste"),
           municipios2 = str_replace(string = municipios2,pattern = "d oeste",replacement ="d'oeste")
           )
  
  
      return(nomes2 %>% pull())
}



preencher = function(dados,colunavalor){

  nomecol = dados %>% colnames()
  nomecol = setdiff(nomecol,colunavalor)
  ncol = dados %>% ncol()
  ncol = ncol - 1
  for(i in nomecol){
    dados = dados %>% 
      pivot_wider(names_from = i,
                  values_from = colunavalor) %>% 
      mutate_all(~replace(., is.na(.), 0)) %>% 
      pivot_longer(cols = ncol:ncol(.),
                   values_to = colunavalor,
                   names_to = i)
  }
  return(dados)
}

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(format(as.data.frame(x),
                     decimal.mark = ","),
              "clipboard",sep="\t",
              row.names=row.names,
              col.names=col.names,...)
}
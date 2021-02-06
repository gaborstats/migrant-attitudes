library(haven)
library(ggplot2)
library(poLCA) 
library(xlsx)
library(openxlsx)
library(tidyLPA)
library(data.table)
library(reshape2)


# 1.import LCA modells

setwd("C:/Users/Gabor/Documents/00_Vallalkozas/02_PPK/adatok/02_analysis/01_LCA/valaszatlagok")

LCA_fit_list = readRDS(file = "LCA_list_nclass7.rds")
LCA_fit_list_bontasban = readRDS(file = "LCA_list_nclass7_bontasban.rds")

setwd("C:/Users/Gabor/Documents/00_Vallalkozas/02_PPK/adatok/02_analysis/01_LCA/valaszatlagok_stat")

# 2.valaszatlagok abrazolasa

valasz_mean  = function(lista = LCA_fit_list$Sztip_bev, n = 5, tizedes = 2){
  
  clusmemb <- as.factor(lista[[n]]$predclass)
  manif = lista[[n]]$y
  
  manif_list = list()
  for (i in 1:n){
    
    manif_list[[i]] = manif[clusmemb==i,] # kulon df subsetteket kell letrehozni osztalyonkent
  }
  
  mean_list = list()
  for (j in 1:n){
    mean_list[[j]] = apply(manif_list[[j]], 2, mean) # osztalyonkenti item atlagok
  }
  
  
  df_m = do.call(cbind.data.frame, mean_list)
  
  nevek = c("c1", "c2", "c3", "c4", "c5", "c6")
  
  names(df_m) <- nevek[1:n]
  
  df_m = round(df_m, tizedes)
  
  return(df_m)
}


# calculate means

Sztip_bev_mor_mean = valasz_mean(lista = LCA_fit_list_bontasban$Sztip_bev_mor, n = 4)
Sztip_bev_soc_mean = valasz_mean(lista = LCA_fit_list_bontasban$Sztip_bev_soc, n = 4)
Sztip_bev_compet_mean = valasz_mean(lista = LCA_fit_list_bontasban$Sztip_bev_compet, n = 3)
Sztip_men_mor_mean = valasz_mean(lista = LCA_fit_list_bontasban$Sztip_men_mor, n = 4)
Sztip_men_soc_mean = valasz_mean(lista = LCA_fit_list_bontasban$Sztip_men_soc, n = 4)
Sztip_men_compet_mean = valasz_mean(lista = LCA_fit_list_bontasban$Sztip_men_compet, n = 3)
Sztip_kEgy_mor_mean = valasz_mean(lista = LCA_fit_list_bontasban$Sztip_kEgy_mor, n = 4)
Sztip_kEgy_soc_mean = valasz_mean(lista = LCA_fit_list_bontasban$Sztip_kEgy_soc, n = 4)
Sztip_kEgy_compet_mean = valasz_mean(lista = LCA_fit_list_bontasban$Sztip_kEgy_compet, n = 3)
Feny_comp_mean = valasz_mean(lista = LCA_fit_list_bontasban$Feny_comp, n = 3)
Nemz_ID_comp_mean = valasz_mean(lista = LCA_fit_list_bontasban$Nemz_ID_comp, n = 2)
Feny_hosszu_mean = valasz_mean(lista = LCA_fit_list_bontasban$Feny_hosszu, n = 4)
Nemz_ID_hosszu_mean = valasz_mean(lista = LCA_fit_list_bontasban$Nemz_ID_hosszu, n = 3)

mylist = list(Sztip_bev_mor_mean = Sztip_bev_mor_mean,
              Sztip_bev_soc_mean = Sztip_bev_soc_mean, 
              Sztip_bev_compet_mean = Sztip_bev_compet_mean,
              Sztip_men_mor_mean = Sztip_men_mor_mean, 
              Sztip_men_soc_mean = Sztip_men_soc_mean, 
              Sztip_men_compet_mean = Sztip_men_compet_mean,
              Sztip_kEgy_mor_mean = Sztip_kEgy_mor_mean, 
              Sztip_kEgy_soc_mean = Sztip_kEgy_soc_mean,
              Sztip_kEgy_compet_mean = Sztip_kEgy_compet_mean,
              Feny_comp_mean = Feny_comp_mean, 
              Nemz_ID_comp_mean = Nemz_ID_comp_mean, 
              Feny_hosszu_mean = Feny_hosszu_mean,
              Nemz_ID_hosszu_mean = Nemz_ID_hosszu_mean)

# mehet xls-be

library(openxlsx)
of="2021-02-06_Valaszatlagok_bontott-valtozok_01.xlsx"
hs1 <- createStyle(
  fgFill = "#DCE6F1", halign = "CENTER", textDecoration = "bold",
  border = "Bottom"
)
OUT <- createWorkbook()
for(aaa in 1:length(mylist)){
  
  addWorksheet(OUT, names(mylist)[aaa])
  writeData(wb = OUT, sheet = names(mylist)[aaa], x = mylist[[aaa]], 
            rowNames = TRUE, colNames = TRUE, headerStyle = hs1)
}
saveWorkbook(OUT,of)
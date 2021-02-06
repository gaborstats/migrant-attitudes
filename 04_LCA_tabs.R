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

Feny_mean = valasz_mean(lista = LCA_fit_list$Feny, n = 4)
Hozzajar_mean = valasz_mean(lista = LCA_fit_list$Hozzajar, n = 4)
Multic_mean = valasz_mean(lista = LCA_fit_list$Multic, n = 3)
Nemz_ID_egesz_mean = valasz_mean(lista = LCA_fit_list$Nemz_ID_egesz, n = 2)
Nemz_ID_essen_mean = valasz_mean(lista = LCA_fit_list$Nemz_ID_essen, n = 4)
Dang_safe_mean = valasz_mean(lista = LCA_fit_list$Dang_safe, n = 3)
Dang_coop_mean = valasz_mean(lista = LCA_fit_list$Dang_coop, n = 3)
Sztip_bev_mean = valasz_mean(lista = LCA_fit_list$Sztip_bev, n = 5)
Sztip_men_mean = valasz_mean(lista = LCA_fit_list$Sztip_men, n = 5)
Sztip_kEgy_mean = valasz_mean(lista = LCA_fit_list$Sztip_kEgy, n = 5)
Szomszed_mean = valasz_mean(lista = LCA_fit_list$Szomszed, n = 4)
Diverzitas_mean = valasz_mean(lista = LCA_fit_list$Diverzitas, n = 3)
Akkult_mean = valasz_mean(lista = LCA_fit_list$Akkult, n = 2)

mylist = list(Feny_mean = Feny_mean, 
              Hozzajar_mean = Hozzajar_mean,
              Multic_mean = Multic_mean,
              Nemz_ID_egesz_mean = Nemz_ID_egesz_mean,
              Nemz_ID_essen_mean = Nemz_ID_essen_mean,
              Dang_safe_mean = Dang_safe_mean,
              Dang_coop_mean = Dang_coop_mean,
              Sztip_bev_mean = Sztip_bev_mean,
              Sztip_men_mean = Sztip_men_mean, 
              Sztip_kEgy_mean = Sztip_kEgy_mean, 
              Szomszed_mean = Szomszed_mean,
              Diverzitas_mean = Diverzitas_mean,
              Akkult_mean = Akkult_mean)

# mehet xls-be

library(openxlsx)
of="2021-02-06_Valaszatlagok_01.xlsx"
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
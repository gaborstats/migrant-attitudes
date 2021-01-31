# import packages 
library(haven)
library(ggplot2)
library(poLCA) 
library(xlsx)
library(openxlsx)
library(tidyLPA)

# NB: PCA with poLCA treats all variables 
# as qualitative/categorical/nominal – NOT as ordinal.

# import clean databases

setwd("C:/Users/Gabor/Documents/00_Vallalkozas/02_PPK/adatok/01_imputalas/round_2")

df_k = haven::read_sav("2021-01-16_kontrol_imp.sav")
df_p = haven::read_sav("2021-01-16_pozitiv_imp.sav")
df_n = haven::read_sav("2021-01-16_negativ_imp.sav")
df = haven::read_sav("2021_01_16_dat_with_imputation.sav") # 3 in 1
df_k_head = head(df_k)

# rekodoljuk a Nem tudjat (6) kozeperteknek (3).
df_k$szomszed_1[df_k$szomszed_1==6]=3
df_k$szomszed_2[df_k$szomszed_2==6]=3
df_k$szomszed_3[df_k$szomszed_3==6]=3
df_k$szomszed_4[df_k$szomszed_4==6]=3
df_k$szomszed_5[df_k$szomszed_5==6]=3
df_k$szomszed_6[df_k$szomszed_6==6]=3
df_k$szomszed_7[df_k$szomszed_7==6]=3
df_k$szomszed_8[df_k$szomszed_8==6]=3
df_k$szomszed_9[df_k$szomszed_9==6]=3
df_k$szomszed_10[df_k$szomszed_10==6]=3
df_k$szomszed_11[df_k$szomszed_11==6]=3
df_k$szomszed_12[df_k$szomszed_12==6]=3
df_k$szomszed_13[df_k$szomszed_13==6]=3
df_k$szomszed_14[df_k$szomszed_14==6]=3
df_k$szomszed_15[df_k$szomszed_15==6]=3
df_k$szomszed_16[df_k$szomszed_16==6]=3
df_k$szomszed_17[df_k$szomszed_17==6]=3
df_k$szomszed_18[df_k$szomszed_18==6]=3
df_k$szomszed_19[df_k$szomszed_19==6]=3


# create composite variables

# means
df_k$nemz_id_comp_egesz = round((df_k$nemz_id_1 + df_k$nemz_id_2 + df_k$nemz_id_3) / 3,0)
df_k$nemz_id_comp_essen = round((df_k$nemz_id_4 + df_k$nemz_id_5 + df_k$nemz_id_6 + df_k$nemz_id_7 + df_k$nemz_id_8) / 5,0)
df_k$feny_comp = round((df_k$feny_1 + df_k$feny_2 + df_k$feny_5 + df_k$feny_7 + df_k$feny_10 + df_k$feny_12 + df_k$feny_14 + df_k$feny_16 + df_k$feny_20 + df_k$feny_24 + df_k$feny_29) / 11,0)
df_k$hoz_comp = round((df_k$feny_3 + df_k$feny_4 + df_k$feny_8 + df_k$feny_9 + df_k$feny_11 + df_k$feny_13 + df_k$feny_15 + df_k$feny_17 + df_k$feny_21) / 9,0)

# sums
# df_k$nemz_id_comp_egesz = (df_k$nemz_id_1 + df_k$nemz_id_2 + df_k$nemz_id_3) 
# df_k$nemz_id_comp_essen = (df_k$nemz_id_4 + df_k$nemz_id_5 + df_k$nemz_id_6 + df_k$nemz_id_7 + df_k$nemz_id_8) 
# df_k$feny_comp = (df_k$feny_1 + df_k$feny_2 + df_k$feny_5 + df_k$feny_7 + df_k$feny_10 + df_k$feny_12 + df_k$feny_14 + df_k$feny_16 + df_k$feny_20 + df_k$feny_24 + df_k$feny_29) 
# df_k$hoz_comp = (df_k$feny_3 + df_k$feny_4 + df_k$feny_8 + df_k$feny_9 + df_k$feny_11 + df_k$feny_13 + df_k$feny_15 + df_k$feny_17 + df_k$feny_21) 


# names(df_k)


setwd("C:/Users/Gabor/Documents/00_Vallalkozas/02_PPK/adatok/02_analysis/01_LCA/round_02/2021-01-30")



###################
# write functions #
###################

# 1. LCA modell fv

fit_LCA_model = function(formula = formula, data = df_k, n = 7){
  
  lcalist = list()
  
  for (i in 1:n){
    
    set.seed(1)
    fitlca = poLCA::poLCA(formula, data = data, nclass = i,
                          nrep = 3, maxiter = 5000, verbose = FALSE)
    
    name <- paste('fitlca',i,sep='')
    lcalist[[name]] <- fitlca
  }
  
  return(lcalist)
}


# 2. GOF SABIC fv
# keplet forrasa: Chen et al, 2017


gof_sabic = 
  function(lista = LCA_nemz_id){
    
    lista_sabic = 0
    
    for (i in 1:length(lista)){
      L = lista[[i]]$llik
      N = lista[[i]]$Nobs
      p = lista[[i]]$npar
      
      lista_sabic[i] = -2 * L + p * log((N+2)/24)
    }
    
    names(lista_sabic) = 1:length(lista)
    lista_sabic = round(lista_sabic,0)
    
    return(lista_sabic)
  }


# 3. GOF entropy

gof_entropy = 
  function(lista = LCA_nemz_id){
    entropy = 0
    
    for (i in 1:length(lista)){
      
      p_c1 = ifelse(is.na(lista[[i]]$P[1]), 0, lista[[i]]$P[1])
      p_c2 = ifelse(is.na(lista[[i]]$P[2]), 0, lista[[i]]$P[2])
      
      l1 = p_c1*log(p_c1)
      l2 = p_c2*log(p_c2) # vigyazni kell, milyen logaritmust valasztunk, legyen termeszetes log.
      entropy[i] = -(l1+l2)
      
    }
    
    names(entropy) = 1:length(lista)
    entropy = round(entropy,4)
    
    return(entropy)
    
  }


# 4. gof bic

gof_bic = 
  function(lista = LCA_nemz_id){
    
    lista_bic = 0
    
    for (i in 1:length(lista)){
      lista_bic[i] = lista[[i]]$bic
    }
    
    names(lista_bic) = 1:length(lista)
    lista_bic = round(lista_bic,0)
    
    return(lista_bic)
  }


# 5. posterior probabilities

gof_posterior = 
  function(lista = LCA_nemz_id){
    
    lista_posterior = list()
    
    for (i in 1:length(lista)){
      L = lista[[i]]$P
      
      lista_posterior[[i]] = round(L,4)
    }
    
    names(lista_posterior) = 1:length(lista)
    
    return(lista_posterior)
  }



# 5.5. GOF LMR LRT

gof_LMR_LRT = function(lista = LCA_nemz_id){
  LMR = c()
  LMR[1] = NaN # Not-a-number (like eg. 0/0)
  
  LMR[2] = tidyLPA::calc_lrt(n =  lista$fitlca1$N,
                    null_ll = lista$fitlca1$llik,
                    null_param = lista$fitlca1$npar, 
                    null_classes = length(lista$fitlca1$P), 
                    alt_ll = lista$fitlca2$llik, 
                    alt_param = lista$fitlca2$npar, 
                    alt_classes = length(lista$fitlca2$P))[2]
  
  LMR[3] = tidyLPA::calc_lrt(n =  lista$fitlca2$N,
                    null_ll = lista$fitlca2$llik,
                    null_param = lista$fitlca2$npar, 
                    null_classes = length(lista$fitlca2$P), 
                    alt_ll = lista$fitlca3$llik, 
                    alt_param = lista$fitlca3$npar, 
                    alt_classes = length(lista$fitlca3$P))[2]
  
  LMR[4] = tidyLPA::calc_lrt(n =  lista$fitlca3$N,
                    null_ll = lista$fitlca3$llik,
                    null_param = lista$fitlca3$npar, 
                    null_classes = length(lista$fitlca3$P), 
                    alt_ll = lista$fitlca4$llik, 
                    alt_param = lista$fitlca4$npar, 
                    alt_classes = length(lista$fitlca4$P))[2]
  
  LMR[5] = tidyLPA::calc_lrt(n =  lista$fitlca4$N,
                    null_ll = lista$fitlca4$llik,
                    null_param = lista$fitlca4$npar, 
                    null_classes = length(lista$fitlca4$P), 
                    alt_ll = lista$fitlca5$llik, 
                    alt_param = lista$fitlca5$npar, 
                    alt_classes = length(lista$fitlca5$P))[2]
  
  LMR[6] = tidyLPA::calc_lrt(n =  lista$fitlca5$N,
                             null_ll = lista$fitlca5$llik,
                             null_param = lista$fitlca5$npar, 
                             null_classes = length(lista$fitlca5$P), 
                             alt_ll = lista$fitlca6$llik, 
                             alt_param = lista$fitlca6$npar, 
                             alt_classes = length(lista$fitlca6$P))[2]
  
  LMR[7] = tidyLPA::calc_lrt(n =  lista$fitlca6$N,
                             null_ll = lista$fitlca6$llik,
                             null_param = lista$fitlca6$npar, 
                             null_classes = length(lista$fitlca6$P), 
                             alt_ll = lista$fitlca7$llik, 
                             alt_param = lista$fitlca7$npar, 
                             alt_classes = length(lista$fitlca7$P))[2]
  
  
  LMR = round(LMR,0)
  return(LMR)
}





# 6. kossuk ossze az illeszkedes statisztikakat egy listaba
bind_fit = function(zoznam = LCA_nemz_id){
  
  LCA_bic = gof_bic(lista = zoznam)
  LCA_sabic = gof_sabic(lista = zoznam)
  LCA_entropy = gof_entropy(lista = zoznam)
  LCA_LMR_LRT = gof_LMR_LRT(lista = zoznam)
  
  n = length(zoznam)
  iter = 1:n
  
  fit_df = list(nclasses = iter, BIC = LCA_bic, SABIC = LCA_sabic, LMR_LRT = LCA_LMR_LRT, entropy = LCA_entropy)
  #fit_df[,1:3] = round(fit_df[,1:3],0)
  #fit_df[,4] = round(fit_df[,4],4)
  
  return(fit_df)
}





##################
# run LCA models #
##################

# 1. define PCA formula

# names(df_k)

# !
i = which(names(df_k)== "feny_1")
j = which(names(df_k)== "feny_28")

i = which(names(df_k)== "nemz_id_1")
j = which(names(df_k)== "nemz_id_8")

i = which(names(df_k)== "danger_1")
j = which(names(df_k)== "danger_20")

#i = which(names(df_k)== "sztip_bev_1")
#j = which(names(df_k)== "sztip_bev_14")

i = which(names(df_k)== "sztip_men_1")
j = which(names(df_k)== "sztip_men_14")

i = which(names(df_k)== "sztip_kEgy_1")
j = which(names(df_k)== "sztip_kEgy_14")

i = which(names(df_k)== "szomszed_1")
j = which(names(df_k)== "szomszed_19")

i = which(names(df_k)== "div_1")
j = which(names(df_k)== "div_8")

i = which(names(df_k)== "akkult_1")
j = which(names(df_k)== "akkult_5")


manif = df_k[i:j]

s = names(manif)
s2 = paste(s, collapse=" ")
s3 = gsub(" ", ", ", s2)
s4 = noquote(s3)
s4

df_k_head = head(df_k)

# !
formula_feny_hosszu = cbind(feny_1, feny_2, feny_5, feny_6, feny_7, feny_10, feny_12, feny_14, feny_16, feny_18, feny_20, feny_24, feny_29,feny_3, feny_4, feny_8, feny_9, feny_11, feny_13, feny_15, feny_17, feny_21  
) ~ 1   

formula_nemz_id_hosszu = cbind(nemz_id_1, nemz_id_2, nemz_id_3,nemz_id_4, nemz_id_5, nemz_id_6, nemz_id_7, nemz_id_8
) ~ 1


formula_nemz_id_comp = cbind(nemz_id_comp_egesz, nemz_id_comp_essen
) ~ 1 

formula_feny_comp = cbind(feny_comp, hoz_comp
) ~ 1 


formula_sztip_bev_mor = cbind(sztip_bev_2, sztip_bev_6, sztip_bev_8, sztip_bev_10, sztip_bev_13
) ~ 1

formula_sztip_bev_soc = cbind(sztip_bev_1, sztip_bev_5, sztip_bev_7, sztip_bev_9, sztip_bev_12
) ~ 1

formula_sztip_bev_compet = cbind(sztip_bev_3, sztip_bev_4, sztip_bev_11, sztip_bev_14
) ~ 1

formula_sztip_men_mor = cbind(sztip_men_2, sztip_men_6, sztip_men_8, sztip_men_10, sztip_men_13
) ~ 1

formula_sztip_men_soc = cbind(sztip_men_1, sztip_men_5, sztip_men_7, sztip_men_9, sztip_men_12
) ~ 1

formula_sztip_men_compet = cbind(sztip_men_3, sztip_men_4, sztip_men_11, sztip_men_14
) ~ 1

formula_sztip_kEgy_mor = cbind(sztip_kEgy_2, sztip_kEgy_6, sztip_kEgy_8, sztip_kEgy_10, sztip_kEgy_13
) ~ 1

formula_sztip_kEgy_soc = cbind(sztip_kEgy_1, sztip_kEgy_5, sztip_kEgy_7, sztip_kEgy_9, sztip_kEgy_12
) ~ 1

formula_sztip_kEgy_compet = cbind(sztip_kEgy_3, sztip_kEgy_4, sztip_kEgy_11, sztip_kEgy_14
) ~ 1

# 2. estimate LCA 

# a compozit valtozok nem integerekre lettek kerekitve, h lefusson az LCA
# Alternativa lehetne az LPA, de ehhez R-ben parameteres valtozok kellenek, 
# es ez megint nem teljesult.
# Q: Nem lenne jobb az atlag helyett az osszeget venni? 
# Most kozepre huz a skala a kerekites miatt. (1-es csak akkor lesz, ha 1 es 1,4 kozott van, mig 2-es akkor ha 1,5 es 2,4 kozott. Ugyanigy torzit 5-nel.)
# RE: Ha osszeget veszunk, akkor van vagy 50 nominalis kategoriank es siralmas a GOF: 
# nincs lejtese, hanem stagnal vagy mar a 2.osztalytol no.
# Tipp: Nem lenne jobb a folytonos composite-okon mast futtatni, pl. faktorelemzest? 
LCA_feny_comp = fit_LCA_model(formula = formula_feny_comp)
LCA_nemz_id_comp = fit_LCA_model(formula = formula_nemz_id_comp)

# Az LCA-hoz R-ben parameteres elo.kene.
# hist(nemz_id_comp_egesz) # fura
# hist(nemz_id_comp_essen)
# hist(feny_comp)
# hist(hoz_comp)

LCA_feny_hosszu = fit_LCA_model(formula = formula_feny_hosszu)
LCA_nemz_id_hosszu = fit_LCA_model(formula = formula_nemz_id_hosszu)

LCA_sztip_bev_mor = fit_LCA_model(formula = formula_sztip_bev_mor)
LCA_sztip_bev_soc = fit_LCA_model(formula = formula_sztip_bev_soc)
LCA_sztip_bev_compet = fit_LCA_model(formula = formula_sztip_bev_compet)
LCA_sztip_men_mor = fit_LCA_model(formula = formula_sztip_men_mor)
LCA_sztip_men_soc = fit_LCA_model(formula = formula_sztip_men_soc)
LCA_sztip_men_compet = fit_LCA_model(formula = formula_sztip_men_compet)
LCA_sztip_kEgy_mor = fit_LCA_model(formula = formula_sztip_kEgy_mor)
LCA_sztip_kEgy_soc = fit_LCA_model(formula = formula_sztip_kEgy_soc)
LCA_sztip_kEgy_compet = fit_LCA_model(formula = formula_sztip_kEgy_compet)


# 3. model fit


LCA_sztip_bev_mor_fit = bind_fit(zoznam = LCA_sztip_bev_mor)
LCA_sztip_bev_soc_fit = bind_fit(zoznam = LCA_sztip_bev_soc)
LCA_sztip_bev_compet_fit = bind_fit(zoznam = LCA_sztip_bev_compet)
LCA_sztip_men_mor_fit = bind_fit(zoznam = LCA_sztip_men_mor)
LCA_sztip_men_soc_fit = bind_fit(zoznam = LCA_sztip_men_soc)
LCA_sztip_men_compet_fit = bind_fit(zoznam = LCA_sztip_men_compet)
LCA_sztip_kEgy_mor_fit = bind_fit(zoznam = LCA_sztip_kEgy_mor)
LCA_sztip_kEgy_soc_fit = bind_fit(zoznam = LCA_sztip_kEgy_soc)
LCA_sztip_kEgy_compet_fit = bind_fit(zoznam = LCA_sztip_kEgy_compet)
LCA_feny_comp_fit = bind_fit(zoznam = LCA_feny_comp)
LCA_nemz_id_comp_fit = bind_fit(zoznam = LCA_nemz_id_comp)
LCA_feny_hosszu_fit = bind_fit(zoznam = LCA_feny_hosszu)
LCA_nemz_id_hosszu_fit = bind_fit(zoznam = LCA_nemz_id_hosszu)

LCA_fit_list = list(LCA_sztip_bev_mor_fit, LCA_sztip_bev_soc_fit, LCA_sztip_bev_compet_fit,
                    LCA_sztip_men_mor_fit, LCA_sztip_men_soc_fit, LCA_sztip_men_compet_fit,
                    LCA_sztip_kEgy_mor_fit, LCA_sztip_kEgy_soc_fit, LCA_sztip_kEgy_compet_fit,
                    LCA_feny_comp_fit,LCA_nemz_id_comp_fit,
                    LCA_feny_hosszu_fit,LCA_nemz_id_hosszu_fit)

# save LCA models

saveRDS(LCA_fit_list, file = "LCA_fit_list_nclass7_bontas_2.rds")
# readRDS(file = "LCA_fit_list_nclass7.rds")


# 4. posterior probabilities

LCA_sztip_bev_mor_posterior = gof_posterior(lista = LCA_sztip_bev_mor)
LCA_sztip_bev_soc_posterior = gof_posterior(lista = LCA_sztip_bev_soc)
LCA_sztip_bev_compet_posterior = gof_posterior(lista = LCA_sztip_bev_compet)
LCA_sztip_men_mor_posterior = gof_posterior(lista = LCA_sztip_men_mor)
LCA_sztip_men_soc_posterior = gof_posterior(lista = LCA_sztip_men_soc)
LCA_sztip_men_compet_posterior = gof_posterior(lista = LCA_sztip_men_compet)
LCA_sztip_kEgy_mor_posterior = gof_posterior(lista = LCA_sztip_kEgy_mor)
LCA_sztip_kEgy_soc_posterior = gof_posterior(lista = LCA_sztip_kEgy_soc)
LCA_sztip_kEgy_compet_posterior = gof_posterior(lista = LCA_sztip_kEgy_compet)
LCA_feny_comp_posterior = gof_posterior(lista = LCA_feny_comp)
LCA_nemz_id_comp_posterior = gof_posterior(lista = LCA_nemz_id_comp)
LCA_feny_hosszu_posterior = gof_posterior(lista = LCA_feny_hosszu)
LCA_nemz_id_hosszu_posterior = gof_posterior(lista = LCA_nemz_id_hosszu)

posterior_list = list(LCA_sztip_bev_mor_posterior, LCA_sztip_bev_soc_posterior, LCA_sztip_bev_compet_posterior,
                      LCA_sztip_men_mor_posterior, LCA_sztip_men_soc_posterior, LCA_sztip_men_compet_posterior,
                      LCA_sztip_kEgy_mor_posterior, LCA_sztip_kEgy_soc_posterior, LCA_sztip_kEgy_compet_posterior,
                      LCA_feny_comp_posterior,LCA_nemz_id_comp_posterior,
                      LCA_feny_hosszu_posterior, LCA_nemz_id_hosszu_posterior)




# write outfile

nevek = c("Sztip_bev_mor", "Sztip_bev_soc", "Sztip_bev_compet",
          "Sztip_men_mor", "Sztip_men_soc", "Sztip_men_compet",
          "Sztip_kEgy_mor", "Sztip_kEgy_soc", "Sztip_kEgy_compet",
          "Feny_comp", "Nemz_ID_comp",
          "Feny_hosszu", "Nemz_ID_hosszu")


library(openxlsx)
of="2021-01-30_LCA_model_fit_03_bontas.xlsx"
OUT <- createWorkbook()
for(aaa in 1:length(LCA_fit_list)){
  
  addWorksheet(OUT, nevek[aaa])
  writeData(OUT, sheet = nevek[aaa], x = t(names(LCA_fit_list[[1]])), colNames = F)
  writeData(OUT, sheet = nevek[aaa], x = LCA_fit_list[aaa], startRow = 2)
  
  writeData(OUT, sheet = nevek[aaa], x = "posterior", startCol = 7, startRow = 1, colNames = F)
  
  for(k in 1:length(LCA_fit_list[[1]]$nclasses)){
    writeData(OUT, sheet = nevek[aaa], x = t(posterior_list[[aaa]][[k]]), startCol = 7, startRow = 1+k, colNames = F)
  }
}
saveWorkbook(OUT,of)



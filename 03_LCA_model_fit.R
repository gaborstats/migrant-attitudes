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


# names(df_k)

setwd("C:/Users/Gabor/Documents/00_Vallalkozas/02_PPK/adatok/02_analysis/01_LCA/round_02/2021-01-23")



###################
# write functions #
###################

# 1. LCA modell fv

fit_LCA_model = function(formula = formula, data = df_k, n = 5){
  
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
formula_feny_f = cbind(feny_1, feny_2, feny_5, feny_6, feny_7, feny_10, feny_12, feny_14, feny_16, feny_18, feny_20, feny_24, feny_29  
) ~ 1 

formula_feny_hoz = cbind(feny_3, feny_4, feny_8, feny_9, feny_11, feny_13, feny_15, feny_17, feny_21  
) ~ 1 

formula_feny_multi = cbind(feny_22, feny_25, feny_26, feny_27, feny_28 
) ~ 1 

formula_nemz_id_egesz = cbind(nemz_id_1, nemz_id_2, nemz_id_3
) ~ 1

formula_nemz_id_essen = cbind(nemz_id_4, nemz_id_5, nemz_id_6, nemz_id_7, nemz_id_8
) ~ 1

formula_dang_coop = cbind(danger_1, danger_2, danger_3, danger_4, danger_5, danger_6, danger_7, danger_8, danger_9, danger_10 
) ~ 1

formula_dang_safe = cbind(danger_11, danger_12, danger_13, danger_14, danger_15, danger_16, danger_17, danger_18, danger_19, danger_20
) ~ 1

formula_sztip_bev = cbind(sztip_bev_1, sztip_bev_2, sztip_bev_3, sztip_bev_4, sztip_bev_5, sztip_bev_6, sztip_bev_7, sztip_bev_8, sztip_bev_9, sztip_bev_10, sztip_bev_11, sztip_bev_12, sztip_bev_13, sztip_bev_14
) ~ 1

formula_sztip_men = cbind(sztip_men_1, sztip_men_2, sztip_men_3, sztip_men_4, sztip_men_5, sztip_men_6, sztip_men_7, sztip_men_8, sztip_men_9, sztip_men_10, sztip_men_11, sztip_men_12, sztip_men_13, sztip_men_14
) ~ 1

formula_sztip_kEgy = cbind(sztip_kEgy_1, sztip_kEgy_2, sztip_kEgy_3, sztip_kEgy_4, sztip_kEgy_5, sztip_kEgy_6, sztip_kEgy_7, sztip_kEgy_8, sztip_kEgy_9, sztip_kEgy_10, sztip_kEgy_11, sztip_kEgy_12, sztip_kEgy_13, sztip_kEgy_14
) ~ 1

formula_szomszed = cbind(szomszed_2, szomszed_10, szomszed_12, szomszed_15, szomszed_16, szomszed_17, szomszed_18, szomszed_19  
) ~ 1 

formula_div = cbind(div_1, div_2, div_3, div_5, div_7, div_8 #, div_4, div_6  
) ~ 1  # it az itemek kozott fuggetlenseget feltetelezunk (poLCA in JSS, p.3.; Everitt 1984: An Introduction to Lantent Variable Models, p.76.)

formula_akkult = cbind(akkult_1, akkult_2, akkult_3, akkult_4, akkult_5  
) ~ 1 

# 2. estimate LCA 

LCA_feny_f = fit_LCA_model(formula = formula_feny_f)
LCA_feny_hoz = fit_LCA_model(formula = formula_feny_hoz)
LCA_feny_multi = fit_LCA_model(formula = formula_feny_multi)
LCA_nemz_id_egesz = fit_LCA_model(formula = formula_nemz_id_egesz)
LCA_nemz_id_essen = fit_LCA_model(formula = formula_nemz_id_essen)
LCA_dang_safe = fit_LCA_model(formula = formula_dang_safe)
LCA_dang_coop = fit_LCA_model(formula = formula_dang_coop)
LCA_sztip_bev = fit_LCA_model(formula = formula_sztip_bev)
LCA_sztip_men = fit_LCA_model(formula = formula_sztip_men)
LCA_sztip_kEgy = fit_LCA_model(formula = formula_sztip_kEgy)
LCA_szomszed = fit_LCA_model(formula = formula_szomszed)
LCA_div = fit_LCA_model(formula = formula_div)
LCA_akkult = fit_LCA_model(formula = formula_akkult)

# 3. model fit

# utana nezni mikor "jo" az entropia ertek
# vszeg a nagyobb ertek a jobb, de h mi az entropia maximuma,
# nem vilagos ebbol a bejegyzesbol: https://en.wikipedia.org/wiki/Entropy_(information_theory) 


LCA_feny_f_fit = bind_fit(zoznam = LCA_feny_f)
LCA_feny_hoz_fit = bind_fit(zoznam = LCA_feny_hoz)
LCA_feny_multi_fit = bind_fit(zoznam = LCA_feny_multi)
LCA_nemz_id_egesz_fit = bind_fit(zoznam = LCA_nemz_id_egesz)
LCA_nemz_id_essen_fit = bind_fit(zoznam = LCA_nemz_id_essen)
LCA_dang_safe_fit = bind_fit(zoznam = LCA_dang_safe)
LCA_dang_coop_fit = bind_fit(zoznam = LCA_dang_coop)
LCA_sztip_bev_fit = bind_fit(zoznam = LCA_sztip_bev)
LCA_sztip_men_fit = bind_fit(zoznam = LCA_sztip_men)
LCA_sztip_kEgy_fit = bind_fit(zoznam = LCA_sztip_kEgy)
LCA_szomszed_fit = bind_fit(zoznam = LCA_szomszed)
LCA_div_fit = bind_fit(zoznam = LCA_div)
LCA_akkult_fit = bind_fit(zoznam = LCA_akkult)

LCA_fit_list = list(LCA_feny_f_fit, LCA_feny_hoz_fit, LCA_feny_multi_fit, LCA_nemz_id_egesz_fit, LCA_nemz_id_essen_fit, LCA_dang_safe_fit, LCA_dang_coop_fit, LCA_sztip_bev_fit, LCA_sztip_men_fit,LCA_sztip_kEgy_fit, LCA_szomszed_fit, LCA_div_fit,LCA_akkult_fit)


# 4. posterior probabilities

LCA_feny_f_posterior = gof_posterior(lista = LCA_feny_f)
LCA_feny_hoz_posterior = gof_posterior(lista = LCA_feny_hoz)
LCA_feny_multi_posterior = gof_posterior(lista = LCA_feny_multi)
LCA_nemz_id_egesz_posterior = gof_posterior(lista = LCA_nemz_id_egesz)
LCA_nemz_id_essen_posterior = gof_posterior(lista = LCA_nemz_id_essen)
LCA_dang_safe_posterior = gof_posterior(lista = LCA_dang_safe)
LCA_dang_coop_posterior = gof_posterior(lista = LCA_dang_coop)
LCA_sztip_bev_posterior = gof_posterior(lista = LCA_sztip_bev)
LCA_sztip_men_posterior = gof_posterior(lista = LCA_sztip_men)
LCA_sztip_kEgy_posterior = gof_posterior(lista = LCA_sztip_kEgy)
LCA_szomszed_posterior = gof_posterior(lista = LCA_szomszed)
LCA_div_posterior = gof_posterior(lista = LCA_div)
LCA_akkult_posterior = gof_posterior(lista = LCA_akkult)

posterior_list = list(LCA_feny_f_posterior,LCA_feny_hoz_posterior,LCA_feny_multi_posterior, LCA_nemz_id_egesz_posterior,LCA_nemz_id_essen_posterior, LCA_dang_safe_posterior, LCA_dang_coop_posterior, LCA_sztip_bev_posterior, LCA_sztip_men_posterior,LCA_sztip_kEgy_posterior, LCA_szomszed_posterior, LCA_div_posterior,LCA_akkult_posterior)




# write outfile

nevek = c("Feny", "Hozzajar", "Multic", "Nemz_ID_egesz", "Nemz_ID_essen", "Dang_safe", "Dang_coop", "Sztip_bev", "Sztip_men", "Sztip_kEgy", "Szomszed", "Diverzitas", "Akkult")


library(openxlsx)
of="2021-01-23_LCA_model_fit_02.xlsx"
OUT <- createWorkbook()
for(aaa in 1:length(LCA_fit_list)){
  
  addWorksheet(OUT, nevek[aaa])
  writeData(OUT, sheet = nevek[aaa], x = t(names(LCA_fit_list[[1]])), colNames = F)
  writeData(OUT, sheet = nevek[aaa], x = LCA_fit_list[aaa], startRow = 2)
  
  writeData(OUT, sheet = nevek[aaa], x = "posterior", startCol = 7, startRow = 1, colNames = F)
  
  for(k in 1:length(LCA_fit_list[[1]])){
    writeData(OUT, sheet = nevek[aaa], x = t(posterior_list[[aaa]][[k]]), startCol = 7, startRow = 1+k, colNames = F)
  }
}
saveWorkbook(OUT,of)

# 1.)
# tip: elso blikkre az a GOF tunik jonak, ahol entropia a legmagasabb, 
# vagy (SA)BIC platora fut. Nezzuk meg ketto kozott melyik relevansabb 
# indikator a mi esetunkben, es javasoljuk azt.

# Q: entropy-nak lehet 2 clusternal is csucsa? 
# RE: vszeg igen, ha utana folyamatosan csokken.
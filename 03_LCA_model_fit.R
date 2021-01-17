# import packages 
library(haven)
library(ggplot2)
library(poLCA) 
library(xlsx)
library(openxlsx)

# NB: PCA with poLCA treats all variables 
# as qualitative/categorical/nominal – NOT as ordinal.

# import clean databases

setwd("C:/Users/Gabor/Documents/00_Vallalkozas/02_PPK/adatok/01_imputalas/round_2")

df_k = haven::read_sav("2021-01-16_kontrol_imp.sav")
df_p = haven::read_sav("2021-01-16_pozitiv_imp.sav")
df_n = haven::read_sav("2021-01-16_negativ_imp.sav")
df = haven::read_sav("2021_01_16_dat_with_imputation.sav") # 3 in 1

# names(df_k)

setwd("C:/Users/Gabor/Documents/00_Vallalkozas/02_PPK/adatok/02_analysis/01_LCA/round_02")



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
  
  LMR[2] = calc_lrt(n =  lista$fitlca1$N,
                    null_ll = lista$fitlca1$llik,
                    null_param = lista$fitlca1$npar, 
                    null_classes = length(lista$fitlca1$P), 
                    alt_ll = lista$fitlca2$llik, 
                    alt_param = lista$fitlca2$npar, 
                    alt_classes = length(lista$fitlca2$P))[2]
  
  LMR[3] = calc_lrt(n =  lista$fitlca2$N,
                    null_ll = lista$fitlca2$llik,
                    null_param = lista$fitlca2$npar, 
                    null_classes = length(lista$fitlca2$P), 
                    alt_ll = lista$fitlca3$llik, 
                    alt_param = lista$fitlca3$npar, 
                    alt_classes = length(lista$fitlca3$P))[2]
  
  LMR[4] = calc_lrt(n =  lista$fitlca3$N,
                    null_ll = lista$fitlca3$llik,
                    null_param = lista$fitlca3$npar, 
                    null_classes = length(lista$fitlca3$P), 
                    alt_ll = lista$fitlca4$llik, 
                    alt_param = lista$fitlca4$npar, 
                    alt_classes = length(lista$fitlca4$P))[2]
  
  LMR[5] = calc_lrt(n =  lista$fitlca4$N,
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
  
  n = length(LCA_nemz_id)
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
i = which(names(df_k)== "nemz_id_1")
j = which(names(df_k)== "nemz_id_8")

#i = which(names(df_k)== "sztip_bev_1")
#j = which(names(df_k)== "sztip_bev_15")

i = which(names(df_k)== "sztip_men_1")
j = which(names(df_k)== "sztip_men_15")

manif = df_k[i:j]

s = names(manif)
s2 = paste(s, collapse=" ")
s3 = gsub(" ", ", ", s2)
s4 = noquote(s3)
# s4

# !
formula_nemz_id = cbind(nemz_id_1, nemz_id_2, nemz_id_3, nemz_id_4, nemz_id_5, nemz_id_6, nemz_id_7, nemz_id_8
) ~ 1

formula_sztip_bev = cbind(sztip_bev_1, sztip_bev_2, sztip_bev_3, sztip_bev_4, sztip_bev_5, sztip_bev_6, sztip_bev_7, sztip_bev_8, sztip_bev_9, sztip_bev_10, sztip_bev_11, sztip_bev_12, sztip_bev_13, sztip_bev_14, sztip_bev_15
) ~ 1

formula_sztip_men = cbind(sztip_men_1, sztip_men_2, sztip_men_3, sztip_men_4, sztip_men_5, sztip_men_6, sztip_men_7, sztip_men_8, sztip_men_9, sztip_men_10, sztip_men_11, sztip_men_12, sztip_men_13, sztip_men_14, sztip_men_15
) ~ 1


# 2. estimate LCA 

LCA_nemz_id = fit_LCA_model(formula = formula_nemz_id)
LCA_sztip_bev = fit_LCA_model(formula = formula_sztip_bev)
LCA_sztip_men = fit_LCA_model(formula = formula_sztip_men)


# 3. model fit

# utana nezni mikor "jo" az entropia ertek
# vszeg a nagyobb ertek a jobb, de h mi az entropia maximuma,
# nem vilagos ebbol a bejegyzesbol: https://en.wikipedia.org/wiki/Entropy_(information_theory) 


LCA_nemz_id_fit = bind_fit(zoznam = LCA_nemz_id)
LCA_sztip_bev_fit = bind_fit(zoznam = LCA_sztip_bev)
LCA_sztip_men_fit = bind_fit(zoznam = LCA_sztip_men)

LCA_fit_list = list(LCA_nemz_id_fit, LCA_sztip_bev_fit, LCA_sztip_men_fit)


# 4. posterior probabilities

LCA_nemz_id_posterior = gof_posterior(lista = LCA_nemz_id)
LCA_sztip_bev_posterior = gof_posterior(lista = LCA_sztip_bev)
LCA_sztip_men_posterior = gof_posterior(lista = LCA_sztip_men)

posterior_list = list(LCA_nemz_id_posterior, LCA_sztip_bev_posterior, LCA_sztip_men_posterior)




# write outfile

nevek = c("Nemzeti_ID", "Sztip_bev", "Sztip_men")


library(openxlsx)
of="LCA_model_fit5.xlsx"
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

# 2.)
# futtassuk le a tobbi valtozora is, ahogy L-kerte
# ld.valtozocsokkentes fajl, aztan kesz vagyunk.
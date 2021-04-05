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

# invert negatively scaled var: nemz_id_8

df_k$nemz_id_8 = ifelse(df_k$nemz_id_8==1,5,
                         ifelse(df_k$nemz_id_8==2,4,
                                ifelse(df_k$nemz_id_8==4,2,
                                       ifelse(df_k$nemz_id_8==5,1,3)
                                       )
                                )
                         )


# create composite variables by averaging
df_k$nemz_id_comp_egesz = (df_k$nemz_id_1 + df_k$nemz_id_2 + df_k$nemz_id_3) / 3
df_k$nemz_id_comp_essen = (df_k$nemz_id_4 + df_k$nemz_id_5 + df_k$nemz_id_6 + df_k$nemz_id_7 + df_k$nemz_id_8) / 5
df_k$feny_comp = (df_k$feny_1 + df_k$feny_2 + df_k$feny_5 + df_k$feny_7 + df_k$feny_10 + df_k$feny_12 + df_k$feny_14 + df_k$feny_16 + df_k$feny_20 + df_k$feny_24 + df_k$feny_29) / 11
df_k$hoz_comp = (df_k$feny_3 + df_k$feny_4 + df_k$feny_8 + df_k$feny_9 + df_k$feny_11 + df_k$feny_13 + df_k$feny_15 + df_k$feny_17 + df_k$feny_21) / 9


setwd("C:/Users/Gabor/Documents/00_Vallalkozas/02_PPK/adatok/02_analysis/04_LPA")

head(df_k[,(ncol(df_k)-3):ncol(df_k)])


# select variables 

feny_hoz = df_k[,(ncol(df_k)-1):ncol(df_k)]
nemz_id = df_k[,(ncol(df_k)-3):(ncol(df_k)-2)]

# nezzunk eloszlasokat
hist(feny_hoz$feny_comp) # left skewd
hist(feny_hoz$hoz_comp) # OK-ish
hist(nemz_id$nemz_id_comp_egesz) # right skewd
hist(nemz_id$nemz_id_comp_essen) # OK-ish



# run LPA models

library("mclust")

# extract BIC from LPA
LPA_modell_BIC = function(manifest_vars = feny_hoz, i = 10){
                  require("mclust")
                  set.seed(123)
                  clustbic <- mclustBIC(manifest_vars, G = 1:i)
                  clusfit <- Mclust(manifest_vars, x = clustbic)
                  best_bic = summary(clusfit, parameters = TRUE)$bi
                  return(best_bic)
}


# extract maxized log-likelihood from LPA
LPA_modell_ML = function(manifest_vars = feny_hoz, i = 10){
  require("mclust")
  set.seed(123)
  clustbic <- mclustBIC(manifest_vars, G = 1:i)
  clusfit <- Mclust(manifest_vars, x = clustbic)
  best_bic = summary(clusfit, parameters = TRUE)$log
  return(best_bic)
}


# loop over clusters

# 1. Feny es Hozz
feny_hoz_best_BIC = list()
feny_hoz_best_ML = list() # maximized log-likelihood
for (j in 1:10){
  feny_hoz_best_BIC[j] = LPA_modell_BIC(manifest_vars = feny_hoz, i = j)
  feny_hoz_best_ML[j] = LPA_modell_ML(manifest_vars = feny_hoz, i = j)
  print(j)

}

feny_hoz_best_BIC 
feny_hoz_best_ML


# 2. Nemz_ID
Nemz_ID_best_BIC = list()
Nemz_ID_best_ML = list()
for (j in 1:10){
  Nemz_ID_best_BIC[j] = LPA_modell_BIC(manifest_vars = nemz_id, i = j)
  Nemz_ID_best_ML[j] = LPA_modell_ML(manifest_vars = nemz_id, i = j)
  print(j)
  
}

Nemz_ID_best_BIC 
Nemz_ID_best_ML



# run with LCA model with appropriate cluster size:

# 1. nemz ID
# specifiy number of clusters
set.seed(123)
clustbic2 <- mclustBIC(nemz_id, G = 1:2)
clustbic2

clusfit2 <- Mclust(nemz_id, x = clustbic2)
summary(clusfit2, parameters = TRUE)

# extract probabilities
round(clusfit2$z, 3)

# extract classes
ni_pred = clusfit2$classification


nemz_id_sum = summary(clusfit2, parameters = TRUE)$mean
nemz_id_sum
# tudunk kulonbseget tenni 3. v 4. vagy 3. v. 5. osztaly kozott? 

clustred2 <- MclustDR(clusfit2)
# plot(clustred2, what = "boundaries", ngrid = 200)
# plot(clustred2, what = "density", dimens = 1)


# 2. Feny es hozzajarulas
set.seed(123)
clustbic <- mclustBIC(feny_hoz, G = 1:3)
clustbic

clusfit <- Mclust(feny_hoz, x = clustbic)
summary(clusfit, parameters = TRUE)

# 7 cluster solution is suggested by BIC search.

# extract probabilities
round(clusfit$z, 3)

# extract classes
fh_pred = clusfit$classification

feny_hoz_sum = summary(clusfit, parameters = TRUE)$mean
feny_hoz_sum
# cluster one is dominated by persons who scored high on feny and low on hoz.
# NB: cluster 2 and cluster 7 are not different at all. Something's wrong with the model


clustred <- MclustDR(clusfit)
#plot(clustred, what = "boundaries", ngrid = 200)
#plot(clustred, what = "density", dimens = 1)


# export results

library(openxlsx)
wb <- createWorkbook("TEST")

addWorksheet(wb, "Nemz_ID_comp_BIC")
writeData(wb, sheet = 1, Nemz_ID_best_BIC)

addWorksheet(wb, "Feny_hozz_comp_BIC")
writeData(wb, sheet = 2, feny_hoz_best_BIC)

addWorksheet(wb, "Nemz_ID_comp_válaszátlagok")
writeData(wb, sheet = 3, nemz_id_sum, rowNames = TRUE)

addWorksheet(wb, "Feny_hozz_válaszátlagok")
writeData(wb, sheet = 4, feny_hoz_sum, rowNames = TRUE)

addWorksheet(wb, "Nemz_ID_comp_ML")
writeData(wb, sheet = 5, Nemz_ID_best_ML)

addWorksheet(wb, "Feny_hozz_comp_ML")
writeData(wb, sheet = 6, feny_hoz_best_ML)

saveWorkbook(wb, file = "2021-04-04_LPA-BIC_01.xlsx", overwrite = TRUE)


# abrazoljuk a valaszatlagokat
# 1. Nemz ID
names(nemz_id_sum)
class(nemz_id_sum)
nemz_id_sum_df = as.data.frame(nemz_id_sum)

names(nemz_id_sum_df) = c("c1", "c2")
nemz_id_sum_df$Kérdés <- rownames(nemz_id_sum_df)
rownames(nemz_id_sum_df)=NULL
df_long = reshape2::melt(nemz_id_sum_df, id.vars = "Kérdés")
names(df_long)[2:3] = c("Osztály", "Válasz")

ggplot2::ggplot(data = df_long, 
                             aes(x = Kérdés, y = Válasz,
                                 group = Osztály)) +
  geom_line(aes(color = Osztály), size=1) +
  geom_point(aes(color = Osztály, shape = Osztály), size=3) +
  theme(legend.position="bottom", axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  coord_cartesian( ylim = c(1, 5))



# 1. Feny Hozz
feny_hoz_sum_df = as.data.frame(feny_hoz_sum)

names(feny_hoz_sum_df) = c("c1", "c2", "c3")
feny_hoz_sum_df$Kérdés <- rownames(feny_hoz_sum_df)
rownames(feny_hoz_sum_df)=NULL
df_long2 = reshape2::melt(feny_hoz_sum_df, id.vars = "Kérdés")
names(df_long2)[2:3] = c("Osztály", "Válasz")

ggplot2::ggplot(data = df_long2, 
                aes(x = Kérdés, y = Válasz,
                    group = Osztály)) +
  geom_line(aes(color = Osztály), size=1) +
  geom_point(aes(color = Osztály, shape = Osztály), size=3) +
  theme(legend.position="bottom", axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  coord_cartesian( ylim = c(1, 5))



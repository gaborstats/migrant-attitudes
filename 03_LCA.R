setwd("C:/Users/Gabor/Documents/00_Vallalkozas/02_PPK/adatok/02_analysis")
library(haven)

# import clean databases

df_k = read_sav("kontrol_imp.sav")
df_p = read_sav("pozitiv_imp.sav")
df_n = read_sav("negativ_imp.sav")
df = read_sav("dat_imputation_final_cimkekkel.sav") # 3 in 1

setwd("C:/Users/Gabor/Documents/00_Vallalkozas/02_PPK/adatok/02_analysis/01_LCA")




################################
# LCA -- nemzeti identifikáció #
################################

library("poLCA") 
# NB: PCA with poLCA treats all variables 
# as qualitative/categorical/nominal – NOT as ordinal.

i = which(names(df_k)== "nemz_id_1")
j = which(names(df_k)== "nemz_id_8")

df_k_nemz = df_k[i:j]

formula = cbind(nemz_id_1, nemz_id_2, nemz_id_3, nemz_id_4, nemz_id_5, nemz_id_6, nemz_id_7, nemz_id_8) ~ 1

set.seed(1)
fitlca1 <- poLCA::poLCA(formula, data = df_k_nemz, nclass = 1,
                        nrep = 3, maxiter = 5000, verbose = FALSE)
fitlca2 <- poLCA::poLCA(formula, data = df_k_nemz, nclass = 2,
                 nrep = 3, maxiter = 5000, verbose = FALSE)
fitlca3 <- poLCA::poLCA(formula, data = df_k_nemz, nclass = 3,
                        nrep = 3, maxiter = 5000, verbose = FALSE)
fitlca4 <- poLCA::poLCA(formula, data = df_k_nemz, nclass = 4,
                        nrep = 3, maxiter = 5000, verbose = FALSE)
fitlca5 <- poLCA::poLCA(formula, data = df_k_nemz, nclass = 5,
                        nrep = 3, maxiter = 5000, verbose = FALSE)

# fitlca1$bic;fitlca2$bic;fitlca3$bic;fitlca4$bic;fitlca5$bic;
# fitlca2$npar
# fitlca2$P

# abrazoljuk a modell illeszkedest

b_temp = cbind(fitlca1$bic, fitlca2$bic, fitlca3$bic, 
          fitlca4$bic, fitlca5$bic)
b_t = as.data.frame(t(b_temp))
names(b_t) = "BIC"
b <- cbind(C = rownames(b_t), b_t, row.names=NULL)
# b

library(ggplot2)

png("Nemz_id_BIC.png", width = 500, height = 300, res=80)
ggplot(data = b, 
  aes(x = C, y = BIC, group = 1)) +
  geom_point(size = 2) +
  geom_line() +
  
  coord_cartesian( ylim = c(0, max(b_temp))) +
  xlab ("Látens osztályok száma") +
  
  ggtitle( "Nemzeti identifikáció") +
  theme(plot.title = element_text(hjust = 0.5)) 
  # ajánlott multilpot fő cím: "LCA modell illeszkedése az adatokra"
dev.off()


# eloszlasvizsgalat

library(purrr)
library(tidyr)
library(ggplot2)

df_k_nemz %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar() + 
  xlab("Válasz") + ylab("Létszám") 

# 1,3,8 nagyon nem normalis eloszlasu!
# ezt csak akkor hasznalhatjuk, ha ide factor scorokat dugunk be.



# abrazoljuk a manifest valtozok atlagat clusterenkent

# plot(fitlca2)

clusmemb <- as.factor(fitlca2$predclass) # predikcio
# fitlca2$posterior # itt vannak a valoszinusegek

df_k_nemz_c1 = df_k_nemz[clusmemb==1,]
df_k_nemz_c2 = df_k_nemz[clusmemb==2,]

df_k_nemz_m1 = apply(df_k_nemz_c1, 2, mean)
df_k_nemz_m2 = apply(df_k_nemz_c2, 2, mean)


# base R plot

plot(df_k_nemz_m1, type = "b", ylim = c(1,5), col = "#0C7BDC", 
     pch = 15, cex.main = 1,
     main = "Nemzeti identifikáció \nkérdésekre adott válaszok átlaga látens osztályonként ", ylab = "válasz", xlab = "kérdés")
lines(df_k_nemz_m2, type = "b", col = "#FFC20A", pch = 19)
legend(1,2, legend=c("egészséges nemzeti id.", "esszencialista"),
       col=c("#0C7BDC", "#FFC20A"), pch = c(15,19), cex = 0.8,
       box.col = "white")


# ggplot 

# Basic line plot with points

df_k_nemz_m1_temp = data.frame(as.list(df_k_nemz_m1))
df_k_nemz_m1_temp2 = as.data.frame(t(df_k_nemz_m1_temp))
# df_k_nemz_m1_temp2$Q = rownames(df_k_nemz_m1_temp2)
df_k_nemz_m1_temp3 <- cbind(Kérdés = rownames(df_k_nemz_m1_temp2), data.frame(df_k_nemz_m1_temp2, row.names=NULL), Osztály = rep("egészséges nemzeti id.", 8))
# df_k_nemz_m1_temp3

df_k_nemz_m2_temp = data.frame(as.list(df_k_nemz_m2))
df_k_nemz_m2_temp2 = as.data.frame(t(df_k_nemz_m2_temp))
# df_k_nemz_m1_temp2$Q = rownames(df_k_nemz_m1_temp2)
df_k_nemz_m2_temp3 <- cbind(Kérdés = rownames(df_k_nemz_m2_temp2), data.frame(df_k_nemz_m2_temp2, row.names=NULL), Osztály = rep("esszencialista", 8))
# df_k_nemz_m2_temp3

df_k_nemz_df = rbind(df_k_nemz_m1_temp3, df_k_nemz_m2_temp3)
names(df_k_nemz_df)[names(df_k_nemz_df)=="V1"] = "Válasz"
# class(df_k_nemz_df)




# cimkek gyartasa aranyszamokkal

arany = round(fitlca2$P,2)
cimke = c("egészséges nemzeti id.", "esszencialista") # ezt automtizaljuk
L = t(cbind(c1 = arany, cimke))
# L

c1 = paste0(L[2,1], " (", L[1,1], ")")
c2 = paste0(L[2,2], " (", L[1,2], ")")
# c1;c2

png("Nemz_id_atlag.png", width = 500, height = 300, res=80)
ggplot(data = df_k_nemz_df, 
       aes(x = Kérdés, y = Válasz,
           group = Osztály)) +
  
  geom_line(aes(color = Osztály), size=1) +
  geom_point(aes(color = Osztály, shape = Osztály), size=3) +
  scale_color_manual(values=c("#0C7BDC", "#FFC20A"), labels = c(c1, c2)) +
  scale_shape_manual(values=c(15, 19), labels = c(c1, c2)) +
  
  theme(legend.position="bottom") +
  coord_cartesian( ylim = c(1, 5))
  
  #ggtitle( "Nemzeti identifikáció \nkérdésekre adott válaszok átlaga látens osztályonként") +
  #theme(plot.title = element_text(hjust = 0.5)) 
  #+ xlab("Kérdés") + ylab("Válasz") 
  # ezek ele jo lenne egy line break. NB: \n nem mukodik.
dev.off()



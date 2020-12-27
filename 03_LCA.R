# import clean databases

setwd("C:/Users/Gabor/Documents/00_Vallalkozas/02_PPK/adatok/02_analysis")

df_k = read_sav("kontrol_imp.sav")
df_p = read_sav("pozitiv_imp.sav")
df_n = read_sav("negativ_imp.sav")
df = read_sav("dat_imputation_final_cimkekkel.sav") # 3 in 1

setwd("C:/Users/Gabor/Documents/00_Vallalkozas/02_PPK/adatok/02_analysis/01_LCA")


# import packages 

library(haven)
library(ggplot2)
library(poLCA) 

# NB: PCA with poLCA treats all variables 
# as qualitative/categorical/nominal – NOT as ordinal.




##########################
# fuggvenyek létrehozása #
##########################


# 1. BIC ábra 

bic_abra = function(lcalist = lcalist, n = 5,
                    fajlnev = "Sztip_men_BIC.png",
                    abracim = "Sztereotípia menekültekkel szemben"){
  b_temp = 0
  for (j in 1:n){
    b_temp[j] = cbind(round(lcalist[[j]]$bic,0)) # extract BIC values
    names(b_temp)[j] = names(lcalist)[j]
  }
  
  iter = rep(1:n)
  b_temp2 = rbind(BIC = b_temp, C = iter) 
  b_t = as.data.frame(t(b_temp2)) # store BIC in a df
  b = cbind(b_t, model = rownames(b_t))
  rownames(b) <- NULL
  
  # create plot
  png(fajlnev, width = 500, height = 300, res=80) # !
  abra_BIC = ggplot2::ggplot(data = b, 
                         aes(x = C, y = BIC, group = 1, label = BIC)) +
    geom_point(size = 2) +
    geom_line() +
    geom_text(size = 3.5, position = position_stack(vjust = 0.8)) + # value labels on chart
    
    coord_cartesian( ylim = c(0, max(b_temp))) +
    xlab ("Látens osztályok száma") +
    
    ggtitle(abracim) +  # !
    theme(plot.title = element_text(hjust = 0.5)) 
  # ajánlott multilpot fő cím: "LCA modell illeszkedése az adatokra"
  
  print(abra_BIC)
  dev.off()
  
  return(abra_BIC)
}


# 2. válaszátlagok ábra

valasz_abra = function(lcalist = lcalist, k = 2,
                       fajlnev = "Sztip_men.png"){
  # !
  clusmemb <- as.factor(lcalist[[k]]$predclass) # itt csinaljunk egy elagazast k = 3-ra
  
  manif_c1 = manif[clusmemb==1,]
  manif_c2 = manif[clusmemb==2,]
  
  manif_m1 = apply(manif_c1, 2, mean) # set up df for ggplot
  manif_m2 = apply(manif_c2, 2, mean)
  
  manif_m1_temp = data.frame(as.list(manif_m1))
  manif_m1_temp2 = as.data.frame(t(manif_m1_temp))
  manif_m1_temp3 <- cbind(Kérdés = rownames(manif_m1_temp2), data.frame(manif_m1_temp2, row.names=NULL), Osztály = rep("c1", nrow(manif_m1_temp2)))
  
  manif_m2_temp = data.frame(as.list(manif_m2))
  manif_m2_temp2 = as.data.frame(t(manif_m2_temp))
  manif_m2_temp3 <- cbind(Kérdés = rownames(manif_m2_temp2), data.frame(manif_m2_temp2, row.names=NULL), Osztály = rep("c2", nrow(manif_m2_temp2)))
  
  manif_avg = rbind(manif_m1_temp3, manif_m2_temp3)
  names(manif_avg)[names(manif_avg)=="V1"] = "Válasz"
  
  # !
  arany = round(lcalist[[k]]$P,2) # cimkeerteke gyartasa jelmagyarazathoz
  cimke = c("c1", "c2") # !
  L = t(cbind(c1 = arany, cimke))
  
  c1 = paste0(L[2,1], " (", L[1,1], ")")
  c2 = paste0(L[2,2], " (", L[1,2], ")")
  
  # ensure categories are not reordered on x axis
  manif_avg$Kérdés <- factor(manif_avg$Kérdés, levels = unique(manif_avg$Kérdés)) 
  
  # !
  png(fajlnev, width = 500, height = 300, res=80) # !
  abra_atlag = ggplot2::ggplot(data = manif_avg, 
                               aes(x = Kérdés, y = Válasz,
                                   group = Osztály)) +
    
    geom_line(aes(color = Osztály), size=1) +
    geom_point(aes(color = Osztály, shape = Osztály), size=3) +
    scale_color_manual(values=c("#0C7BDC", "#FFC20A"), labels = c(c1, c2)) +
    scale_shape_manual(values=c(15, 19), labels = c(c1, c2)) +
    
    theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian( ylim = c(1, 5))
  
  print(abra_atlag)
  dev.off()
  return(abra_atlag)
}





####################################
# LCA -- sztereotípia: menekültek #
####################################

# 1. define PCA formula

# names(df_k)

# !
i = which(names(df_k)== "sztip_men_1")
j = which(names(df_k)== "sztip_men_15")

manif = df_k[i:j]

s = names(manif)
s2 = paste(s, collapse=" ")
s3 = gsub(" ", ", ", s2)
s4 = noquote(s3)
# s4

# !
formula = cbind(sztip_men_1, sztip_men_2, sztip_men_3, sztip_men_4, sztip_men_5, sztip_men_6, sztip_men_7, sztip_men_8, sztip_men_9, sztip_men_10, sztip_men_11, sztip_men_12, sztip_men_13, sztip_men_14, sztip_men_15
                ) ~ 1


# 2. fit LCA model

# !
n = 5 # define the number of maximum clusters the model should try
lcalist = list()

for (i in 1:n){
  
  set.seed(1)
  fitlca = poLCA::poLCA(formula, data = manif, nclass = i,
               nrep = 3, maxiter = 5000, verbose = FALSE)
  
  name <- paste('fitlca',i,sep='')
  lcalist[[name]] <- fitlca
}

# names(lcalist)
# lcalist[[3]]




# 3. abrazoljuk a modellek illeszkedeset

bic_abra(lcalist = lcalist, n = n, 
         fajlnev = "BIC_sztip_men.png",
         abracim = "Sztereotípia menekültekkel szemben")




# 4. abrazoljuk a manifest valtozok atlagat clusterenkent

valasz_abra(lcalist = lcalist, k = 2, fajlnev = "Atlag_sztip_men.png")





####################################
# LCA -- sztereotípia: bevándorlók #
####################################

# 1. define PCA formula

# names(df_k)

# !
i = which(names(df_k)== "sztip_bev_1")
j = which(names(df_k)== "sztip_bev_15")

manif = df_k[i:j]

s = names(manif)
s2 = paste(s, collapse=" ")
s3 = gsub(" ", ", ", s2)
s4 = noquote(s3)
# s4

# !
formula = cbind(sztip_bev_1, sztip_bev_2, sztip_bev_3, sztip_bev_4, sztip_bev_5, sztip_bev_6, sztip_bev_7, sztip_bev_8, sztip_bev_9, sztip_bev_10, sztip_bev_11, sztip_bev_12, sztip_bev_13, sztip_bev_14, sztip_bev_15
) ~ 1


# 2. fit LCA model

# !
n = 5 # define the number of maximum clusters the model should try
lcalist = list()

for (i in 1:n){
  
  set.seed(1)
  fitlca = poLCA::poLCA(formula, data = manif, nclass = i,
                        nrep = 3, maxiter = 5000, verbose = FALSE)
  
  name <- paste('fitlca',i,sep='')
  lcalist[[name]] <- fitlca
}


# 3. abrazoljuk a modellek illeszkedeset

bic_abra(lcalist = lcalist, n = n, 
         fajlnev = "BIC_sztip_bev.png",
         abracim = "Sztereotípia bevándorlókkal szemben")


# 4. abrazoljuk a manifest valtozok atlagat clusterenkent

valasz_abra(lcalist = lcalist, k = 2, fajlnev = "Atlag_sztip_bev.png")



##############################################
# LCA -- sztereotípia: külföldi egyetemisták #
##############################################

# 1. define PCA formula

# names(df_k)

# !
i = which(names(df_k)== "sztip_kEgy_1")
j = which(names(df_k)== "sztip_kEgy_15")

manif = df_k[i:j]

s = names(manif)
s2 = paste(s, collapse=" ")
s3 = gsub(" ", ", ", s2)
s4 = noquote(s3)
# s4

# !
formula = cbind(sztip_kEgy_1, sztip_kEgy_2, sztip_kEgy_3, sztip_kEgy_4, sztip_kEgy_5, sztip_kEgy_6, sztip_kEgy_7, sztip_kEgy_8, sztip_kEgy_9, sztip_kEgy_10, sztip_kEgy_11, sztip_kEgy_12, sztip_kEgy_13, sztip_kEgy_14, sztip_kEgy_15
) ~ 1


# 2. fit LCA model

# !
n = 5 # define the number of maximum clusters the model should try
lcalist = list()

for (i in 1:n){
  
  set.seed(1)
  fitlca = poLCA::poLCA(formula, data = manif, nclass = i,
                        nrep = 3, maxiter = 5000, verbose = FALSE)
  
  name <- paste('fitlca',i,sep='')
  lcalist[[name]] <- fitlca
}


# 3. abrazoljuk a modellek illeszkedeset

bic_abra(lcalist = lcalist, n = n, 
         fajlnev = "BIC_sztip_kEgy.png",
         abracim = "Sztereotípia külf.egyetemistákkal szemben")


# 4. abrazoljuk a manifest valtozok atlagat clusterenkent

valasz_abra(lcalist = lcalist, k = 2, fajlnev = "Atlag_sztip_kEgy.png")



###############################
# LCA -- dangerous worldviews #
###############################

# 1. define PCA formula

# names(df_k)

# !
i = which(names(df_k)== "danger_1")
j = which(names(df_k)== "danger_20")

manif = df_k[i:j]

s = names(manif)
s2 = paste(s, collapse=" ")
s3 = gsub(" ", ", ", s2)
s4 = noquote(s3)
# s4

# !
formula = cbind(danger_1, danger_2, danger_3, danger_4, danger_5, danger_6, danger_7, danger_8, danger_9, danger_10, danger_11, danger_12, danger_13, danger_14, danger_15, danger_16, danger_17, danger_18, danger_19, danger_20
) ~ 1


# 2. fit LCA model

# !
n = 5 # define the number of maximum clusters the model should try
lcalist = list()

for (i in 1:n){
  
  set.seed(1)
  fitlca = poLCA::poLCA(formula, data = manif, nclass = i,
                        nrep = 3, maxiter = 5000, verbose = FALSE)
  
  name <- paste('fitlca',i,sep='')
  lcalist[[name]] <- fitlca
}


# 3. abrazoljuk a modellek illeszkedeset

bic_abra(lcalist = lcalist, n = n, 
         fajlnev = "BIC_danger.png",
         abracim = "Dangerous worldviews")


# 4. abrazoljuk a manifest valtozok atlagat clusterenkent

valasz_abra(lcalist = lcalist, k = 2, fajlnev = "Atlag_danger.png")

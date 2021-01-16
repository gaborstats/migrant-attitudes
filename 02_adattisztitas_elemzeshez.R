setwd("C:/Users/Gabor/Documents/00_Vallalkozas/02_PPK/adatok/01_imputalas/round_2")
library(haven)

# adattisztitas az elemzés előkészítéséhez
# prereq: imputed data sets from Samu

df_imp = read_sav("2021_01_16_dat_with_imputation.sav")


################################
# fontos valtozok kivalasztasa #
################################


# dobjuk ki a valtozokat, amik nem lettek atnevezve (nevukben "q" szerepel)

v2 = names(df_imp)
new_var2 = v2[!grepl("q", v2, fixed = TRUE)] 
df_imp_reduced = df_imp[,dput(new_var2)]


df_imp_p = df_imp_reduced[df_imp_reduced$minta==1,] # pozitiv
df_imp_n = df_imp_reduced[df_imp_reduced$minta==2,] 
df_imp_k = df_imp_reduced[df_imp_reduced$minta==3,] # kontrol



# dobjuk ki az completely NA valtozokat

torol1 = names(df_imp_p)[sapply(df_imp_p, function(x)all(is.na(x)))]
df_imp_p_wo_NA = df_imp_p[, !(colnames(df_imp_p) %in% torol1)]

torol2 = names(df_imp_n)[sapply(df_imp_n, function(x)all(is.na(x)))]
df_imp_n_wo_NA = df_imp_n[, !(colnames(df_imp_n) %in% torol2)]




#####################################
# adatok elokeszitese az elemzeshez #
#####################################


df = df_imp_reduced
df_k = df_imp_k
df_p = df_imp_p_wo_NA
df_n= df_imp_n_wo_NA

names(df_p)[sapply(df_p, function(x)any(is.na(x)))]
names(df_n)[sapply(df_n, function(x)any(is.na(x)))]
names(df_k)[sapply(df_k, function(x)any(is.na(x)))]
# nincs NA a bontott adatbazisokban

# remove variables that are constant and therefore have zero variance

names(df[sapply(df, var, na.rm = T) == 0])
names(df_k[sapply(df_k, var, na.rm = T) == 0])

df$szulhely = NULL
df_k$szulhely = NULL
df_n$szulhely = NULL
df_p$szulhely = NULL

df_k$minta = NULL
df_n$minta = NULL
df_p$minta = NULL

# egyeb felesleges valtozok a korrelation matrixba: 

df_k$id = NULL
df_n$id = NULL
df_p$id = NULL

df_k$irsz = NULL
df_n$irsz = NULL
df_p$irsz = NULL



# mentsuk el a felosztott adatbazisokat

setwd("C:/Users/Gabor/Documents/00_Vallalkozas/02_PPK/adatok/01_imputalas/round_2")

write_sav(df_k, "2016-01-16_kontrol_imp.sav")
write_sav(df_n, "2016-01-16_negativ_imp.sav")
write_sav(df_p, "2016-01-16_pozitiv_imp.sav")

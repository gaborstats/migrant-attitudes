library(haven)

df_m = read_sav("minden.sav")
df_t = read_sav("tisztitott.sav")
df_temp = read_sav("tiszta98.sav")
df_67 = read_sav("v67.sav")
df_50 = read_sav("v50.sav")



### 1. irrelevans valtozok kiszurese ###

names(df_temp)

df_temp_head = head(df_temp)

digi = names(df_temp)[247:487] # digi.biztonság


m = which(names(df_temp)=="q7_1")
n = which(names(df_temp)=="q12_15")
mind_missing = names(df_temp)[m:n] # completely missing variables
nyilt_kerdesek = c("q66_other", "q68_1comment", "q68_2comment", "q68_3comment", "q69_other", "q75a_other", "q76a_other", "q76b_other")

num = c(2:4, 6, 554:559)
egyeb_felesleges = names(df_temp)[num] # adatfelvetel technikai reszletei (lastpage, refurl) es szamolt valtozok (x1:valaszok)

kivesz = c(digi, nyilt_kerdesek, mind_missing, egyeb_felesleges)

df_98 = df_temp[ , -which(names(df_temp) %in% kivesz)] 
rm(df_temp)




### 2. fenyegetettseg (i_) valtozok kapcsolasa ###

names(df_m)
osz = c(1,5, 562:587)
valt = names(df_m)[osz]
df_m_small = df_m[,valt]

library("dplyr")
df_98_j = left_join(x = df_98, y = df_m_small, by = c("id","ipaddr"))



### 3. duplikalt ID-k kezelese ###

dup = df_98[which(duplicated(df_98[,c("id")])==T),]
dup = df_98[which(duplicated(df_98[,c("id","ipaddr")])==T),]
dup = df_98[which(duplicated(df_98[,c("id", "szulev", "nem", "csal", "isk")])==T),] # 2 olyan eset volt ahol az ID-n kivul a demo adatok is duplikaltak
dup2 = df_m[df_m$id == 128,] # de a tobbi valasz itt is eltero

id = 1:nrow(df_98)
df_98$id = id # felulirtam az ID-ket, mert a "duplikalt" ID-k mas valaszadokhoz tartoztak
df_98_j$id = id

df_98$ipaddr = NULL
df_98_j$ipaddr = NULL

df_98_head = head(df_98)



### 4. valasszuk szet a mintakat ###


df_m1_with_na = df_98_j[df_98_j$minta==1,]
df_m2_with_na = df_98_j[df_98_j$minta==2,]
df_m3_with_na = df_98_j[df_98_j$minta==3,] # kontrol



### 5. toroljuk azokat a valtozokat, ahol az osszes ertek NA ###


torol1 = names(df_m1_with_na)[sapply(df_m1_with_na, function(x)all(is.na(x)))]
df_m1 = df_m1_with_na[, !(colnames(df_m1_with_na) %in% torol1)]

torol2 = names(df_m2_with_na)[sapply(df_m2_with_na, function(x)all(is.na(x)))]
df_m2 = df_m2_with_na[, !(colnames(df_m2_with_na) %in% torol2)]

torol3 = names(df_m3_with_na)[sapply(df_m3_with_na, function(x)all(is.na(x)))]
df_m3 = df_m3_with_na[, !(colnames(df_m3_with_na) %in% torol3)]



### 6. hany hianyzo adatunk van oszlopnkent a 3 mintaban ###


p = as.data.frame(sapply(df_m1, function(x) sum(is.na(x))) )
names(p) = "hianyzok_szama"
#p[p$hianyzok_szama>0,]
sort(p[p$hianyzok_szama>0,], decreasing = F)

n = as.data.frame(sapply(df_m2, function(x) sum(is.na(x))) )
names(n) = "hianyzok_szama"
#n[n$hianyzok_szama>0,]
sort(n[n$hianyzok_szama>0,], decreasing = F)

k = as.data.frame(sapply(df_m3, function(x) sum(is.na(x))) )
names(k) = "hianyzok_szama"
#k[k$hianyzok_szama>0,]
sort(k[k$hianyzok_szama>0,], decreasing = F)


### 7. number of complete cases per data set ###

# proba1 = df_m1[complete.cases(df_m1),] # 0 observations
# proba2 = df_m2[complete.cases(df_m2),] # 0 observations
# proba3 = df_m3[complete.cases(df_m3),] # 0 observations

# Konkluzio: az alom hogy olyan sorokkal dolgozzunk, ahol nincs NA ertek!


### 8. mentsuk el a 3 mintat ###

setwd("C:/Users/Gabor/Documents/00_Vallalkozas/02_PPK/adatok/clean-data")

write_sav(df_m1, "pozitiv.sav")
write_sav(df_m2, "negativ.sav")
write_sav(df_m3, "kontrol.sav")

# import data 
df_p = read_sav("pozitiv.sav")
df_n = read_sav("negativ.sav")
df = read_sav("kontrol.sav")


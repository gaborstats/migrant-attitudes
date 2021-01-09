
setwd("C:/Users/Gabor/Documents/00_Vallalkozas/02_PPK/adatok/01_data-cleaning")

library(haven)
library(dplyr)

df_temp = haven::read_sav("tiszta98.sav")
df_m = haven::read_sav("2012-01-05_minden.sav") # i-suffixes valtozok miatt kell, tartalmazza i10-et is a 2-es mintara.
df_74 = haven::read_sav("sav74.sav") # ebbol csak a q1_27 valtozo kell

table(df_temp$q62_10, useNA = "ifany")
table(df_m$q62_10, useNA = "ifany")
table(df_temp$q62_10, useNA = "ifany")
table(df_m$q62_10, useNA = "ifany")

############
# join dfs #
############


### 1. irrelevans valtozok kiszurese ###


# df_temp_head = head(df_temp)
# names(df_temp_head)

d_1 = which(names(df_temp)=="q39") 
d_2 = which(names(df_temp)=="q61_7")
digi = names(df_temp)[d_1:d_2] # digi.biztonság


m = which(names(df_temp)=="q7_1") # sztereotipia: zsido, roma, ffi, no, kabszerhasznalo, hetero
n = which(names(df_temp)=="q12_15")
o1 = which(names(df_temp)=="q62_1") # onertekeles
o2 = which(names(df_temp)=="q62_10")
mind_missing = names(df_temp)[c(m:n, o1:o2)] 
# (almost) completely missing variables (egy valaszadon kivul senki nem valaszolta meg oket)

# names(df_temp)[sapply(df_temp, function(x)all(is.na(x)))]
# table(df_temp$q12_1, useNA = "always")

# nyilt_kerdesek = c("q66_other", "q68_1comment", "q68_2comment", "q68_3comment", "q69_other", "q75a_other", "q76a_other", "q76b_other")

num = c(2:4, 6, 554:559)
egyeb_felesleges = names(df_temp)[num] # adatfelvetel technikai reszletei (lastpage, refurl) es szamolt valtozok (x1:valaszok)

kivesz = c(digi, mind_missing, egyeb_felesleges)

df_98 = df_temp[ , -which(names(df_temp) %in% kivesz)] 
rm(df_temp)



### 2. fenyegetettseg (i_) valtozok kapcsolasa ###

osz = c(1,5, 562:587)
valt = names(df_m)[osz]
df_m_small = df_m[,valt]

df_98_k = dplyr::left_join(x = df_98, y = df_m_small, by = c("id","ipaddr"))


# q1_27 változó kapcsolása a df_74 df-bol

o = which(names(df_74)=="q1_27")
osz2 = c(1,7,48)
valt2 = names(df_74)[osz2]
df_74_small = df_74[,valt2]

df_98_j = left_join(x = df_98_k, y = df_74_small, by = c("id","ipaddr"))

# df_98_j$q2 # Itt van benne: 
# q2 Mennyire hasznos vagy artalmas a bevnadorlas? (L: q11)



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
# sort(p[p$hianyzok_szama>0,], decreasing = F)

n = as.data.frame(sapply(df_m2, function(x) sum(is.na(x))) )
names(n) = "hianyzok_szama"
#n[n$hianyzok_szama>0,]
# sort(n[n$hianyzok_szama>0,], decreasing = F)

k = as.data.frame(sapply(df_m3, function(x) sum(is.na(x))) )
names(k) = "hianyzok_szama"
#k[k$hianyzok_szama>0,]
# sort(k[k$hianyzok_szama>0,], decreasing = F)


### 7. number of complete cases per data set ###

# proba1 = df_m1[complete.cases(df_m1),] # 0 observations
# proba2 = df_m2[complete.cases(df_m2),] # 0 observations
# proba3 = df_m3[complete.cases(df_m3),] # 0 observations

# Konkluzio: az alom hogy olyan sorokkal dolgozzunk, ahol nincs NA ertek!


# next step: nevezzuk at a valtozokat es NE soroljuk ki a "q" nevvel 
# renedlk. valtozokat.
# vegul ellenorizzuk le, h megvan-e minden, amit L-kert (figyeljunk, h ott
# legyen az onertekeles is.)




##############
# rename dfs #
##############

# rename dfs for new session

df = NULL
df_n = NULL
df_p = NULL

df = df_m3
df_p = df_m1
df_n = df_m2

# ezt kell potolni:
# q1_27, q2
# which(names(df)=="q1_27")

# 8. rename variables in kontrol data set


df = dplyr::rename(df, 
            # nemzeti identitas
            nemz_id_1 = q32_1, nemz_id_2 = q32_2, nemz_id_3 = q32_3,
            nemz_id_4 = q32_4, nemz_id_5 = q32_5, nemz_id_6 = q32_6,
            nemz_id_7 = q32_7, nemz_id_8 = q32_8,
            
            # sztereotipiak: bevandorlok
            sztip_bev_1 = q4_1, sztip_bev_2 = q4_2, sztip_bev_3 = q4_3,
            sztip_bev_4 = q4_4, sztip_bev_5 = q4_5, sztip_bev_6 = q4_6,
            sztip_bev_7 = q4_7, sztip_bev_8 = q4_8, sztip_bev_9 = q4_9,
            sztip_bev_10 = q4_10, sztip_bev_11 = q4_11, sztip_bev_12 = q4_12,
            sztip_bev_13 = q4_13, sztip_bev_14 = q4_14, sztip_bev_15 = q4_15,
            
            # sztereotipiak: menekultek
            sztip_men_1 = q5_1, sztip_men_2 = q5_2, sztip_men_3 = q5_3,
            sztip_men_4 = q5_4, sztip_men_5 = q5_5, sztip_men_6 = q5_6,
            sztip_men_7 = q5_7, sztip_men_8 = q5_8, sztip_men_9 = q5_9,
            sztip_men_10 = q5_10, sztip_men_11 = q5_11, sztip_men_12 = q5_12,
            sztip_men_13 = q5_13, sztip_men_14 = q5_14, sztip_men_15 = q5_15,
            
            # sztereotipiak: kulfoldi egyetemistak
            sztip_kEgy_1 = q6_1, sztip_kEgy_2 = q6_2, sztip_kEgy_3 = q6_3,
            sztip_kEgy_4 = q6_4, sztip_kEgy_5 = q6_5, sztip_kEgy_6 = q6_6,
            sztip_kEgy_7 = q6_7, sztip_kEgy_8 = q6_8, sztip_kEgy_9 = q6_9,
            sztip_kEgy_10 = q6_10, sztip_kEgy_11 = q6_11, sztip_kEgy_12 = q6_12,
            sztip_kEgy_13 = q6_13, sztip_kEgy_14 = q6_14, sztip_kEgy_15 = q6_15,
            
            # szomszedos
            szomszed_1 = q13, szomszed_2 = q14, szomszed_3 = q15, 
            szomszed_4 = q16, szomszed_5 = q17, szomszed_6 = q18, 
            szomszed_7 = q19, szomszed_8 = q20, szomszed_9 = q21,
            szomszed_10 = q22, szomszed_11 = q23, szomszed_12 = q24,
            szomszed_13 = q25, szomszed_14 = q26, szomszed_15 = q27,
            szomszed_16 = q28, szomszed_17 = q29, szomszed_18 = q30,
            szomszed_19 = q31,
            
            # akkulturacio
            akkult_1 = q3_1, akkult_2 = q3_2, akkult_3 = q3_3,
            akkult_4 = q3_4, akkult_5 = q3_5,
            
            # bevándorló kontaktus (no, west, "east")
            kont_w_1 = q33_1_2, kont_e_1 = q33_1_3, 
            kont_w_2 = q33_2_2, kont_e_2 = q33_2_3, 
            kont_w_3 = q33_3_2, kont_e_3 = q33_3_3, 
            kont_w_4 = q33_4_2, kont_e_4 = q33_4_3,
            kont_w_5 = q33_5_2, kont_e_5 = q33_5_3,
            kont_w_6 = q33_6_2, kont_e_6 = q33_6_3,
            kont_w_7 = q33_7_2, kont_e_7 = q33_7_3,
            
            # etniakai diverzitas tapasztalat
            div_1 = q77_1, div_2 = q77_2, div_3 = q77_3, 
            div_4 = q77_4, div_5 = q77_5, div_6 = q77_6, 
            div_7 = q77_7, div_8 = q77_8,
            
            # dangerous worldviews
            danger_1 = q34_1, danger_2 = q34_2, danger_3 = q34_3,
            danger_4 = q34_4, danger_5 = q34_5, danger_6 = q34_6,
            danger_7 = q34_7, danger_8 = q34_8, danger_9 = q34_9,
            danger_10 = q34_10, danger_11 = q34_11, danger_12 = q34_12,
            danger_13 = q34_13, danger_14 = q34_14, danger_15 = q34_15,
            danger_16 = q34_16, danger_17 = q34_17, danger_18 = q34_18,
            danger_19 = q34_19, danger_20 = q34_20,
            
            # demo (idegennyelv, gazd.helyzet, vallasossag)
            ideg_1 = q68_1, ideg_2 = q68_2, ideg_3 = q68_3,
            gyer_ideg = q70, gazd = q71, pol = q73_1, val_1 = q75,
            val_2 = q76b,
            
            # fenyegetettseg, hozzajarulas, multikult.
            # feny_19 = i19, feny_23 = i23, # nem leteznek
            feny_1 = i1, feny_2 = i2, feny_3 = i3, feny_4 = i4,
            feny_5 = i5, feny_6 = i6, feny_7 = i7, feny_8 = i8,
            feny_9 = i9, feny_10 = i10, feny_11 = i11, feny_12 = i12,
            feny_13 = i13, feny_14 = i14, feny_15 = i15, feny_16 = i16,
            feny_17 = i17, feny_18 = i18, feny_20 = i20,
            feny_21 = i21, feny_22 = i22, feny_24 = i24,
            feny_25 = i25, feny_26 = i26, feny_27 = i27, feny_28 = i28,
            
            # kiegeszites
            feny_29 = q1_27, haszon = q2
            
            
            # covid 35 - 38
            
)


# 9. rename variables in pozitiv data set


df_p = dplyr::rename(df_p, 
              # nemzeti identitas
              nemz_id_1 = q32_1, nemz_id_2 = q32_2, nemz_id_3 = q32_3,
              nemz_id_4 = q32_4, nemz_id_5 = q32_5, nemz_id_6 = q32_6,
              nemz_id_7 = q32_7, nemz_id_8 = q32_8,
              
              # sztereotipiak: bevandorlok
              sztip_bev_1 = q4_1, sztip_bev_2 = q4_2, sztip_bev_3 = q4_3,
              sztip_bev_4 = q4_4, sztip_bev_5 = q4_5, sztip_bev_6 = q4_6,
              sztip_bev_7 = q4_7, sztip_bev_8 = q4_8, sztip_bev_9 = q4_9,
              sztip_bev_10 = q4_10, sztip_bev_11 = q4_11, sztip_bev_12 = q4_12,
              sztip_bev_13 = q4_13, sztip_bev_14 = q4_14, sztip_bev_15 = q4_15,
              
              # sztereotipiak: menekultek
              sztip_men_1 = q5_1, sztip_men_2 = q5_2, sztip_men_3 = q5_3,
              sztip_men_4 = q5_4, sztip_men_5 = q5_5, sztip_men_6 = q5_6,
              sztip_men_7 = q5_7, sztip_men_8 = q5_8, sztip_men_9 = q5_9,
              sztip_men_10 = q5_10, sztip_men_11 = q5_11, sztip_men_12 = q5_12,
              sztip_men_13 = q5_13, sztip_men_14 = q5_14, sztip_men_15 = q5_15,
              
              # sztereotipiak: kulfoldi egyetemistak
              sztip_kEgy_1 = q6_1, sztip_kEgy_2 = q6_2, sztip_kEgy_3 = q6_3,
              sztip_kEgy_4 = q6_4, sztip_kEgy_5 = q6_5, sztip_kEgy_6 = q6_6,
              sztip_kEgy_7 = q6_7, sztip_kEgy_8 = q6_8, sztip_kEgy_9 = q6_9,
              sztip_kEgy_10 = q6_10, sztip_kEgy_11 = q6_11, sztip_kEgy_12 = q6_12,
              sztip_kEgy_13 = q6_13, sztip_kEgy_14 = q6_14, sztip_kEgy_15 = q6_15,
              
              # szomszedos
              szomszed_1 = q13, szomszed_2 = q14, szomszed_3 = q15, 
              szomszed_4 = q16, szomszed_5 = q17, szomszed_6 = q18, 
              szomszed_7 = q19, szomszed_8 = q20, szomszed_9 = q21,
              szomszed_10 = q22, szomszed_11 = q23, szomszed_12 = q24,
              szomszed_13 = q25, szomszed_14 = q26, szomszed_15 = q27,
              szomszed_16 = q28, szomszed_17 = q29, szomszed_18 = q30,
              szomszed_19 = q31,
              
              # akkulturacio
              akkult_1 = q3_1, akkult_2 = q3_2, akkult_3 = q3_3,
              akkult_4 = q3_4, akkult_5 = q3_5,
              
              # bevándorló kontaktus (no, west, "east")
              kont_w_1 = q33_1_2, kont_e_1 = q33_1_3, 
              kont_w_2 = q33_2_2, kont_e_2 = q33_2_3, 
              kont_w_3 = q33_3_2, kont_e_3 = q33_3_3, 
              kont_w_4 = q33_4_2, kont_e_4 = q33_4_3,
              kont_w_5 = q33_5_2, kont_e_5 = q33_5_3,
              kont_w_6 = q33_6_2, kont_e_6 = q33_6_3,
              kont_w_7 = q33_7_2, kont_e_7 = q33_7_3,
              
              # etniakai diverzitas tapasztalat
              div_1 = q77_1, div_2 = q77_2, div_3 = q77_3, 
              div_4 = q77_4, div_5 = q77_5, div_6 = q77_6, 
              div_7 = q77_7, div_8 = q77_8,
              
              # dangerous worldviews
              danger_1 = q34_1, danger_2 = q34_2, danger_3 = q34_3,
              danger_4 = q34_4, danger_5 = q34_5, danger_6 = q34_6,
              danger_7 = q34_7, danger_8 = q34_8, danger_9 = q34_9,
              danger_10 = q34_10, danger_11 = q34_11, danger_12 = q34_12,
              danger_13 = q34_13, danger_14 = q34_14, danger_15 = q34_15,
              danger_16 = q34_16, danger_17 = q34_17, danger_18 = q34_18,
              danger_19 = q34_19, danger_20 = q34_20,
              
              # demo (idegennyelv, gazd.helyzet, vallasossag)
              ideg_1 = q68_1, ideg_2 = q68_2, ideg_3 = q68_3,
              gyer_ideg = q70, gazd = q71, pol = q73_1, val_1 = q75,
              val_2 = q76b,
              
              # fenyegetettseg, hozzajarulas, multikult.
              # feny_19 = i19, feny_23 = i23, # nem leteznek
              feny_3 = i3, feny_4 = i4, feny_9 = i9, 
              feny_11 = i11, feny_15 = i15,
              
              
              # kiegeszites
              haszon = q2
              
              # covid 35 - 38
              
)


# 10. rename variables in negativ data set

# names(df_n)

df_n = dplyr::rename(df_n, 
              # nemzeti identitas
              nemz_id_1 = q32_1, nemz_id_2 = q32_2, nemz_id_3 = q32_3,
              nemz_id_4 = q32_4, nemz_id_5 = q32_5, nemz_id_6 = q32_6,
              nemz_id_7 = q32_7, nemz_id_8 = q32_8,
              
              # sztereotipiak: bevandorlok
              sztip_bev_1 = q4_1, sztip_bev_2 = q4_2, sztip_bev_3 = q4_3,
              sztip_bev_4 = q4_4, sztip_bev_5 = q4_5, sztip_bev_6 = q4_6,
              sztip_bev_7 = q4_7, sztip_bev_8 = q4_8, sztip_bev_9 = q4_9,
              sztip_bev_10 = q4_10, sztip_bev_11 = q4_11, sztip_bev_12 = q4_12,
              sztip_bev_13 = q4_13, sztip_bev_14 = q4_14, sztip_bev_15 = q4_15,
              
              # sztereotipiak: menekultek
              sztip_men_1 = q5_1, sztip_men_2 = q5_2, sztip_men_3 = q5_3,
              sztip_men_4 = q5_4, sztip_men_5 = q5_5, sztip_men_6 = q5_6,
              sztip_men_7 = q5_7, sztip_men_8 = q5_8, sztip_men_9 = q5_9,
              sztip_men_10 = q5_10, sztip_men_11 = q5_11, sztip_men_12 = q5_12,
              sztip_men_13 = q5_13, sztip_men_14 = q5_14, sztip_men_15 = q5_15,
              
              # sztereotipiak: kulfoldi egyetemistak
              sztip_kEgy_1 = q6_1, sztip_kEgy_2 = q6_2, sztip_kEgy_3 = q6_3,
              sztip_kEgy_4 = q6_4, sztip_kEgy_5 = q6_5, sztip_kEgy_6 = q6_6,
              sztip_kEgy_7 = q6_7, sztip_kEgy_8 = q6_8, sztip_kEgy_9 = q6_9,
              sztip_kEgy_10 = q6_10, sztip_kEgy_11 = q6_11, sztip_kEgy_12 = q6_12,
              sztip_kEgy_13 = q6_13, sztip_kEgy_14 = q6_14, sztip_kEgy_15 = q6_15,
              
              # szomszedos
              szomszed_1 = q13, szomszed_2 = q14, szomszed_3 = q15, 
              szomszed_4 = q16, szomszed_5 = q17, szomszed_6 = q18, 
              szomszed_7 = q19, szomszed_8 = q20, szomszed_9 = q21,
              szomszed_10 = q22, szomszed_11 = q23, szomszed_12 = q24,
              szomszed_13 = q25, szomszed_14 = q26, szomszed_15 = q27,
              szomszed_16 = q28, szomszed_17 = q29, szomszed_18 = q30,
              szomszed_19 = q31,
              
              # akkulturacio
              akkult_1 = q3_1, akkult_2 = q3_2, akkult_3 = q3_3,
              akkult_4 = q3_4, akkult_5 = q3_5,
              
              # bevándorló kontaktus (no, west, "east")
              kont_w_1 = q33_1_2, kont_e_1 = q33_1_3, 
              kont_w_2 = q33_2_2, kont_e_2 = q33_2_3, 
              kont_w_3 = q33_3_2, kont_e_3 = q33_3_3, 
              kont_w_4 = q33_4_2, kont_e_4 = q33_4_3,
              kont_w_5 = q33_5_2, kont_e_5 = q33_5_3,
              kont_w_6 = q33_6_2, kont_e_6 = q33_6_3,
              kont_w_7 = q33_7_2, kont_e_7 = q33_7_3,
              
              # etniakai diverzitas tapasztalat
              div_1 = q77_1, div_2 = q77_2, div_3 = q77_3, 
              div_4 = q77_4, div_5 = q77_5, div_6 = q77_6, 
              div_7 = q77_7, div_8 = q77_8,
              
              # dangerous worldviews
              danger_1 = q34_1, danger_2 = q34_2, danger_3 = q34_3,
              danger_4 = q34_4, danger_5 = q34_5, danger_6 = q34_6,
              danger_7 = q34_7, danger_8 = q34_8, danger_9 = q34_9,
              danger_10 = q34_10, danger_11 = q34_11, danger_12 = q34_12,
              danger_13 = q34_13, danger_14 = q34_14, danger_15 = q34_15,
              danger_16 = q34_16, danger_17 = q34_17, danger_18 = q34_18,
              danger_19 = q34_19, danger_20 = q34_20,
              
              # demo (idegennyelv, gazd.helyzet, vallasossag)
              ideg_1 = q68_1, ideg_2 = q68_2, ideg_3 = q68_3,
              gyer_ideg = q70, gazd = q71, pol = q73_1, val_1 = q75,
              val_2 = q76b,
              
              # fenyegetettseg, hozzajarulas, multikult.
              # feny_19 = i19, feny_23 = i23, # nem leteznek
              feny_2 = i2, feny_6 = i6, feny_16 = i16, 
              feny_24 = i24, feny_10 = i10,
              
              
              # kiegeszites
              haszon = q2
              
              # covid 35 - 38
              
)




##############################################
# remove largely missing cases and variables #
##############################################


# tavolitsuk el a megfigyeleseket, akik danger kerdeseket en block kihagytak
# 1046.sorig (excluded)
# names(df)
proba = df[,105:110]
proba_p = df_p[,105:110]
proba_n = df_n[,105:110]

df_short = df[1046:nrow(df),]
df_short_p = df_p[363:nrow(df_p),]
df_short_n = df_n[245:nrow(df_n),]


# remove cases where all diversity variables are missing

# negatives
div_s = which(names(df_short_n)=="div_1")
div_e = which(names(df_short_n)=="div_8")

proba_n = df_short_n[,div_s:div_e]
count = rowSums(is.na(proba_n))
i = count == 8
df_short_n_2 = df_short_n[!i,]

# pozitives
div_s = which(names(df_short_p)=="div_1")
div_e = which(names(df_short_p)=="div_8")

proba_p = df_short_p[,div_s:div_e]
i = NULL; count = NULL
count = rowSums(is.na(proba_p))
i = count == 8
df_short_p_2 = df_short_p[!i,]

# kontroll
div_s = which(names(df_short_p)=="div_1")
div_e = which(names(df_short_p)=="div_8")

proba = df_short[,div_s:div_e]
i = NULL; count = NULL
count = rowSums(is.na(proba))
i = count == 8
df_short_2 = df_short[!i,]



# remove telip variable

kidob = "teltip"

df_short_3 = df_short_2[ , !(names(df_short_2) %in% kidob)]
df_short_n_3 = df_short_n_2[ , !(names(df_short_n_2) %in% kidob)]
df_short_p_3 = df_short_p_2[ , !(names(df_short_p_2) %in% kidob)]


# remove first x rows from kontroll, as they are systematically missing in stereotip variabes
df_short_4 = df_short_3[-c(1:16), ]
df_short_n_4 = df_short_n_3[-c(1:5), ]
df_short_p_4 = df_short_p_3[-c(1:11), ]




#################
# visualization #
#################

dim(df_short_n)

library(visdat)
vis_miss(df_short_n_4[1:60])
vis_miss(df_short_n_4[61:101]) # kontakt erosen hianyos, de L kerte, h maradjon
vis_miss(df_short_n_4[102:145])
vis_miss(df_short_n_4[146:186])
vis_miss(df_short_n_4[187:ncol(df_short_n_4)])

vis_miss(df_short_p_4[1:60])
vis_miss(df_short_p_4[61:101]) # kontakt erosen hianyos, de L kerte, h maradjon
vis_miss(df_short_p_4[102:145])
vis_miss(df_short_p_4[146:186])
vis_miss(df_short_p_4[187:ncol(df_short_p_4)])

vis_miss(df_short_4[1:60])
vis_miss(df_short_4[61:101]) # kontakt erosen hianyos, de L kerte, h maradjon
vis_miss(df_short_4[102:145])
vis_miss(df_short_4[146:186])
vis_miss(df_short_4[187:ncol(df_short_4)])



# summary of data

dim(df_short_4)
dim(df_short_p_4)
dim(df_short_n_4)

# hianyzo adatok aranya
m = round(mean(is.na(df_short_4))*100, 1)
cat("Share of missing values in control df is", m, "%")

mp = round(mean(is.na(df_short_p_4))*100, 1)
cat("Share of missing values in positive df is", mp, "%")

mn = round(mean(is.na(df_short_n_4))*100, 1)
cat("Share of missing values in negative df is", mn, "%")


setwd("C:/Users/Gabor/Documents/00_Vallalkozas/02_PPK/adatok/01_data-cleaning/clean-data")

write_sav(df_short_4, "2021_01_09_kontrol_4.sav")
write_sav(df_short_n_4, "2021_01_09_negativ_4.sav")
write_sav(df_short_p_4, "2021_01_09_pozitiv_4.sav")


# bind the three samples together 

df_full = dplyr::bind_rows(df_short_4, df_short_n_4)
df_full2 = dplyr::bind_rows(df_full, df_short_p_4)

dim(df_full2)

# hianyzo adatok aranya
mf = round(mean(is.na(df_full2))*100, 1)
cat("Share of missing values in control df is", mf, "%")


# mentsuk el
write_sav(df_full2, "2021_01_09_dat.sav")
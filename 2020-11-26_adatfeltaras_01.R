setwd("C:/Users/Gabor/Documents/00_Vallalkozas/02_PPK/adatok/clean-data")
library(haven)

# import data 

df_p = read_sav("pozitiv.sav")
df_n = read_sav("negativ.sav")
df = read_sav("kontrol.sav")


# 1. rename variables in kontrol data set

library(dplyr)
df = rename(df, 
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
            
            # bevándorló kontaktus (west, "east")
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
            feny_25 = i25, feny_26 = i26, feny_27 = i27, feny_28 = i28
            
            
            # covid 35 - 38
            
            )


# soroljuk ki a valtozokat, amiknek neveben "q" szerepel

v = names(df)
new_var = v[!grepl("q", v, fixed = TRUE)]
df2 = df[,dput(new_var)]



# 2. rename variables in pozitiv data set

names(df_p)

library(dplyr)
df_p = rename(df_p, 
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
            
            # bevándorló kontaktus (west, "east")
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
            feny_11 = i11, feny_15 = i15
            
            
            # covid 35 - 38
            
)


# soroljuk ki a valtozokat, amiknek neveben "q" szerepel

v2 = names(df_p)
new_var2 = v2[!grepl("q", v2, fixed = TRUE)]
df_p2 = df_p[,dput(new_var2)]



# 3. rename variables in negativ data set

names(df_n)

library(dplyr)
df_n = rename(df_n, 
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
              
              # bevándorló kontaktus (west, "east")
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
              feny_24 = i24
              
              
              # covid 35 - 38
              
)


# soroljuk ki a valtozokat, amiknek neveben "q" szerepel

v3 = names(df_n)
new_var3 = v3[!grepl("q", v3, fixed = TRUE)]
df_n2 = df_n[,dput(new_var3)]



### 4. mentsuk el az atnevezett valtozoju tablakat ###

setwd("C:/Users/Gabor/Documents/00_Vallalkozas/02_PPK/adatok/clean-data")

write_sav(df2, "pozitiv_2.sav")
write_sav(df_n2, "negativ_2.sav")
write_sav(df_p2, "kontrol_2.sav")



### 5. vizualise missing data

library(visdat)
# s: https://cran.r-project.org/web/packages/visdat/readme/README.html

vis_miss(df2[1:60])
vis_miss(df2[61:101]) # kontaktot alig valaszoltak meg
vis_miss(df2[102:163])

names(df_p2)
vis_miss(df_p2[1:60])
vis_miss(df_p2[61:101]) # kontaktot alig valaszoltak meg
vis_miss(df_p2[102:142])

vis_miss(df_n2[1:60])
vis_miss(df_n2[61:101]) # kontaktot alig valaszoltak meg
vis_miss(df_n2[102:141])

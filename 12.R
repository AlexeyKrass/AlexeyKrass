setwd("D:/Group 124/Mathmod Gorbanevskiy/Mathmod")
tbl = read_csv("eddypro.csv") 
tbl 
class(tbl) 
names(tbl) 
tbl = read_csv("eddypro.csv",skip = 1) 
tbl = read_csv("eddypro.csv",skip = 1, comment=c("[")) 
tbl = read_csv("eddypro.csv",skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("[")) 
tbl = tbl[-1,] 
tbl 
tbl = select(tbl, -(roll)) 
tbl<-tbl[,c(-1,-3,-9,-12,-15,-18,-21,-30,-35,-70,-88:-99)] 
names(tbl) 
#отбор показателей весеннего ночного периода
filter(tbl,DOY>90 & DOY<150) 
filter(tbl,daytime == FALSE) 
tbl = tbl %>% mutate_if(is.character, factor) 
names(tbl) = str_replace_all(names(tbl), "[!]","_emph_") 
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(tbl) 
sapply(tbl,is.numeric) 
tbl_numeric = tbl[,sapply(tbl,is.numeric) ] 
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ] 
cor_td = cor(tbl_numeric) 
cor_td 
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(co2_flux) 
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude 
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep="")) 
formula 
row_numbers = 1:length(tbl$date) 
teach = sample(row_numbers, floor(length(tbl$date)*.7)) 
test = row_numbers[-teach] 
teaching_tbl_unq = tbl[teach,] 
testing_tbl_unq = tbl[test,] 
mod = lm(formula, data=tbl) 
mod 
coef(mod) 
resid(mod) 
confint(mod) 
summary(mod)
  anova(mod)
выкинуть переменные, взять между собой взаимодействие (переменные)^2
  R2 не меньше 0.7
  
mod1 = lm(co2_flux~(DOY+Tau+rand_err_Tau + H+LE+ rand_err_LE + rand_err_h2o_flux + H_strg + h2o_time_lag + sonic_temperature + 
              air_temperature+air_density+air_molar_volume+es+RH+VPD+u_star_+T_star_+un_H+un_LE+un_co2_flux+un_h2o_flux+h2o_var+
                w_div_ts_cov+w_div_co2_cov+w_div_h2o_cov+flowrate)^2,data=tbl)
mod1
coef(mod1) 
resid(mod1) 
confint(mod1) 
summary(mod1)
anova(mod1)
mod2=lm(co2_flux~(DOY+Tau+rand_err_Tau + H+LE+ rand_err_LE + rand_err_h2o_flux + H_strg + h2o_time_lag + sonic_temperature + 
                     air_temperature+air_density+air_molar_volume+es+RH+VPD+u_star_+T_star_+un_H+un_LE+un_co2_flux+un_h2o_flux+h2o_var+
                     w_div_ts_cov+w_div_co2_cov+w_div_h2o_cov)^2-DOY:LE-DOY:rand_err_h2o_flux-DOY:sonic_temperature-
                     DOY:u_star_-DOY:es-DOY:u_star_-Tau:air_density-Tau:VPD-Tau:u_star_-Tau:h2o_var-rand_err_Tau:RH-rand_err_Tau:VPD-
                     H:H_strg-H:rand_err_h2o_flux-H:es-H:RH-H:VPD-H:T_star_-H:un_H-H:h2o_var-H:w_div_h2o_cov-LE:rand_err_LE-
                      LE:rand_err_h2o_flux-LE:rand_err_h2o_flux-LE:es-LE:RH-LE:VPD-LE:un_co2_flux-LE:w_div_co2_cov-rand_err_LE:rand_err_h2o_flux-
                     rand_err_LE:air_molar_volume-rand_err_LE:VPD-rand_err_LE:T_star_-rand_err_LE:h2o_var-rand_err_LE:h2o_var,data=tbl)
mod2
coef(mod2) 
resid(mod2) 
confint(mod2) 
summary(mod2)
anova(mod2)
mod3=lm(co2_flux~(DOY+Tau+rand_err_Tau + H+LE+ rand_err_LE + rand_err_h2o_flux + H_strg + h2o_time_lag + sonic_temperature + 
                    air_temperature+air_density+air_molar_volume+es+RH+VPD+T_star_+un_H+un_LE+un_co2_flux+un_h2o_flux+h2o_var+
                    w_div_ts_cov+w_div_co2_cov+w_div_h2o_cov)^2-DOY:air_temperature-DOY:w_div_ts_cov-Tau:rand_err_h2o_flux-
          Tau:h2o_time_lag -Tau:air_molar_volume -rand_err_Tau:rand_err_LE-rand_err_Tau:H_strg-rand_err_Tau:h2o_time_lag-
          rand_err_Tau:air_molar_volume-rand_err_Tau:un_h2o_flux-H:air_density-LE:H_strg -LE:h2o_time_lag-LE:un_H-LE:un_h2o_flux-
          rand_err_LE:h2o_time_lag-rand_err_LE:es -rand_err_LE:un_co2_flux-rand_err_LE:w_div_h2o_cov-rand_err_LE:w_div_h2o_cov-
          rand_err_h2o_flux:air_molar_volume-rand_err_h2o_flux:RH-rand_err_h2o_flux:T_star_-rand_err_h2o_flux:h2o_var-rand_err_h2o_flux:w_div_h2o_cov-
          H_strg:h2o_time_lag-H_strg:VPD-H_strg:un_h2o_flux-H_strg:w_div_ts_cov -H_strg:w_div_h2o_cov -h2o_time_lag:sonic_temperature-
          h2o_time_lag:air_temperature -h2o_time_lag:air_temperature,data=tbl) 
mod3
coef(mod3) 
resid(mod3) 
confint(mod3) 
summary(mod3)
anova(mod3)         
mod4=lm(co2_flux~(DOY+Tau+rand_err_Tau + H+LE+ rand_err_LE + rand_err_h2o_flux + H_strg + h2o_time_lag + sonic_temperature + 
                       air_temperature+air_density+air_molar_volume+es+RH+VPD+T_star_+un_H+un_LE+un_co2_flux+un_h2o_flux+h2o_var+
                       w_div_ts_cov+w_div_co2_cov+w_div_h2o_cov)^2-DOY:LE-DOY:rand_err_h2o_flux-Tau:air_temperature-Tau:air_density-
                rand_err_Tau:rand_err_h2o_flux-rand_err_Tau:sonic_temperature-rand_err_Tau:VPD-rand_err_Tau:un_co2_flux-rand_err_Tau:h2o_var-
          H:rand_err_h2o_flux-H:H_strg-H:es-H:T_star_-H:h2o_var-H:w_div_h2o_cov-LE:rand_err_LE-LE:rand_err_h2o_flux-LE:es-LE:RH-LE:VPD-LE:un_LE   -
          LE:un_co2_flux-rand_err_LE:rand_err_h2o_flux-rand_err_LE:air_molar_volume-rand_err_LE:VPD-rand_err_LE:w_div_co2_cov-rand_err_h2o_flux:H_strg-
          rand_err_h2o_flux:air_density-rand_err_h2o_flux:un_H-rand_err_h2o_flux:un_h2o_flux-rand_err_h2o_flux:w_div_ts_cov-H_strg:air_density-H_strg:w_div_co2_cov-
          h2o_time_lag:air_molar_volume-h2o_time_lag:RH-h2o_time_lag:VPD-h2o_time_lag:T_star_,data=tbl) 
mod4
coef(mod4) 
resid(mod4) 
confint(mod4) 
summary(mod4)
anova(mod4)        
mod5=lm(co2_flux~(DOY+Tau+rand_err_Tau + H+LE+ rand_err_LE + rand_err_h2o_flux + H_strg + h2o_time_lag + sonic_temperature + 
                    air_temperature+air_density+air_molar_volume+es+RH+VPD+T_star_+un_H+un_LE+un_co2_flux+un_h2o_flux+h2o_var+
                    w_div_ts_cov+w_div_h2o_cov)^2-DOY:es-DOY:w_div_ts_cov-Tau:air_molar_volume-Tau:h2o_var-rand_err_Tau:H_strg-
          rand_err_Tau:h2o_time_lag-rand_err_Tau:air_temperature-rand_err_Tau:air_density -H:air_density-H:RH-H:VPD-H:un_H-H:un_LE-
          LE:H_strg-LE:h2o_time_lag-LE:un_h2o_flux -rand_err_LE:es -rand_err_LE:RH-rand_err_LE:T_star_-rand_err_LE:un_co2_flux -
          rand_err_LE:un_h2o_flux-rand_err_LE:h2o_var-rand_err_LE:w_div_h2o_cov-rand_err_h2o_flux:RH-rand_err_LE:T_star_-rand_err_LE:un_co2_flux-rand_err_LE:un_h2o_flux-
          rand_err_LE:h2o_var-rand_err_LE:w_div_h2o_cov-rand_err_h2o_flux:RH-H_strg:h2o_time_lag-H_strg:air_molar_volume-H_strg:w_div_h2o_cov-
          h2o_time_lag:sonic_temperature-h2o_time_lag:air_density-h2o_time_lag:es-h2o_time_lag:un_h2o_flux-h2o_time_lag:w_div_co2_cov,data=tbl)
mod5
coef(mod5) 
resid(mod5) 
confint(mod5) 
summary(mod5)
anova(mod5) 
mod6=lm(co2_flux~(DOY+Tau+rand_err_Tau + H+LE+ rand_err_LE + rand_err_h2o_flux + H_strg + h2o_time_lag + sonic_temperature + 
                    air_temperature+air_density+air_molar_volume+es+RH+VPD+T_star_+un_H+un_LE+un_co2_flux+un_h2o_flux+h2o_var+
                    w_div_ts_cov+w_div_h2o_cov)^2-DOY:LE-DOY:VPD-Tau:air_temperature-Tau:air_density-Tau:VPD-rand_err_Tau:rand_err_LE-
          rand_err_Tau:rand_err_h2o_flux-rand_err_Tau:air_molar_volume-rand_err_Tau:VPD-rand_err_Tau:un_h2o_flux-H:H_strg-H:w_div_h2o_cov-LE:rand_err_LE-
          LE:rand_err_h2o_flux-LE:es-LE:un_H-LE:w_div_ts_cov-rand_err_LE:H_strg-rand_err_LE:h2o_time_lag -rand_err_LE:air_molar_volume -rand_err_LE:VPD -
          rand_err_h2o_flux:air_density-rand_err_h2o_flux:air_molar_volume-rand_err_h2o_flux:un_h2o_flux-rand_err_h2o_flux:h2o_var-H_strg:air_temperature-
          H_strg:air_density- h2o_time_lag:air_molar_volume-h2o_time_lag:RH-h2o_time_lag:VPD-sonic_temperature:air_temperature-sonic_temperature:air_molar_volume,data=tbl)
mod6
coef(mod6) 
resid(mod6) 
confint(mod6) 
summary(mod6)
anova(mod6)
mod7=lm(co2_flux~(DOY+Tau+rand_err_Tau + H+LE+ rand_err_LE + rand_err_h2o_flux + H_strg + h2o_time_lag + sonic_temperature + 
                    air_temperature+air_density+air_molar_volume+es+RH+VPD+T_star_+un_H+un_LE+un_co2_flux+un_h2o_flux+h2o_var+
                    w_div_ts_cov+w_div_h2o_cov)^2-DOY:rand_err_h2o_flux-DOY:air_density-DOY:air_molar_volume -DOY:un_h2o_flux-DOY:w_div_ts_cov-
          Tau:sonic_temperature-Tau:air_molar_volume-Tau:h2o_var-rand_err_Tau:H_strg-rand_err_Tau:h2o_time_lag-rand_err_Tau:air_density-
          rand_err_Tau:h2o_var-H:rand_err_h2o_flux-H:es-H:RH-H:T_star_-H:un_h2o_flux-H:h2o_var-LE:H_strg-LE:h2o_time_lag-LE:RH-LE:VPD-LE:T_star_-LE:un_LE-LE:un_h2o_flux-rand_err_LE:air_temperature -
          rand_err_LE:es-rand_err_LE:un_co2_flux-rand_err_LE:un_h2o_flux-rand_err_LE:w_div_h2o_cov-rand_err_h2o_flux:h2o_time_lag-
          rand_err_h2o_flux:sonic_temperature-rand_err_h2o_flux:T_star_-rand_err_h2o_flux:un_co2_flux-rand_err_h2o_flux:w_div_ts_cov-rand_err_h2o_flux:w_div_h2o_cov-
          H_strg:h2o_time_lag-H_strg:RH -H_strg:un_h2o_flux-H_strg:w_div_ts_cov-h2o_time_lag:air_temperature-h2o_time_lag:air_density-h2o_time_lag:es-sonic_temperature:air_density-sonic_temperature:es,data=tbl)
mod7
coef(mod7) 
resid(mod7) 
confint(mod7) 
summary(mod7)
anova(mod7)

plot(mod7)


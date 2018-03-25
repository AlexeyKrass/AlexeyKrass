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

mod1 = lm(co2_flux~(DOY+Tau+rand_err_Tau + H+LE+ rand_err_LE +h2o_flux+ rand_err_h2o_flux + H_strg + h2o_time_lag + sonic_temperature + 
                      air_temperature+air_density+air_molar_volume+RH+T_star_+un_H+un_LE+un_co2_flux+un_h2o_flux+
                      w_div_ts_cov+w_div_h2o_cov)^2,data=tbl)
mod1
coef(mod1) 
resid(mod1) 
confint(mod1) 
summary(mod1)
anova(mod1)
mod2=lm(co2_flux~(DOY+Tau+H+LE+ rand_err_LE +h2o_flux+ rand_err_h2o_flux + H_strg + h2o_time_lag + sonic_temperature + 
                    air_temperature+air_density+air_molar_volume+RH+T_star_+un_H+un_LE+un_co2_flux+un_h2o_flux+
                    w_div_ts_cov+w_div_h2o_cov)^2-DOY:LE-DOY:h2o_flux-DOY:H_strg-DOY:sonic_temperature,data=tbl)
mod2
coef(mod2) 
resid(mod2) 
confint(mod2) 
summary(mod2)
anova(mod2)
mod3=lm(co2_flux~(DOY+Tau+H+LE+ rand_err_LE +h2o_flux+ rand_err_h2o_flux + H_strg + h2o_time_lag + sonic_temperature + 
                    air_temperature+air_density+air_molar_volume+RH+T_star_+un_H+un_LE+un_co2_flux+un_h2o_flux+
                    w_div_ts_cov+w_div_h2o_cov)^2-DOY:LE-DOY:h2o_flux-DOY:H_strg-DOY:sonic_temperature-DOY:un_h2o_flux-DOY:w_div_ts_cov-Tau:sonic_temperature-Tau:air_density-
          Tau:w_div_h2o_cov-H:LE-H:rand_err_LE-H:h2o_flux-H:rand_err_h2o_flux-H:H_strg-H:h2o_time_lag-H:sonic_temperature-H:air_temperature-H:air_density-
          H:air_molar_volume-H:RH-H:T_star_-H:un_H-H:un_h2o_flux-H:w_div_ts_cov-H:w_div_h2o_cov-LE:rand_err_LE-LE:h2o_flux-LE:rand_err_h2o_flux-
          LE:H_strg-LE:h2o_time_lag-LE:T_star_-LE:un_LE-LE:un_co2_flux-LE:un_h2o_flux-LE:w_div_ts_cov-LE:w_div_h2o_cov-rand_err_LE:H_strg-
          rand_err_LE:h2o_time_lag-rand_err_h2o_flux:sonic_temperature- rand_err_h2o_flux:air_density -rand_err_h2o_flux:air_molar_volume-rand_err_h2o_flux:RH-rand_err_h2o_flux:un_H -rand_err_h2o_flux:un_h2o_flux-
          rand_err_h2o_flux:w_div_ts_cov-rand_err_h2o_flux:w_div_h2o_cov-H_strg:h2o_time_lag-H_strg:sonic_temperature-H_strg:air_temperature-H_strg:air_density-H_strg:RH-H_strg:un_co2_flux-
          H_strg:w_div_h2o_cov-h2o_time_lag:sonic_temperature-h2o_time_lag:air_temperature- h2o_time_lag:air_density-h2o_time_lag:air_molar_volume-h2o_time_lag:RH-
        h2o_time_lag:un_co2_flux-h2o_time_lag:un_h2o_flux- h2o_time_lag:w_div_ts_cov-sonic_temperature:air_temperature-sonic_temperature:air_density - sonic_temperature:air_molar_volume-
        sonic_temperature:RH-sonic_temperature:un_LE-sonic_temperature:un_co2_flux-sonic_temperature:w_div_ts_cov-Tau:air_temperature-Tau:air_molar_volume-LE:air_temperature-
          LE:un_H-rand_err_LE:h2o_flux-rand_err_LE:rand_err_h2o_flux-rand_err_LE:air_temperature-rand_err_LE:air_molar_volume-rand_err_LE:RH-rand_err_LE:un_h2o_flux-rand_err_LE:w_div_ts_cov-rand_err_LE:w_div_h2o_cov-
          h2o_flux:H_strg-h2o_flux:h2o_time_lag-h2o_flux:sonic_temperature-h2o_flux:air_density-h2o_flux:RH-h2o_flux:un_H-h2o_flux:un_LE-h2o_flux:un_co2_flux-
        h2o_flux:un_h2o_flux-h2o_flux:w_div_ts_cov-h2o_flux:w_div_h2o_cov-rand_err_h2o_flux:H_strg-rand_err_h2o_flux:h2o_time_lag-rand_err_h2o_flux:T_star_-H_strg:air_molar_volume-H_strg:un_h2o_flux-H_strg:w_div_ts_cov-
          sonic_temperature:un_h2o_flux-air_temperature:air_density-air_temperature:air_molar_volume-air_temperature:RH-air_temperature:un_co2_flux-air_density:air_molar_volume-
          air_density:un_LE-air_density:w_div_ts_cov-air_molar_volume:RH-air_molar_volume:un_H-air_molar_volume:un_co2_flux-air_molar_volume:w_div_ts_cov-air_molar_volume:w_div_h2o_cov-
          RH:un_H-RH:un_LE-RH:w_div_ts_cov-RH:w_div_h2o_cov-T_star_:un_h2o_flux-T_star_:w_div_ts_cov-T_star_:w_div_h2o_cov-un_H:un_h2o_flux-un_H:w_div_h2o_cov-
          un_LE:w_div_ts_cov-un_co2_flux:un_h2o_flux-un_co2_flux:w_div_ts_cov-un_co2_flux:w_div_h2o_cov- un_h2o_flux:w_div_ts_cov- un_h2o_flux:w_div_h2o_cov-w_div_ts_cov:w_div_h2o_cov,data=tbl)  
mod3
coef(mod3) 
resid(mod3) 
confint(mod3) 
summary(mod3)
anova(mod3)         
mod4=lm(co2_flux~(DOY+Tau+H+LE+ rand_err_LE +h2o_flux+ rand_err_h2o_flux + H_strg + h2o_time_lag + sonic_temperature + 
                    air_temperature+air_density+air_molar_volume+RH+T_star_+un_H+un_LE+un_co2_flux+un_h2o_flux+
                    w_div_ts_cov+w_div_h2o_cov)^2-DOY:LE-DOY:h2o_flux-DOY:H_strg-DOY:sonic_temperature-DOY:un_h2o_flux-DOY:w_div_ts_cov-Tau:sonic_temperature-Tau:air_density-
          Tau:w_div_h2o_cov-H:LE-H:rand_err_LE-H:h2o_flux-H:rand_err_h2o_flux-H:H_strg-H:h2o_time_lag-H:sonic_temperature-H:air_temperature-H:air_density-
          H:air_molar_volume-H:RH-H:T_star_-H:un_H-H:un_h2o_flux-H:w_div_ts_cov-H:w_div_h2o_cov-LE:rand_err_LE-LE:h2o_flux-LE:rand_err_h2o_flux-
          LE:H_strg-LE:h2o_time_lag-LE:T_star_-LE:un_LE-LE:un_co2_flux-LE:un_h2o_flux-LE:w_div_ts_cov-LE:w_div_h2o_cov-rand_err_LE:H_strg-
          rand_err_LE:h2o_time_lag-rand_err_h2o_flux:sonic_temperature- rand_err_h2o_flux:air_density -rand_err_h2o_flux:air_molar_volume-rand_err_h2o_flux:RH-rand_err_h2o_flux:un_H -rand_err_h2o_flux:un_h2o_flux-
          rand_err_h2o_flux:w_div_ts_cov-rand_err_h2o_flux:w_div_h2o_cov-H_strg:h2o_time_lag-H_strg:sonic_temperature-H_strg:air_temperature-H_strg:air_density-H_strg:RH-H_strg:un_co2_flux-
          H_strg:w_div_h2o_cov-h2o_time_lag:sonic_temperature-h2o_time_lag:air_temperature- h2o_time_lag:air_density-h2o_time_lag:air_molar_volume-h2o_time_lag:RH-
          h2o_time_lag:un_co2_flux-h2o_time_lag:un_h2o_flux- h2o_time_lag:w_div_ts_cov-sonic_temperature:air_temperature-sonic_temperature:air_density - sonic_temperature:air_molar_volume-
          sonic_temperature:RH-sonic_temperature:un_LE-sonic_temperature:un_co2_flux-sonic_temperature:w_div_ts_cov-Tau:air_temperature-Tau:air_molar_volume-LE:air_temperature-
          LE:un_H-rand_err_LE:h2o_flux-rand_err_LE:rand_err_h2o_flux-rand_err_LE:air_temperature-rand_err_LE:air_molar_volume-rand_err_LE:RH-rand_err_LE:un_h2o_flux-rand_err_LE:w_div_ts_cov-rand_err_LE:w_div_h2o_cov-
          h2o_flux:H_strg-h2o_flux:h2o_time_lag-h2o_flux:sonic_temperature-h2o_flux:air_density-h2o_flux:RH-h2o_flux:un_H-h2o_flux:un_LE-h2o_flux:un_co2_flux-
          h2o_flux:un_h2o_flux-h2o_flux:w_div_ts_cov-h2o_flux:w_div_h2o_cov-rand_err_h2o_flux:H_strg-rand_err_h2o_flux:h2o_time_lag-rand_err_h2o_flux:T_star_-H_strg:air_molar_volume-H_strg:un_h2o_flux-H_strg:w_div_ts_cov-
          sonic_temperature:un_h2o_flux-air_temperature:air_density-air_temperature:air_molar_volume-air_temperature:RH-air_temperature:un_co2_flux-air_density:air_molar_volume-
          air_density:un_LE-air_density:w_div_ts_cov-air_molar_volume:RH-air_molar_volume:un_H-air_molar_volume:un_co2_flux-air_molar_volume:w_div_ts_cov-air_molar_volume:w_div_h2o_cov-
          RH:un_H-RH:un_LE-RH:w_div_ts_cov-RH:w_div_h2o_cov-T_star_:un_h2o_flux-T_star_:w_div_ts_cov-T_star_:w_div_h2o_cov-un_H:un_h2o_flux-un_H:w_div_h2o_cov-
          un_LE:w_div_ts_cov-un_co2_flux:un_h2o_flux-un_co2_flux:w_div_ts_cov-un_co2_flux:w_div_h2o_cov- un_h2o_flux:w_div_ts_cov- un_h2o_flux:w_div_h2o_cov-w_div_ts_cov:w_div_h2o_cov-LE:air_density-rand_err_h2o_flux:un_LE-
          H_strg:un_LE-h2o_time_lag:un_LE-h2o_time_lag:w_div_h2o_cov-sonic_temperature:w_div_h2o_cov-air_density:RH-air_density:un_co2_flux-air_density:w_div_h2o_cov-
          RH:un_h2o_flux-un_H:un_LE-un_LE:un_h2o_flux-air_temperature:un_LE-air_density:un_h2o_flux-un_LE:w_div_h2o_cov-rand_err_LE:air_density-air_temperature:w_div_ts_cov-RH:T_star_,data=tbl)
mod4
coef(mod4) 
resid(mod4) 
confint(mod4) 
summary(mod4)
anova(mod4)        
#Модель идеально описывает данные
plot(mod4)


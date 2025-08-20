library(car)
library(lattice)
library(xtable)
library(dplyr)


# 读取所需数据
CAP_data <- growth_accounts[growth_accounts$var == "CAP",]
VA_CP_data <- growth_accounts[growth_accounts$var == "VA_CP",]
LP1_G_data <- growth_accounts[growth_accounts$var == "LP1_G",]
Kq_data <- data_frame(nace_r2_code=capital_accounts$nace_r2_code,geo_code=capital_accounts$geo_code,nace_r2_name=capital_accounts$nace_r2_name,geo_name=capital_accounts$geo_name,year=capital_accounts$year,Kq_GFCF=capital_accounts$Kq_GFCF)
VA_Q_data <- national_accounts[c("nace_r2_code","geo_code","nace_r2_name","geo_name","year","VA_Q")]
EMP_data <- national_accounts[c("nace_r2_code","geo_code","nace_r2_name","geo_name","year","H_EMP")]

# 整合

CAP_data <- CAP_data %>% rename(CAP = value)%>% 
  select(-var)
VA_CP_data <- VA_CP_data %>% rename(VA_CP = value)%>% 
  select(-var)
LP1_G_data <- LP1_G_data %>% rename(LP1_G = value)%>% 
  select(-var)

join_keys <- c("nace_r2_code","geo_code","nace_r2_name","geo_name","year")

CAP_data <- CAP_data %>% mutate(across(all_of(join_keys), as.character))
VA_CP_data <- VA_CP_data %>% mutate(across(all_of(join_keys), as.character))
LP1_G_data <- LP1_G_data %>% mutate(across(all_of(join_keys), as.character))
Kq_data <- Kq_data %>% mutate(across(all_of(join_keys), as.character))
VA_Q_data <- VA_Q_data %>% mutate(across(all_of(join_keys), as.character))

aggre_data <- CAP_data %>%
  inner_join(VA_CP_data, by = join_keys) %>%
  inner_join(LP1_G_data, by = join_keys) %>%
  inner_join(Kq_data,by = join_keys)%>%
  inner_join(VA_Q_data,by = join_keys)


aggre_data$CP <- aggre_data$VA_Q/aggre_data$Kq_GFCF
aggre_data$gamma <- (lead(aggre_data$LP1_G))/100
aggre_data$chi <- (lead(aggre_data$CP)/aggre_data$CP) - 1
aggre_data$pi <- aggre_data$CAP/aggre_data$VA_CP
aggre_data$omega <- (aggre_data$gamma*(1+aggre_data$chi))/(aggre_data$gamma-aggre_data$chi)

# 判断可行性条件
aggre_data <- aggre_data %>%
  mutate(
    viability_met = ifelse(is.na(gamma) | is.na(chi), 
                           NA,                        
                           ifelse((gamma > chi & omega > pi) | (gamma <= chi & omega <= pi), 
                                  "yes", 
                                  "no"))
  )

#判断技术进步模式
aggre_data <- aggre_data %>%
  mutate(
    TCpattern = ifelse(gamma > 0 & chi < 0, "MBTC",
                       ifelse(gamma < 0 & chi > 0, "RMBTC",
                              ifelse(gamma > 0 & chi > 0, "both-strengthen", "neither")))
  )


# 按国家地区拆分
country_list <- split(aggre_data, aggre_data$geo_code)
str(country_list, max.level = 1) 
names(country_list)

# 计算总体
total_CAP_by_country <- CAP_data %>%
  group_by(geo_name, year) %>%
  summarise(total_CAP = sum(CAP, na.rm = TRUE)) 

total_VA_CP_by_country <- growth_accounts %>%
  filter(var == "VA_CP") %>%
  group_by(geo_name, year) %>%
  summarise(total_VA_CP = sum(value, na.rm = TRUE))

total_Kq_GFCF_by_country <- capital_accounts %>%
  group_by(geo_name, year) %>%
  summarise(total_Kq_GFCF = sum(Kq_GFCF, na.rm = TRUE))

total_VA_Q_by_country <- national_accounts %>%
  group_by(geo_name, year) %>%
  summarise(total_VA_Q = sum(VA_Q, na.rm = TRUE))

total_H_EMP_by_country <- national_accounts %>%
  group_by(geo_name, year) %>%
  summarise(total_H_EMP = sum(H_EMP, na.rm = TRUE))

LP_total <- inner_join(
  total_VA_Q_by_country,
  total_H_EMP_by_country,
  by = c("geo_name", "year")
)

LP_total1 <- LP_total %>%
  arrange(geo_name, year) %>%
  group_by(geo_name) %>%
  mutate(
    LP = total_VA_Q / total_H_EMP,
    gamma_total = (lead(LP) / LP) - 1
  ) %>%
  ungroup()%>%
  mutate(
    geo_name = as.character(geo_name),
    year = as.character(year)
  )

CP_total <- inner_join(
  total_VA_Q_by_country,
  total_Kq_GFCF_by_country,
  by = c("geo_name", "year")
)

CP_total1 <- CP_total %>%
  arrange(geo_name, year) %>%
  group_by(geo_name) %>%
  mutate(
    CP = total_VA_Q / total_Kq_GFCF,
    chi_total = (lead(CP) / CP) - 1
  ) %>%
ungroup()%>%
  mutate(
    geo_name = as.character(geo_name),
    year = as.character(year)
  )

factor_income_data <- inner_join(
  total_CAP_by_country,
  total_VA_CP_by_country,
  by = c("geo_name", "year")
)

capital_share_data <- factor_income_data %>%
  arrange(geo_name, year) %>%
  mutate(
    pi_total = ifelse(total_VA_CP > 0, total_CAP / total_VA_CP, NA)
  ) %>%
  mutate(
    geo_name = as.character(geo_name),
    year = as.character(year)
  )


total_data <- LP_total1 %>%
  inner_join(CP_total1, by = c("geo_name", "year")) %>%
  inner_join(capital_share_data, by = c("geo_name", "year"))

total_data <- total_data %>%
  mutate(
    omega_total = gamma_total*(1+chi_total)/(gamma_total-chi_total)
    )

# 判断可行性条件
total_data <- total_data %>%
  mutate(
    viability_met = ifelse(is.na(gamma_total) | is.na(chi_total), 
                           NA,                        
                           ifelse((gamma_total > chi_total & omega_total > pi_total) | (gamma_total <= chi_total & omega_total <= pi_total), 
                                  "yes", 
                                  "no"))
  )

#判断技术进步模式
total_data <- total_data %>%
  mutate(
    TCpattern = ifelse(gamma_total > 0 & chi_total < 0, "MBTC",
                       ifelse(gamma_total < 0 & chi_total > 0, "RMBTC",
                              ifelse(gamma_total > 0 & chi_total > 0, "both-strengthen", "neither")))
  )

# 按国家地区拆分
total_country_list <- split(total_data, total_data$geo_name)
str(total_country_list, max.level = 1) 
names(total_country_list)

# 计数
summary_list <- lapply(country_list, function(df) {
  df %>%
    count(TCpattern, viability_met, name = "count") %>%
    na.omit()
})
counts <- table(aggre_data$geo_name, aggre_data$nace_r2_name, aggre_data$viability_met)
counts_t <- as.data.frame(counts)
colnames(counts_t) <- c("geo_name", "nace_r2_name", "viability_met", "counts") 


KLEMS <- list("基本数据" = aggre_data,
              "计数" = counts_t)
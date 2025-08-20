# package to use
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)

# read value added,capital compensation,capital stock,growth of labor productivity,PPI
VA <- read_excel("CIP_4.0_(2023)_1.2.xlsx")
Cap_VA <- read_excel("CIP_4.0_(2023)_1.4.xlsx",sheet="Capital compensation")
PPI <- read_excel("CIP_4.0_(2023)_1.6.xlsx")
Cap_S <- read_excel("CIP_4.0_(2023)_2.2.xlsx")
LAB <- read_excel("CIP_4.0_(2023)_3.2.xlsx")
G_LP <- read_excel("CIP_4.0_(2023)_4.xlsx",sheet = "g(LP)")



# data preparation and cleaning
VA1 <- VA[-1:-2,-1:-3] 
CAP_VA1 <- Cap_VA[-1:-2,-1:-2]
CAP_S1 <- Cap_S[-1:-2,-1:-2]
G_LP1 <- G_LP[-1:-2,-1:-3]
gamma <- cbind(G_LP1,NA)
PPI1 <- PPI[-1:-2,-1:-2]
LAB1 <- LAB[-1:-2,-1:-2]
PPI_fac <- PPI1/100

# calculate the cumulative product
fixed_base_index_factors <- t(apply(PPI_fac, 1, cumprod))
fixed_base_ppi <- as.data.frame(fixed_base_index_factors * 100)
colnames(fixed_base_ppi) <- colnames(PPI1)

# calculate real value added
VA_real <- (VA1/fixed_base_ppi)*100

# calculate the sum
total_real <- VA_real %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))
VA_real <- bind_rows(VA_real,total_real)
total_CAP_S <- CAP_S1 %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))
CAP_S1 <- bind_rows(CAP_S1,total_CAP_S)
total_CAP_VA <- CAP_VA1 %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))
CAP_VA1 <- bind_rows(CAP_VA1,total_CAP_VA)
total_LAB <- LAB1 %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))
LAB1 <- bind_rows(LAB1,total_LAB)
total_VA <- VA1 %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))
VA1 <- bind_rows(VA1,total_VA)
total_LAB <- total_real/total_LAB
total_gamma <- (total_LAB[-1]-total_LAB[-length(total_LAB)])/total_LAB[-length(total_LAB)]
total_gamma <- cbind(total_gamma,NA)

# share of capital compensation
pi <- CAP_VA1/VA1
#growth of factor productivity
rho <- VA_real/CAP_S1
gamma <- bind_rows(gamma,total_gamma)
rho1 <- rho[,2:ncol(rho)] 
rho2 <- rho[,1:(ncol(rho)-1)]
chi <- cbind((rho1-rho2)/rho2,NA)

# viability parameter
omega <- gamma*(1+chi)/(gamma-chi)
gamma <- as.data.frame(lapply(gamma, as.numeric))
rho <- as.data.frame(lapply(rho, as.numeric))
omega <- as.data.frame(lapply(omega, as.numeric))
pi <- as.data.frame(lapply(pi, as.numeric))

col <- Cap_S[2, -1:-2]
row_industries <- Cap_VA[-1:-2,2]
row <- Cap_VA[-1:-2,2]
col1 <- as.character(unlist(Cap_S[2, -1:-2]))
row1 <- c(as.character(unlist(row_industries)), "Total")

# viability condition and pattern of technical change
viability <- ifelse(is.na(gamma) | is.na(chi), 
                    NA, 
                    ifelse((gamma > chi & omega > pi) | (gamma <= chi & omega <= pi), 
                           "yes", 
                           "no"))
viability_check<- as.data.frame(viability)

TCpattern <- ifelse(gamma > 0 & chi < 0, "MBTC",
                    ifelse(gamma < 0 & chi > 0, "RMBTC",
                           ifelse(gamma > 0 & chi > 0, "both-strengthen", "neither")))
pattern <- as.data.frame(TCpattern)

# combination
combined <- paste(as.matrix(pattern), as.matrix(viability_check), sep = " and ")
combined_matrix_reshaped <- matrix(combined, 
                                   nrow = nrow(pattern), 
                                   ncol = ncol(pattern))
combined_df <- as.data.frame(combined_matrix_reshaped)

colnames(viability_check) <- col1
rownames(viability_check) <- row1
colnames(gamma) <- col1
rownames(gamma) <- row1
colnames(chi) <- col1
rownames(chi) <- row1
colnames(pi) <- col1
rownames(pi) <- row1
colnames(omega) <- col1
rownames(omega) <- row1
colnames(pattern) <- col1
rownames(pattern) <- row1
colnames(combined_df) <- col1
rownames(combined_df) <- row1


# count
row_yes_counts <- apply(viability_check, 1, function(row) sum(row == "yes", na.rm = TRUE))
row_no_counts <- apply(viability_check, 1, function(row) sum(row == "no", na.rm = TRUE))
row_na_counts <- apply(viability_check, 1, function(row) sum(is.na(row)))

row_summary <- cbind(Yes_Count = row_yes_counts, 
                     No_Count = row_no_counts,
                     NA_Count = row_na_counts)

total_summary <- table(unlist(viability_check), useNA = "ifany")

row_summary <- as.data.frame(row_summary)
total_summary <- as.data.frame(total_summary)
colnames(total_summary) <- c("结果", "计数")

viability_vector <- unlist(viability_check)
tc_pattern_vector <- unlist(pattern)

contingency_table <- table(Pattern = tc_pattern_vector, Viability = viability_vector, useNA = "ifany")

contingency_df <- as.data.frame.matrix(contingency_table)

CHN_result <- list(
  "劳动生产率增长率" = gamma,
  "资本生产率增长率" = chi,
  "利润份额" = pi,
  "可行性参数" = omega,
  "可行性核查" = viability_check,
  "技术进步模式" = pattern,
  "总体结果" =combined_df,
  "可行性条件分行统计" = row_summary,
  "可行性条件总计" = total_summary,
  "按技术进步模式统计" = contingency_df
)


# package to use
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)

# read data
VA_CP <- read_excel("AT_growth accounts.xlsx",sheet="VA_CP")
Cap_VA <- read_excel("AT_growth accounts.xlsx",sheet="CAP")
G_LP <- read_excel("AT_growth accounts.xlsx",sheet="LP1_G")
VA_Q <- read_excel("AT_national accounts.xlsx",sheet="VA_Q")
EMP <- read_excel("AT_national accounts.xlsx",sheet="H_EMP")
Cap_S <- read_excel("AT_capital accounts.xlsx",sheet="Kq_GFCF")





# data preparation and cleaning
VA_CP <- VA_CP[-1:-5] 
Cap_VA <- Cap_VA[-1:-5]
G_LP <- G_LP[-1:-5]
VA_Q <- VA_Q[-1:-5]
EMP <- EMP[-1:-5]
Cap_S1 <- Cap_S[-1:-5]


# calculate the sum
total_VA_CP <- VA_CP %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))
VA_CP <- bind_rows(VA_CP,total_VA_CP)
total_VA_Q <- VA_Q %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))
VA_Q <- bind_rows(VA_Q,total_VA_Q)
total_Cap_S <- Cap_S %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))
Cap_S1 <- bind_rows(Cap_S1,total_Cap_S)
total_Cap_VA <- Cap_VA %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))
Cap_VA <- bind_rows(Cap_VA,total_Cap_VA)
total_EMP <- EMP %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))
total_LP <- total_VA_Q/total_EMP
total_LP1 <- total_LP[,-1]
total_LP2 <- total_LP[,-ncol(total_LP)]
total_gamma1 <- (total_LP1-total_LP2)/total_LP2
total_gamma <- total_LP
total_gamma[1, 1:(ncol(total_gamma)-1)] <- total_gamma1
total_gamma[1,ncol(total_gamma)] <- NA
# share of capital compensation
pi <- Cap_VA/VA_CP
#growth of factor productivity  
rho <- VA_Q/Cap_S1
rho1 <- rho[,2:ncol(rho)] 
rho2 <- rho[,1:(ncol(rho)-1)]
chi <- cbind((rho1-rho2)/rho2,NA)
G_LP1 <- G_LP
G_LP1[, 1:(ncol(G_LP) - 1)] <- G_LP[, 2:ncol(G_LP)]
G_LP1[,ncol(G_LP)] <- NA
gamma <- G_LP1/100
gamma <- bind_rows(gamma,total_gamma)


# viability parameter
omega <- gamma*(1+chi)/(gamma-chi)
gamma <- as.data.frame(lapply(gamma, as.numeric))
rho <- as.data.frame(lapply(rho, as.numeric))
omega <- as.data.frame(lapply(omega, as.numeric))
pi <- as.data.frame(lapply(pi, as.numeric))


row <- Cap_S[, 4]
row_industries <- Cap_S[,4]
row1 <- c(as.character(unlist(row_industries)), "Total")
col1 <- colnames(Cap_S)
col1 <- col1[-1:-5]
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

result <- list(
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


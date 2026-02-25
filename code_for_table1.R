
load("R.Rdata")
str(all_data)

selected_data <- all_data %>%
  select(
    Futility, Age, Gender, BMI, ASA, Malnutrition, Diabetes, 
    pre_hemoglobin, post_hemoglobin, percent_delta_hemoglobin, 
    pre_WBC, post_WBC, percent_delta_WBC, 
    pre_ALB, post_ALB, percent_delta_ALB, 
    pre_TBIL, post_TBIL, percent_delta_TBIL, 
    pre_DBIL, post_DBIL, percent_delta_DBIL, 
    pre_CEA, post_CEA, percent_delta_CEA, 
    pre_CA199, post_CA199, percent_delta_CA199, 
    pre_Diameter, post_Diameter, percent_delta_Diameter,
    Type_of_operation, Neoadjuvant_regimen, Degree_of_differentiation, 
    Head_location, Jaundice, DFS, DFS_time, OS, OS_time, "30d_Mortality", 
    "90d_Mortality",Death_within_6_mo, Recurrence_within_6_mo, Futility_of_surgery, group
  )

# 增加对数转换列
selected_data <- selected_data %>%
  mutate(
    log_pre_CA199 = log(pre_CA199),  # pre_CA199的自然对数
    log_post_CA199 = log(post_CA199)  # post_CA199的自然对数
  )

# 检查转换结果
summary(selected_data$log_pre_CA199)
summary(selected_data$log_post_CA199)

# 查看数据结构
str(selected_data)
# 的数据处理代码
final_data <- selected_data %>%
  mutate(
    # 仅转换变量类型，不重命名
    Futility = factor(Futility, 
                      levels = c(0, 1), 
                      labels = c("No", "Yes")),
    
    Gender = factor(Gender, 
                    levels = c(1, 2), 
                    labels = c("Female", "Male")),
    
    Malnutrition = factor(Malnutrition, 
                          levels = c(0, 1), 
                          labels = c("No", "Yes")),
    
    Diabetes = factor(Diabetes, 
                      levels = c(0, 1), 
                      labels = c("No", "Yes")),
    
    Head_location = factor(Head_location, 
                           levels = c(0, 1), 
                           labels = c("No", "Yes")),
    Jaundice = factor(Jaundice, 
                      levels = c(0, 1), 
                      labels = c("No", "Yes")),
    
    Type_of_operation = factor(Type_of_operation, 
                      levels = c(1, 2, 3), 
                      labels = c("Distal_pancreatectomy", "Pancreatoduodenectomy", "Total_pancreatectomy")),
    
    group = factor(group)
  ) %>%
  
  # 保留所有原始列（不移除任何列）
  # 按原始列名重新排列（示例顺序，请根据实际需求调整）
  select(
    Futility, Age, Gender, BMI, ASA, Malnutrition, Diabetes,
    pre_hemoglobin, post_hemoglobin, percent_delta_hemoglobin, 
    pre_WBC, post_WBC, percent_delta_WBC,
    pre_ALB, post_ALB, percent_delta_ALB,
    pre_TBIL, post_TBIL, percent_delta_TBIL,
    pre_DBIL, post_DBIL, percent_delta_DBIL,
    pre_CEA, post_CEA, percent_delta_CEA,
    pre_CA199, post_CA199, percent_delta_CA199,
    pre_Diameter, post_Diameter, percent_delta_Diameter,log_pre_CA199, log_post_CA199,
    Type_of_operation, Neoadjuvant_regimen, Degree_of_differentiation,
    Head_location, Jaundice, DFS, DFS_time, OS, OS_time,
    "30d_Mortality", "90d_Mortality", Death_within_6_mo, 
    Recurrence_within_6_mo, Futility_of_surgery, group
  )

final_data <- final_data %>%
  mutate(across(c("DFS", "OS", "30d_Mortality", "90d_Mortality", 
                  "Death_within_6_mo", "Recurrence_within_6_mo", 
                  "Futility_of_surgery"), 
                ~as.numeric(.))) %>%
  # 然后再转换为因子
  mutate(across(c("DFS", "OS", "30d_Mortality", "90d_Mortality", 
                  "Death_within_6_mo", "Recurrence_within_6_mo", 
                  "Futility_of_surgery"), 
                ~factor(., levels = c(0, 1), labels = c("No", "Yes"))))
# 检查数据结构
str(final_data)

# 保存结果
save(final_data, file = "数据14_table1数据集.Rdata")


library(compareGroups)

# 所有纳入人群基线特征
tab1 <- descrTable(~ ., data = final_data, method = NA)
print(tab1, digits = 1, big.mark = ",")


# 是否手术失败队列比较
tab2 <- descrTable(Futility ~ ., data = final_data, method = NA)
print(tab2, digits = 1, big.mark = ",")


# 训练开发队列比较
tab3 <- descrTable(group ~ ., data = final_data, method = NA)
print(tab3, digits = 1, big.mark = ",")


library(tableone)# tableone 包不支持对数据进行非正态的间距分布
tab1 <- CreateTableOne(data = final_data)
print(tab1, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
tab1Mat <- print(tab1, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)


tab2 <- CreateTableOne(strata = "Futile" , data = final_data)
print(tab2, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
tab2Mat <- print(tab2, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)


tab3 <- CreateTableOne(strata = "group" , data = final_data)
print(tab3, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
tab3Mat <- print(tab3, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

# 清空环境
rm(list = ls())
# 设置工作目录
setwd("D:/R/")

# 检查结果
table(sampled_data$Gender, sampled_data$Early_recurrence)

skimr::skim(sampled_data)

colnames(sampled_data)
library("autoReg")
overall_log <- glm(Early_recurrence ~ Sex + Age + BMI + 
                     Hb + WBC + 
                     PLT + LDH + 
                     Maximal_diameter + 
                     CYFRA21_1 + 
                     PNI + SII + TP53 +
                     Adjuvant_chemotherapy, 
                   data = sampled_data, 
                   family = binomial)

# 查看模型摘要
summary(overall_log)

model1<-autoReg(overall_log,uni=TRUE,multi=FALSE,threshold=0.05)
#只显示单因素
model1

model2<-autoReg(overall_log,uni=FALSE,milti=TRUE,threshold=0.05)
#只显示多因素
model2
model3<-autoReg(overall_log,uni=TRUE,milti=TRUE,threshold=0.05)
#单多因素
model3

# 保存模型结果到CSV文件
write.csv(as.data.frame(model3),
          file = "model3_results.csv",
          row.names = FALSE)



# 模型机器---R语言tidymodels包机器学习分类与回归模型---二分类---logistic回归
# https://parsnip.tidymodels.org/reference/details_logistic_reg_glm.html
##############################################################
# install.packages("tidymodels")
library(tidymodels)
source("tidyfuncs4cls2.R")
# 多核并行
library(doParallel)
registerDoParallel(
  makePSOCKcluster(
    max(1, (parallel::detectCores(logical = F))-1)
  )
)
set.seed(42)
# 重抽样设定-5折交叉验证
set.seed(42)
folds <- vfold_cv(traindata, v = 5, strata = Futility)
folds
# 数据预处理配方
datarecipe_logistic <- recipe(Futility ~ ., traindata)
datarecipe_logistic
# 设定模型
model_logistic <- logistic_reg(
  mode = "classification",
  engine = "glm"
)
model_logistic
# workflow
wk_logistic <- 
  workflow() %>%
  add_recipe(datarecipe_logistic) %>%
  add_model(model_logistic)
wk_logistic
# 训练模型
set.seed(42)
final_logistic <- wk_logistic %>%
  fit(traindata)
final_logistic
##################################################################
# 训练集预测评估
predtrain_logistic <- eval4cls2(
  model = final_logistic, 
  dataset = traindata, 
  yname = "Futility", 
  modelname = "Logistic", 
  datasetname = "traindata",
  cutoff = "yueden",
  positivelevel = yourpositivelevel,
  negativelevel = yournegativelevel
)
predtrain_logistic$prediction
predtrain_logistic$predprobplot
predtrain_logistic$rocplot
predtrain_logistic$prplot
predtrain_logistic$caliplot
predtrain_logistic$cmplot#13_混淆矩阵训练集33
predtrain_logistic$metrics
predtrain_logistic$diycutoff
predtrain_logistic$ksplot
# pROC包auc值及其置信区间
pROC::auc(predtrain_logistic$proc)
pROC::ci.auc(predtrain_logistic$proc)
# 预测评估测试集预测评估
predtest_logistic <- eval4cls2(
  model = final_logistic, 
  dataset = testdata, 
  yname = "Futility", 
  modelname = "Logistic", 
  datasetname = "testdata",
  cutoff = predtrain_logistic$diycutoff,
  positivelevel = yourpositivelevel,
  negativelevel = yournegativelevel
)
predtest_logistic$prediction
predtest_logistic$predprobplot
predtest_logistic$rocplot
predtest_logistic$prplot
predtest_logistic$caliplot
predtest_logistic$cmplot#13_混淆矩阵测试集33
predtest_logistic$metrics
predtest_logistic$diycutoff
predtest_logistic$ksplot
# pROC包auc值及其置信区间
pROC::auc(predtest_logistic$proc)
pROC::ci.auc(predtest_logistic$proc)
# ROC比较检验
pROC::roc.test(predtrain_logistic$proc, predtest_logistic$proc)
# 合并训练集和测试集上ROC曲线
predtrain_logistic$rocresult %>%
  bind_rows(predtest_logistic$rocresult) %>%
  mutate(dataAUC = paste(data, curvelab),
         dataAUC = forcats::as_factor(dataAUC)) %>%
  ggplot(aes(x = 1-specificity,
             y = sensitivity, 
             color = dataAUC)) +
  geom_path(linewidth = 1) +
  geom_abline(linetype = "dashed") +
  scale_x_continuous(expand = c(0,0), limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2),
                     labels = seq(0, 1, by = 0.2)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1), 
                     breaks = seq(0, 1, by = 0.2),
                     labels = seq(0, 1, by = 0.2)) +
  labs(color = "") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "inside",
        legend.justification = c(1,0),
        legend.background = element_blank(),
        legend.key = element_blank(), 
        text = element_text(family = "serif"))
# 合并训练集和测试集上PR曲线
predtrain_logistic$prresult %>%
  bind_rows(predtest_logistic$prresult) %>%
  mutate(dataAUC = paste(data, curvelab),
         dataAUC = forcats::as_factor(dataAUC)) %>%
  ggplot(aes(x = recall,
             y = precision, 
             color = dataAUC)) +
  geom_path(linewidth = 1) +
  geom_abline(linetype = "dashed", slope = -1, intercept = 1) +
  scale_x_continuous(expand = c(0,0), limits = c(0, 1), 
                     breaks = seq(0, 1, by = 0.2),
                     labels = seq(0, 1, by = 0.2)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1), 
                     breaks = seq(0, 1, by = 0.2),
                     labels = seq(0, 1, by = 0.2)) +
  labs(color = "") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "inside",
        legend.justification = c(0,0),
        legend.background = element_blank(),
        legend.key = element_blank(), 
        text = element_text(family = "serif"))
# 合并训练集和测试集上校准曲线
predtrain_logistic$caliresult %>%
  bind_rows(predtest_logistic$caliresult) %>%
  mutate(data = forcats::as_factor(data)) %>%
  ggplot(aes(x = predprobgroup,
             y = Fraction, 
             color = data)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3, pch = 15) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous(expand = c(0,0), limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.1),
                     labels = seq(0, 1, by = 0.1)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.1),
                     labels = seq(0, 1, by = 0.1)) +
  labs(x = "Bin Midpoint", y = "Event Rate", color = "") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "inside",
        legend.justification = c(1,0),
        legend.background = element_blank(),
        legend.key = element_blank(), 
        text = element_text(family = "serif"))
# 合并训练集和测试集上性能指标
predtrain_logistic$metrics %>%
  bind_rows(predtest_logistic$metrics) %>%
  dplyr::select(-.estimator) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)

# 合并训练集和测试集性能指标并保存为CSV
combined_metrics <- predtrain_logistic$metrics %>%
  bind_rows(predtest_logistic$metrics) %>%
  dplyr::select(-.estimator) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)

# 保存为CSV文件
write.csv(combined_metrics, 
          file = "data15_logistic_regression_指标.csv", 
          row.names = FALSE,  # 不保存行号
          fileEncoding = "UTF-8")  # 确保中文正常保存
##################################################################
# 交叉验证
set.seed(42)
cv_logistic <- 
  wk_logistic %>%
  fit_resamples(
    folds,
    metrics = metricset_cls2,
    control = control_resamples(save_pred = T,
                                verbose = T,
                                event_level = "second",
                                parallel_over = "everything",
                                save_workflow = T)
  )
cv_logistic
# 交叉验证指标结果
evalcv_logistic <- list()
# 评估指标设定
metrictemp <- metric_set(yardstick::roc_auc, yardstick::pr_auc)
evalcv_logistic$evalcv <- 
  collect_predictions(cv_logistic) %>%
  group_by(id) %>%
  metrictemp(Futility, .pred_1, event_level = "second") %>%
  group_by(.metric) %>%
  mutate(model = "logistic",
         mean = mean(.estimate),
         sd = sd(.estimate)/sqrt(length(folds$splits)))
evalcv_logistic$evalcv
# 交叉验证预测结果图示
# ROC
evalcv_logistic$cvroc <- 
  collect_predictions(cv_logistic) %>%
  group_by(id) %>%
  roc_curve(Futility, .pred_1, event_level = "second") %>%
  ungroup() %>%
  left_join(evalcv_logistic$evalcv %>% filter(.metric == "roc_auc"), 
            by = "id") %>%
  mutate(idAUC = paste(id, " ROCAUC:", round(.estimate, 4)),
         idAUC = forcats::as_factor(idAUC)) %>%
  ggplot(aes(x = 1-specificity, y = sensitivity, color = idAUC)) +
  geom_path(linewidth = 1) +
  geom_abline(linetype = "dashed") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.2),
                       labels = c(0, seq(0.2, 0.8, by = 0.2), 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.2),
                       labels = c(0, seq(0.2, 0.8, by = 0.2), 1)) +
    labs(color = "") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = c(1,0),
          legend.justification = c(1,0),
          legend.background = element_blank(),
          legend.key = element_blank(), 
          text = element_text(family = "serif"))
evalcv_logistic$cvroc
# PR
evalcv_logistic$cvpr <- 
  collect_predictions(cv_logistic) %>%
  group_by(id) %>%
  pr_curve(Futility, .pred_1, event_level = "second") %>%
  ungroup() %>%
  left_join(evalcv_logistic$evalcv %>% filter(.metric == "pr_auc"), 
            by = "id") %>%
  mutate(idAUC = paste(id, " PRAUC:", round(.estimate, 4)),
         idAUC = forcats::as_factor(idAUC)) %>%
  ggplot(aes(x = recall, y = precision, color = idAUC)) +
  geom_path(linewidth = 1) +
  geom_abline(linetype = "dashed", intercept = 1, slope = -1) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.2),
                       labels = c(0, seq(0.2, 0.8, by = 0.2), 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.2),
                       labels = c(0, seq(0.2, 0.8, by = 0.2), 1)) +
    labs(color = "") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = c(0,0),
          legend.justification = c(0,0),
          legend.background = element_blank(),
          legend.key = element_blank(), 
          text = element_text(family = "serif"))
evalcv_logistic$cvpr

###################################################################
# 自变量数据集
colnames(traindata)
traindatax <- traindata %>%
  dplyr::select(-Futility)
colnames(traindatax)
# 分类型、连续型自变量名称
catvars <- getcategory(traindatax)
convars <- getcontinuous(traindatax)
# 提取最终的算法模型
final_logistic2 <- final_logistic %>%
  extract_fit_engine()
final_logistic2
######################## DALEX解释对象
explainer_logistic <- DALEXtra::explain_tidymodels(
  final_logistic, 
  data = traindatax,
  y = ifelse(traindata$Futility == yourpositivelevel, 1, 0),
  type = "classification",
  label = "Logistic"
)
# 变量重要性
vip_logistic <- viplot(explainer_logistic, showN = 9)
vipdata_logistic <- vip_logistic$data
vip_logistic$plot
# 变量偏依赖图
pdplot(explainer_logistic, convars)#5_logistics模型的变量偏依赖图DALEX包55
pdplot(explainer_logistic, "percent_delta_CA199")
pdplot(explainer_logistic, catvars)
pdplot(explainer_logistic, "ASA")
###################################### iml解释对象
conflicted::conflicts_prefer(caret::cluster)  # 声明优先级
conflicted::conflicts_prefer(pROC::coords)
# 声明使用caret包的specificity和sensitivity函数
conflicts_prefer(caret::specificity)
conflicts_prefer(caret::sensitivity)
conflicted::conflicts_prefer(reshape::smiths)
conflicted::conflicts_prefer(Metrics::accuracy)
conflicted::conflicts_prefer(DALEX::explain)
conflicted::conflicts_prefer(reshape2::melt)
conflicted::conflicts_prefer(munsell::complement)
conflicted::conflicts_prefer(timeDate::align)
conflicted::conflicts_prefer(cvms::validate)
conflicted::conflicts_prefer(htmltools::p)
conflicted::conflicts_prefer(shiny::runExample)

predictor_logistic <- iml::Predictor$new(
  final_logistic, 
  data = traindatax,
  y = traindata$Futility,
  predict.function = function(model, newdata){
    predict(model, newdata, type = "prob") %>%
      rename_with(~gsub(".pred_", "", .x))
  },
  type = "prob"
)
# 交互作用
interact_logistic <- iml::Interaction$new(predictor_logistic)
plot(interact_logistic) +
  theme_minimal()
interact_logistic_1vo <- 
  iml::Interaction$new(predictor_logistic, feature = "post_Diameter")
plot(interact_logistic_1vo) +
  theme_minimal()
interact_logistic_1v1 <- iml::FeatureEffect$new(
  predictor_logistic, 
  feature = c("post_Diameter", "post_DBIL"),
  method = "pdp"
)
plot(interact_logistic_1v1) +
  scale_fill_viridis_c() +
  labs(fill = "") +
  theme_minimal() 

###################################### lime单样本预测分解
explainer_logistic <- lime::lime(
  traindatax,
  lime::as_classifier(final_logistic, c(yournegativelevel, yourpositivelevel))
)
explanation_logistic <- lime::explain(
  traindatax[1,],  # 训练集第1个样本
  explainer_logistic, 
  n_labels = 2, 
  n_features = ncol(traindatax)
)
lime::plot_features(explanation_logistic)
######################## fastshap包
shapresult_logistic <- shap4cls2(
  finalmodel = final_logistic,
  predfunc = function(model, newdata) {
    predict(model, newdata, type = "prob") %>%
      dplyr::select(ends_with(yourpositivelevel)) %>%
      pull()
  },
  datax = traindatax,
  datay = traindata$Futility,
  yname = "Futility",
  flname = catvars,
  lxname = convars,
  plotname = c(catvars, convars)
)
# shap变量重要性图
shapresult_logistic$shapvipplot#7变量重要性图shap包55
# shap蜜蜂图
shapresult_logistic$shapplot#7变量重要性蜜蜂图shap包55
# 单样本预测分解
shap41 <- shapviz::shapviz(
  shapresult_logistic$shapley,
  X = traindatax
)
shapviz::sv_force(shap41, row_id = 18)  +  # 训练集第1个样本
  theme(text = element_text(family = "serif"))
shapviz::sv_waterfall(shap41, row_id = 1)  +  # 训练集第1个样本
  theme(text = element_text(family = "serif"))#8单样本解释瀑布图55
shapviz::sv_importance(
  shap41, 
  kind = "beeswarm",
  bee_width = 0.1
) +
  theme_minimal()#8变量重要性beeswarm图55
# 所有分类变量的shap图示
shapresult_logistic$shapplotd_facet
shapresult_logistic$shapplotd_one
# 所有连续变量的shap图示
shapresult_logistic$shapplotc_facet#10ASA解释图3.5
shapresult_logistic$shapplotc_one#
shapresult_logistic$shapplotc_one2
# 单变量shap图示
sdplot(shapresult_logistic, "post_WBC", "Futility")#10WBC解释图3.5
sdplot(shapresult_logistic, "post_ALB", "Futility")#10ALB解释图3.5
sdplot(shapresult_logistic, "post_DBIL", "Futility")#10DBIL解释图3.5
sdplot(shapresult_logistic, "percent_delta_CA199", "Futility")#10CA199解释图3.5
sdplot(shapresult_logistic, "post_Diameter", "Futility")#10最大径解释图3.5

# 所有变量一张图
# shap变量重要性
shapresult_logistic$shapvipplot_unity
# shap依赖图
shapresult_logistic$shapplot_unity
# shap变量重要性条形图+shap蜜蜂图
shapplotplus(shapresult_logistic,
             plotname = c(catvars, convars),
             bottomxstart = -0.3,
             bottomxstop = 0.5,
             topxrange = 0.15)#11shap变量重要性条形图+shap蜜蜂图55


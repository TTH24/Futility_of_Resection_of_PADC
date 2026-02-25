# Prediction Tool for Surgical Failure Risk After Neoadjuvant Therapy in PDAC

This repository contains the R source code used in the study:

**Futility of Resection After Neoadjuvant Therapy in Pancreatic Ductal Adenocarcinoma**

Submitted to *iScience* (Manuscript ID: ISCIENCE-D-25-10749R4)

---

## 📌 Overview

This repository provides the complete R scripts used for:

- Data preprocessing
- Logistic regression model development
- Internal 5-fold cross-validation
- External validation
- SHAP analysis
- Figure generation
- Development of the Shiny web-based prediction tool

The web application is available at:
https://tth-general.shinyapps.io/Futility_of_Surgery/

---

## 🔒 Data Availability

The clinical dataset used in this study contains sensitive patient information and is not publicly available.

De-identified data may be made available from the corresponding author upon reasonable request and institutional approval.

Contact:
Professor Tian-Hong Teng  
Email: tianhongteng24@163.com

---

## 📂 Repository Structure

- `R/` – All R scripts used for analysis and model construction
- `data/` – Contains a description of data structure (no patient data included)
- `docs/` – Workflow diagrams or supplementary figures

---

## 🖥 Software Environment

- R version 4.4.1
- tidyverse
- tidymodels
- pROC
- fastshap
- DALEX
- ggplot2
- shiny

Full session information is provided in `sessionInfo.txt`.

---

## 📜 License

This code is released under the MIT License.

---

## 📖 Citation

If you use this code, please cite:

Luo HT, Xie ZQ, Chen SJ, et al. Futility of Resection After Neoadjuvant Therapy in PDAC. iScience (under review).

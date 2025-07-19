
# 👁️ Eye-Tracking Analysis of Fluency and Lexical Retrieval

This project investigates how fluency and speaker accent influence listeners’ visual attention toward low- and high-frequency referent objects during speech comprehension. Using three experiments and thousands of observations, we explored whether disfluencies prompt English native listeners to anticipate less frequent referents.

📅 **Report Date**: 2025  
👨‍💻 **Author**: Esdras Koome Micheni

![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)

---

## 🎯 Objectives

- Analyze how **fluency conditions** (fluent vs. disfluent) affect eye-tracking behavior.
- Evaluate listener responses to **low-frequency (LF)** and **high-frequency (HF)** referent objects.
- Investigate how **accent strength** in non-native speakers modulates referent prediction.
- Use **logistic regression** and **chi-square tests** to validate hypotheses.
- Examine if fluency predicts referent looks consistently across three experiments.

---

## 📊 Summary of Experiments

### ✅ Experiment 1 – Native Speaker, Fluent vs. Disfluent

- Total observations: 47,865
- Listeners significantly increased looks to **low-frequency objects** during **disfluency**.
- Hypothesis accepted: Disfluency from native speech biases listeners toward LF referents.

### ✅ Experiment 2 – Non-Native Speaker, Strong Foreign Accent

- Total observations: 58,372
- Looks to LF objects did **not significantly differ** from HF during disfluency.
- Hypothesis accepted: Disfluency from non-native speakers with strong accents does **not** guide listeners toward LF referents.

### ✅ Experiment 3 – Native-Like vs. Strong Foreign Accent

- Total observations: 54,552
- Listeners more likely looked to LF referents when disfluency occurred in **native-like accents**.
- Accent strength modulated effect of disfluency.
- Hypothesis confirmed: Sensitivity to accent affects prediction.

---

## 🔍 Analysis Techniques

- **Exploratory Data Analysis (EDA)**:
  - Distribution of trials, fluency conditions, and referent looks
- **Logistic Regression**:
  - Modeled the likelihood of looking at LF objects based on fluency and other predictors
- **Chi-Square Tests**:
  - Evaluated whether fluency significantly changed LF vs. HF referent looks
- **Time Variables**:
  - Linear and quadratic timing metrics used (with outlier correction via transformations)

---

## 📁 Project Structure
```
eye-tracking-fluency-analysis/
├── data/ # Raw and cleaned datasets
├── eda/ # Exploratory data visualizations
├── models/ # Logistic regression outputs
├── results/ # Chi-square test summaries
├── report/ # Analysis Report.pdf
├── LICENSE # MIT License
└── README.md # This file
```

---

## 📦 Requirements

This project was built using R. Required packages:

```r
install.packages(c("ggplot2", "dplyr", "tidyr", "broom", "readr", "car"))
```
## 📬 Connect With Me

- 📧 Email: [esdraskoome@gmail.com](mailto:esdraskoome@gmail.com)  
- 💼 [LinkedIn](https://www.linkedin.com/in/esdras-koome-micheni-106651338/)  
- 🧑‍💻 [Upwork Profile](https://www.upwork.com/freelancers/~01bbdaff1dc6ce0241)  
- ▶ [YouTube Channel](https://www.youtube.com/channel/UCBhBTBAanuBNiQs3r7mwDmA)


# EDA - Customer Churn in Telcom
Before data modeling, I followed the extensive step-by-step guide from [A Comprehensive Guide to Data Exploration](https://www.analyticsvidhya.com/blog/2016/01/guide-data-exploration/#one) and performed EDA in R using Kaggle dataset - [Telco Customer Churn](https://www.kaggle.com/blastchar/telco-customer-churn).
## Introduction
By conducting exploratory data analysis (EDA), we can gain insight into a dataset, examine variables, detect anomalies, and prepare data before applying a predictive model. Thus, EDA could help to increase the accuracy of the models since good input quality will lead to good output quality. With that being said, this post aims to carefully walk through and understand each step in EDA process.
## Steps 
1. Variable Identification 
   - Predictor and Target
   - Data type
   - Variable category
2. Univariate Analysis 
   - Numerical
   - Categorical
3. Bi-variate Analysis 
   - Categorical & Categorical
   - Categorical & Numerical
   - Numerical & Numerical
4. Missing Value Treatment 
5. Outlier Treatment 
6. Feature Engineering
   - Variable transformation
   - Variable/Feature creation
## Conclusion
To summarize, so far, I have fixed data types, treated missing values, cleaned categories, and built some graphs to gain insights. From the univariate analysis, I revealed the overall condition in the dataset, and from the bi-variate analysis, I got some clues of which variable (tenure, MonthlyCharges, and SeniorCitizen) might have a significant impact on customer churn. Thus, after descriptive analysis, next, I can start performing predictive analysis! 

See more details here: [Predicting Customer Churn in Telecom](https://github.com/yuki04160/Predicting-Customer-Churn-in-Telecom).

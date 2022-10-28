# mitral
"Myocardial Infarction and Mitral Regurgitation: Predicting Severity and Survivability" project files. 
</br>
Overall goal of the project is to perform survival analysis and create a classifier out of noisy data and weak predictors. The data are highly imbalanced (about 93%/7% split) and have a high degree of missing information. Read the [report here.](https://github.com/npeckover/mitral/blob/main/mitral.pdf)
</br>
</br>
Data used is an edited version (removed some excel formatting gunk and irrelevant sheets) of the data available here: 
</br>
</br>
[Sharma, Harish (2021), “Mitral Regurgitation Following Acute Myocardial Infarction Treated by Percutaneous Coronary Intervention – Prevalence, Risk factors and Predictors of Outcome”, Mendeley Data, V1, doi: 10.17632/3yb74rkp4d.1](https://data.mendeley.com/datasets/3yb74rkp4d/1)
</br>
</br>
MR_wrangling.R cleans and formats the original dataset using [mitral_regurg_touse.csv.](https://github.com/npeckover/mitral/blob/main/mitral_regurg_touse.csv)
</br>
</br>
MR_EDA.R is an EDA file that creates some interesting plots and some basic summaries
</br>
</br>
MR_survival_analysis.R is as it sounds, using the 'survival' package for quick analysis and a couple of plots.
</br>
</br>
MR_modeling.R attempts to create a classification model. This is an edited version that trims down the number of useless models to three: a logistic regression model, a regularized logistic regression model, and a random forest model. The final model utilizes XGBoost.
</br>
</br>
![Heat map of mitral regurgitation severity by coronary blockage location and severity](https://github.com/npeckover/MR.EDA/blob/main/MR_heat.bmp)

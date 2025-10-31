# **Project: Symptoms of depression and anxiety are early predictors of multi-domain disability progression in progressive MS**

**Project background**

In multiple sclerosis (MS), depression and anxiety are common, yet their relation to disease progression is unclear. We investigated whether Hospital Anxiety and Depression Scale (HADS) symptoms predict neurological disability progression.

**Project methods**

In our prospective cohort of people with primary progressive MS (PPMS), depressive and anxiety symptoms were assessed with the HADS. Disability progression at 1 and 2 years was defined by Expanded Disability Status Scale (EDSS), a 3-variable composite endpoint (EDSS, Timed 25-Foot Walk; T25FW, or Arm Function in Multiple Sclerosis Questionnaire; AMSQ), or a 5-variable composite endpoint (EDSS, T25FW, AMSQ, Symbol Digit Modalities Test (SDMT), or Patient-Determined Disease Steps (PDDS)). Logistic regression modelled whether HADS scores predicted disability progression. Boruta feature selection identified the most informative HADS items.

---

## **1. hads_statistic_visualization.R**

This script focuses on the **statistic analysis and visualization of input datasets**

1. baseline characterizing of the datasets and baseline statistics
2. negative binomial model for statistic differences in HADS score between progression and non progression
3. logistic regression model using hads score to predict disease progression
4. boxplots and Venn visualizations for the final manuscript

---

## **2. hads_CV_Boruta_analysis.Rmd**

This script focuses on the **prediction with logistic regression and Boruta feature selection**

1. bootstrap of cross-validation process of logistic regression, use HADS scores to predict disease progression
2. Boruta feature selection to find the most influental question from HADS questionnaire
3. bootstrap of cross-validation process of logistic model, but using only the most influental questions from Boruta selection to predict disease progression


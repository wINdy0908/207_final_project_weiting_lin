# Effect_of_Stimulus_on_Mouse
This project aimed to find the neuronal spike activities of two mice across five sessions and four levels of contrast on both the right and left sides.

## Exploratory Data Analysis and data preprocessing
* Observed different trial numbers and neuronal spikes in five sessions, and used two table to present the difference.
* Utilized average firing rate in session 5 and plot the distribution.
* Plotted main effect and interaction effect from left contrast levels and right contrast levels across five sessions.
  

## Data modeling
* Built mixed-effect models, including a two-way ANOVA with both a reduced model and a full model,
* Selected the full model, which takes into account the interaction effect, based on an F-test.

## Data prediction 
* Employed Logistic Regression models that considered two-sided contrast levels, intersection of two-sided contrast levels and the average neuronal spike activities of a trial to predict if the mouse made the correct choice.
* Used Likelihood Ratio Test to determine the better Logistic Regression Models.
* Adopted confusion matrix to measure the accuracy.

## Sensitive analysis
* ANOVA model:
  * Utilized Pearson residuals and deviance residuals to determine if model has lack-of-fit issue
  * Checked Homogeneity of Variance in five sessions.
  * Checked normality of residuals
* GLM model:
  * Utilized Pearson residuals and deviance residuals on Logistic Regression model check if there has Symmetric Pattern in residuals.
  * Analysize influentail points
  
  
  
  //test



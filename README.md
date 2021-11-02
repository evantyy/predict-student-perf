# Predicting student performances with attributes

The data consisting of student's achievement in two Portuguese secondary schools includes grades, demographic, social and school related features. Collected using school reports and questionnaires, it includes two subjects: Mathematics (mat) and Portuguese language (por) which were modeled under binary/five-level classification and regression tasks. <p> Note: target attribute G3 has a strong correlation with attributes G2 and G1. <br /> This occurs because G3 is the final year grade, while G1 and G2 corresponds to 1st and 2nd period grades. It is more difficult to predict G3 without G2 and G1, but such prediction is much more useful.

Data: [Student](https://archive.ics.uci.edu/ml/datasets/student+performance) <p>
Relevant papers: [Data Mining Prediction](http://www3.dsi.uminho.pt/pcortez/student.pdf) <p>
Observations: [My Personal Report](https://evantyy.github.io/predict-student-perf/)

## Description

Utilised three machine learning algorithms to check how student's attributes affects performance.
- Linear Regression
- Ridge Regression
- Random Forest (~70% accuracy)

## Getting Started

### Packages used

* [dplyr](https://cran.r-project.org/web/packages/dplyr/dplyr.pdf) : data manipulation
* [tidyr](https://cran.r-project.org/web/packages/tidyr/tidyr.pdf) : tidy data
* [ggplot2](https://cran.r-project.org/package=ggplot2/ggplot2.pdf) : maps variables to plots & charts
* [caTools](https://cran.r-project.org/web/packages/caTools/caTools.pdf) : moving window statistics
* [caret](https://cran.r-project.org/web/packages/caret/caret.pdf) : train & plot classification and regression models
* [randomForest](https://cran.r-project.org/web/packages/randomForest/randomForest.pdf)
* [glmnet](https://cran.r-project.org/web/packages/glmnet/glmnet.pdf)

## Authors

Evangeline Tan
[@evantyy](https://github.com/evantyy)

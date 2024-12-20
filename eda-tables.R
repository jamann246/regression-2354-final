# loading data
data <- read.csv("reg-final.csv") |> 
  dplyr::mutate(
    dplyr::across(c(depress, bp, health, male), factor), 
    health = factor(health, labels = c("Excellent","Good/Very Good","Fair","Poor")),
    male = factor(male, labels = c("Female","Male")),
    depress = factor(depress, labels = c("Does Not Have Depression","Has Depression")),
    bp = factor(bp, labels = c("Does Not Have High Blood Pressure","Has High Blood Pressure"))
  )


table1::label(data$age) <- "Age"
table1::label(data$male) <- "Sex"
table1::label(data$alcage) <- "Alcohol Age"
table1::label(data$cigage) <- "Cigarette Age"
table1::label(data$depress) <- "Depression Status"
table1::label(data$bp) <- "Blood Pressure"
table1::label(data$health) <- "Health Self-Rating"

# univariate analysis
table1::table1(
  ~age+alcage+cigage+male+depress+bp+health, 
  droplevels = TRUE, data = data)

# bivariate analysis
table1::table1(
  ~age+alcage+cigage+male+depress+bp | health, 
  droplevels = TRUE, data = data)

# anova tests for continuous predictors
summary(aov(age ~ health, data=data))
# p = 0.0049

summary(aov(alcage ~ health, data=data))
# p = 0.257, F=1.395

summary(aov(cigage ~ health, data=data))
# p = 0.81, F = 0.321

# chi squared tests for categorical predictors
sex_table <- table(data$health, data$male)
chisq.test(sex_table)
# p=0.5007

depression_table <- table(data$health, data$depress)
chisq.test(depression_table)
# p << 0.81

bp_table <- table(data$health, data$bp)
chisq.test(bp_table)
# p << 0.05
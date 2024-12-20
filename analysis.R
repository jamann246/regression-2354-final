
# loading data
data <- read.csv("reg-final.csv") |> 
  dplyr::mutate(
    dplyr::across(c(depress, bp), factor)
  )

# Part 1: fitting the model
fit <- MASS::polr(
  factor(health) ~ depress + alcage + cigage + age + bp,  
  method = "logistic", 
  data = data
)

# summary of the model
summary(fit)

# calculate and store p values
(ctable <- coef(summary(fit)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

# combined table
(ctable <- cbind(ctable, "p value" = p))

exp(coef(fit))

# Part 2: testing assumptions =======

# Test proportional odds assumption
brant::brant(fit)

# Test multicolliniarity 
car::vif(fit)

# Part 3: Predict probabilities ====
# Define the new scenarios
new_data <- data.frame(
  depress = factor(c(0, 1)),
  alcage = c(mean(data$alcage), mean(data$alcage)),  # Assume fixed value
  cigage = c(mean(data$cigage, na.rm=TRUE), mean(data$cigage, na.rm=TRUE)),  # Assume fixed value
  age = c(64, 64),
  bp = factor(c(1, 1))
)

predicted_probs <- predict(fit, new_data, type = "probs")
print(predicted_probs)

# Calculate the difference in probabilities between scenarios
prob_diff <- predicted_probs[2, ] - predicted_probs[1, ]
print(prob_diff)

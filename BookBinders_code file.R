##Assignment 4

###Part 1

# Clear memory
rm(list = ls()) # remove everything in your workspace 

# Bring in data
setwd("C:\\Users\\raksh\\OneDrive\\Desktop\\Sem 3\\Predictive Analysis\\Assignments\\Assignment 4")
df <- read_csv("BBB.csv")

# Transform "buyer" and "gender" into a 0/1 dummy variable in the "df" data frame
df$buyer_dummy <- ifelse(df$buyer == "yes", 1, 0)
df$gender_dummy <- ifelse(df$gender == "M", 1, 0)

# Fit a logistic regression model
log_model <- glm(buyer_dummy ~ last + total_ + gender_dummy + child + youth + 
               cook + do_it + reference + art + geog, data = df, family = binomial)

# Summarize the model
summary(log_model)

# Add predicted probability of 'purchase' as an additional column
df$purch_prob <- log_model %>% predict(df, type = "response")
head(df$purch_prob)

# From the data of the company, consisting of 550,000 customers,
# Predictive algorithm helped us to search and identify 50,000 customers,
# That were likely to purchase specialty books
# Here we assign a score to the customers based on their likelyhood
# of making the next purchase

# Converting to Odds Ratio
exp(coef(log_model))
# Odds ratios and 95% Confidence Interval
exp(cbind(OR = coef(log_model), confint.default(log_model)))
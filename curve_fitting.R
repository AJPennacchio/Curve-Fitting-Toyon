library(tidyverse)
library(broom)
library(tidyr)

#data pre-processing

#read in data
df_long <- read.csv('data.csv')
#transform into long format
df_long <- gather(df_long, condition, Fv_Fm, c(Irr.sha, Irr.sun, Un.sha, Un.sun), factor_key=TRUE)
#rename column
names(df_long)[names(df_long) == "Leaf #"] <- "Leaf"
#remove observations without Fv_Fm reading
df_long <- df_long[!grepl('-', df_long$Fv_Fm), ]
df_long$Fv_Fm <- as.numeric(df_long$Fv_Fm)
#Remove IrrSha plant 5 (insufficient data to curve fit)
df_long <- df_long %>%
  filter(!(condition == "Irr.sha" & Leaf.. == 5))

#df to hold T50 values for various curves
coefficients_df <- data.frame(Condition = character(0), T50 = double(0))


#visualize data pre curve-fitting
ggplot(df_long, aes(x=Temperature, y=Fv_Fm)) +
  geom_point(aes(color=condition)) 

#loop through conditions 
for(condition in unique(df_long$condition)){

#create sub dataframe of each condition
condition_df <- df_long[df_long$condition == condition, ]

#break apart sub dataframe by plant and curve fit
condition_model <- condition_df %>%
  split(.$Leaf..) %>%
  map(~ {
    data <- .
    fit <- nls(Fv_Fm ~ SSlogis(Temperature, Asym, xmid, scal), data = data, control = nls.control(maxiter = 100))
    return(fit)
  })

#temperature (X) values to be plotted for best fit line
x_seq <- seq(25, 61, by = 1)


#regression line plots for each plant/condition
condition_plots <- condition_model %>%
  map(~ {
    data <- augment(., newdata = data.frame(Temperature = x_seq))  # Generate data for the fitted line
    ggplot(data = data, aes(x = Temperature, y = .fitted, color = Leaf..)) +
      geom_line(color = "black") +
      geom_point(data = condition_df, aes(x = Temperature, y = Fv_Fm)) +
      labs(x = "Temperature", y = "Fv_Fm", title = condition)
  })

# View the plots
walk(condition_plots, print)

#extract T50 value from each model and put into coefficients_df
for(model in names(condition_model)){
  curr <- condition_model[[model]]
  xmid <- coef(curr)
  new_row <- data.frame(Condition = condition, T50 = xmid["xmid"])
  coefficients_df = rbind(coefficients_df, new_row)
  rm(new_row, curr)
}

}

#clear up space
rownames(coefficients_df) <- NULL
rm(condition_df, condition_model, condition_plots, model)

#run anova on coefficients_df
model <- aov(T50 ~ Condition, data = coefficients_df)
summary(model)

#box plot of each condition
ggplot(coefficients_df, aes(x=Condition, y=T50)) + 
  geom_boxplot(fill='lightblue') + 
  labs(title = "T50 scores by Condition")


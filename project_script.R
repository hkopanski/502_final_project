library(tidyverse)
library(cowplot) #Use required packages

df_data <- read_csv("framingham_data.csv") # Read in data

df_data$index <- seq(nrow(df_data)) # Add an index column

# Split data into smoker and nonsmoker
df_smoker <- df_data %>% filter(currentSmoker == 1)
df_nonsmoker <- df_data %>% filter(currentSmoker == 0)

#Plots 

options(repr.plot.width = 6, repr.plot.height = 4, repr.plot.res = 150)

plot_colors <- c("#001427","#708d81","#f4d58d","#bf0603","#8d0801")
y_limits <- c(0, 0.0225)

total_data <- ggplot(df_data) + geom_density(aes(sysBP), 
                                             fill = plot_colors[1],
                                             alpha = 0.6) +
  ylim(y_limits) + ylab("Density") + xlab("")

sep_data <- ggplot() + geom_density(data = df_smoker, aes(sysBP), 
                                    fill = plot_colors[3], alpha = 0.6) + 
  geom_density(data = df_nonsmoker, aes(sysBP), 
               fill = plot_colors[5], alpha = 0.6) +
  ylim(y_limits) + ylab("") + xlab("Systolic Blood Pressure")

plot_3 <- ggplot() + geom_density(data = df_smoker, aes(sysBP), 
                                  fill = plot_colors[3], alpha = 0.5) + 
  geom_density(data = df_data, aes(sysBP), 
               fill = plot_colors[1], alpha = 0.5) +
  geom_density(data = df_nonsmoker, aes(sysBP), 
               fill = plot_colors[5], alpha = 0.5) +
  ylim(y_limits) + ylab("") + xlab("")

plot_grid(total_data, sep_data, plot_3, align = 'vh', 
          hjust = -1,nrow = 1, ncol = 3)

#Create a sample variance function to ensure proper calculation
sample_variance <- function(x, sample = TRUE){
  if (sample == TRUE){
    sum((x - mean(x))^2) / (length(x) - 1)
  } else if(sample == FALSE) {
    sum((x - mean(x))^2) / (length(x))
  }
}
#Create pooled sample variance function 
f_pooled_variance <- function(x, y){
  ((length(x) - 1) * sample_variance(x) + 
     (length(y) - 1) * sample_variance(y)) / 
    (length(x) + length(y) - 2)
}

# Two Sample T-test - Pooled Sample Variance - P-value

alpha <- 0.05

mu_smoker <- mean(df_smoker$sysBP)
var_smoker <- sample_variance(df_smoker$sysBP)
n_smoker <- length(df_smoker$sysBP)

mu_nonsmoker <- mean(df_nonsmoker$sysBP)
var_nonsmoker <- sample_variance(df_nonsmoker$sysBP)
n_nonsmoker <- length(df_nonsmoker$sysBP)

dof_1 <- (n_smoker + n_nonsmoker - 2)

p_sample_var <- f_pooled_variance(df_smoker$sysBP, 
                                  df_nonsmoker$sysBP)


t_obs <- (mu_smoker - mu_nonsmoker) / (sqrt(p_sample_var) * 
                                         sqrt(1/n_smoker + 1/n_nonsmoker))

t_stat <- qt(alpha / 2, dof_1)

p_value_obs <- dt(t_obs, dof_1)

# We need the Satterthwaite approximation


rmarkdown::render("a502_Project.Rmd", output_format = "all")
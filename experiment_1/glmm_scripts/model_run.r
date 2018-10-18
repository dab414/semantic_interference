library(tidyverse)
library(data.table)
library(lme4)

current_data <- fread('exp1_clean.csv', stringsAsFactors = TRUE)

current_data <- current_data %>% 
  mutate(block = recode(block, '1' = 'one', '2' = 'two'),
         cycle = factor(recode(cycle, 
                        '1' = 'one',
                        '2' = 'two',
                        '3' = 'three',
                        '4' = 'four',
                        '5' = 'five')))
  
contrasts(current_data$block)  <- c(-.5,.5)
contrasts(current_data$context)  <- c(-.5,.5)
contrasts(current_data$item_type)  <- c(-.5,.5)

start <- Sys.time()
m1 <- lmer(rt ~ cycle * item_type * context * block + 
	(1 + cycle + item_type + context + block | subject) + 
	(1  + cycle + item_type + context + block | item), data = current_data)
end <- Sys.time()

sink(file = 'm1_report.txt')
print('A model with all fixed effects and only main effects of slopes by both subjects and items')
summary(m1)
paste('This model converged in: ', end-start)
sink(NULL)


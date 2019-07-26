# Clear Environment
rm(list=ls())

# Load Libraries
library(corrplot)
library(data.table)
library(ggplot2)
library(magrittr)


# Set working path
fpath <- 'H:/R/reddit_regression_battleship'

# Import data
dat <- data.table(readRDS(file.path(fpath,'data/raw_data.rds')))

# Clean data
clean.dat <- dat %>%
    .[, x17 := NULL] %>% # remove x17: it's a constant
    .[, x18 := NULL] %>% # remove x18: it's identical to x15
    .[, x14_2 := x14**2]

# Denisty plot of y
ggplot(clean.dat, aes(x = y))+
    geom_density()+
    theme_bw()

# Is there a difference between the 2 factors fo x8 with respect to y?
ggplot(clean.dat, aes(x = y, col = factor(x8)))+
    geom_density()+
    theme_bw()

# Is there a difference between the 5 factors fo x9 with respect to y?
ggplot(clean.dat, aes(x = y, col = factor(x9)))+
    geom_density()+
    theme_bw()

# Scatter plots split by x8 & x9
ggplot(clean.dat, aes(x = x14, y = y, col = x8))+
    geom_point()+
    theme_bw()
ggplot(clean.dat, aes(x = x14, y = y, col = x9))+
    geom_point()+
    theme_bw()

# Scatter plot of all remaining fields
Xs <- gsub(',', '\\+', toString(names(clean.dat)))
pairs(as.formula(paste("y~ ", Xs)), data= clean.dat, col = 'steelblue')

# Correlation plot of all remaining (numeric) fields
str(clean.dat)
corrplot(cor(clean.dat[ ,-c(9,10)]), method = "color", addCoef.col = "white", number.digits = 2)



### MODEL
mod <- lm(y ~ I(x14,2), data = clean.dat)
(summ.mod <- summary(mod)); r2 <- round(summ.mod$r.squared,3)
predictions <- data.frame(x = 0:60, y = predict(mod, newdata = list(x14 = 0:60, x14_2 = (0:60)**2)))


ggplot(clean.dat, aes(x = x14, y =y, col = x9))+
    geom_point()+scale_color_viridis_d(option = "D")+
    geom_line(data=predictions, aes(x = x, y = y), col = 'red')+
    annotate("text", x = 15, y = -10000, 
              label = paste('formula: y = x9 + x14 + x14^2 -1 \n R^2:', r2))+
    # theme_bw()+
    labs(title = 'Regression Battleship Plot')
ggsave(file.path(fpath, 'reg_plot.png'))


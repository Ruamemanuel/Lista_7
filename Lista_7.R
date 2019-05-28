if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(dplyr) == F) install.packages('dplyr'); require(tidyverse)
if(require(ggiraphExtra) == F) install.packages('ggiraphExtra'); require(ggiraphExtra)
if(require(modelr) == F) install.packages('modelr'); require(modelr)
if(require(htest) == F) install.packages('htest'); require(htest)

options(scipen = 999)

# Letra A ----

setwd("C:/GitHub/Lista_7/Lista_7/Dados")
library(haven)
fair <- read_dta("fair.dta")

summary(fair)

table(fair)

# Letra B ----

reg1 <- lm(VOTE~GROWTH, data = fair)
summary(reg1)

ggplot(fair, aes(VOTE, GROWTH)) + 
  labs(x = "Voto", y ="Crescimento", title = "Grafico de dispersao") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

confint(reg1)

rmse(reg1, fair)

# Letra c ----

reg2 <- lm(VOTE~GROWTH + GOODNEWS, data = fair)
summary(reg2)

equation1=function(x){coef(reg2)[2]*x+coef(reg2)[1]}
equation2=function(x){coef(reg2)[2]*x+coef(reg2)[1]+coef(reg2)[3]}

ggplot(fair,aes(y = VOTE, x = GROWTH, color = GOODNEWS)) + 
  geom_point() +
  stat_function(fun=equation1,geom="line",
                color=scales::hue_pal()(2)[1]) +
  stat_function(fun=equation2,geom="line",
                color=scales::hue_pal()(2)[2])

plot(reg2)

confint(reg2)

rmse(reg2, fair)

mean(residuals(reg2))

summary(fair$GROWTH)

# Letra d ----

reg3 <- lm(VOTE~GROWTH + WAR, data = fair)
summary(reg3)

ggPredict(reg3)

rmse(reg3, fair)

confint(reg3)

plot(reg3)

mean(residuals(reg3))

library(readxl)
df <- read_excel("pwt.xlsx")
manu <- read_excel("manufacturing percent.xlsx")

attach(df)
library(dplyr)
library(data.table)
library(car)
library(ggplot2)
library(lmtest)
library(plm)

## consider BRICs - the association of five major emerging national economies: Brazil, 
# Russia, India, China and South Africa, as well as our benchmark country the United States.
# Since they are emerging markets, we could consider the year starting from 1990.
df_bric <- df %>%
  filter(countrycode %in% c("BRA","IND","ZAF","CHN","RUS","USA"))
df_bric <- df_bric %>%
  filter(year >= 1990)
manu <- manu %>%
  filter(year >= 1990)
df_bric$time <- df_bric$year-1989

# Plotting
ggplot(df_bric, aes(x = year, y = ctfp, colour = country)) + geom_line() + geom_point()
ggplot(manu, aes(x = year, y = Manufacturing , colour = countrycode)) + geom_line() + geom_point()
boxplot(ctfp ~ countrycode, data = df_bric)
ggplot(df_bric, aes(x = year, y = rtfpna, colour = country)) + geom_line() + geom_point()

# Data analysis
# TFP ~ human capital + openess
df_bric$open <- df_bric$csh_m + df_bric$csh_x
df_bric <- df_bric %>% 
  group_by(countrycode) %>%
  mutate(plc_1 = dplyr::lag(pl_c, n = 1, default = NA),
         hc_1 = dplyr::lag(hc, n = 1, default = NA))
df_bric$inflation <- df_bric$pl_c/df_bric$plc_1

pdf <- pdata.frame(df_bric, index=c("countrycode","year"))
pdf$inflation <- pdf$pl_c/pdf$plc_1
pdf$open <- pdf$csh_m + pdf$csh_x


fixed1 <-  plm(rtfpna ~ hc + inflation + open + csh_g + csh_i + labsh + time
               , data=pdf, model="within")
summary(fixed1)

fixed1 <-  plm(rtfpna ~ hc_1 + inflation + open + csh_g + csh_i + labsh + time
               , data=pdf, model="within")
summary(fixed1)

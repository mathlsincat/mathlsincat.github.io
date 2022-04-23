library(infer)
library(tidyverse)
gss
prop_test(gss,
          college ~ NULL,
          success = "degree",
          z = FALSE,
          conf_int = TRUE,
          conf_level = 0.95, correct = FALSE)

table(gss$college)

prop.test(table(gss$college)[2], nrow(gss), conf.level = 0.95, correct = FALSE)

Z_hat <- gss %>% 
  specify(formula = college ~ NULL, success = "degree") %>%
  calculate(stat = "z")

x = gss%>%
  specify(formula = college ~ NULL, success = "degree")%>%
  hypothesise(p = 0.2, null = "point")%>%
  generate(reps = 1000, type = "draw") %>%
  calculate(stat = "z")

x %>%
  get_p_value(obs_stat = Z_hat, direction = "greater")

gss%>%
  specify(formula = college ~ NULL, success = "degree")%>%
  hypothesise(p = 0.2, null = "point")%>%
  calculate(stat = "z")

gss%>%
  specify(formula = college ~ NULL, success = "degree")%>%
  hypothesise(p = 0.2, null = "point")%>%
  get_p_value(direction = "greater")

null_dist <- gss %>%
  specify(age ~ partyid) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F")

prop_test()
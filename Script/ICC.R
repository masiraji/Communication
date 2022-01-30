data = foreign::read.spss("Raw/Test retest of communication scale.sav", to.data.frame=TRUE)


code <- c(    "never"=1,
              "rarely"=2,
              "sometimes"=3,
              "often"=4,
              "always" =5)


test.retest.1 <- mutate(data, across(starts_with("TCS"), ~unname(code[.])))
test.retest.2 <- mutate(test.retest.1, across(starts_with("RCS"), ~unname(code[.])))


test.retest <- test.retest.2 %>% 
  rowwise() %>% 
  mutate(test_total=sum(across(starts_with("TCS")), na.rm = T))



test.retest <- test.retest %>% 
  rowwise() %>% 
  mutate(retest_total=sum(across(starts_with("RCS")), na.rm = T))



icc <- as.data.frame(cbind(test.retest$test_total, test.retest$retest_total))


psych::ICC(icc)



library(irr)

icc(icc, model = "twoway",
    type =  "agreement", unit = "single", conf.level = 0.95)
icc(icc, model = "twoway",
    type =  "agreement", unit = "average", conf.level = 0.95)


icc(icc, model = "oneway",
    type =  "agreement", unit = "single", conf.level = 0.95)
icc(icc, model = "oneway",
    type =  "consistency", unit = "average", conf.level = 0.95)



icc(icc, model = c("oneway", "twoway"),
    type = c("consistency", "agreement"), unit = c("single", "average"),conf.level = 0.95)
icc(icc, model = c("oneway", "twoway"),
    type = c("consistency", "agreement"), unit = c("single", "average"),conf.level = 0.95)
icc(icc, model = c("oneway", "twoway"),
    type = c("consistency", "agreement"), unit = c("single", "average"),conf.level = 0.95)
icc(icc, model = c("oneway", "twoway"),
    type = c("consistency", "agreement"), unit = c("single", "average"),conf.level = 0.95)
icc(icc, model = c("oneway", "twoway"),
    type = c("consistency", "agreement"), unit = c("single", "average"),conf.level = 0.95)

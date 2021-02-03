#Lebanon
#create a freq table for CU data
cuce_freq <- LEBc_ce_df %>% count(YearCEAdd) %>% rename(CUCE = n, Year = YearCEAdd) %>% transform(Year = as.character(Year))
#check data types
sapply(cuce_freq, class)
#print table
cuce_freq

#create a freq table for NCEd data
nced_freq <- LEBn_ce_df %>% count(year_est) %>% rename(NCED = n, Year = year_est) %>% transform(Year = as.character(Year))
#check data types
sapply(nced_freq, class)
#print table
nced_freq

#Merge the CU and NCED Data sets
LEB_Count_freq <- merge(cuce_freq,nced_freq, by = "Year")
#check data types
sapply(LEB_Count_freq, class)
#Chi-Square Test
chisq.test(LEB_Count_freq$CUCE, LEB_Count_freq$NCED, simulate.p.value = TRUE)

#Poisson Model: regression coefficient would be interpretable as a correlation coefficient
#https://www.researchgate.net/post/How_do_I_assess_the_association_between_two_random_count_variables_that_have_excessive_zeros
summary(LEB_Poisssonglm(formula = LEB_Count_freq$CUCE ~ LEB_Count_freq$NCED, family = "poisson"))

#------------------------------------------------------------------------------------------------------------------------------#

#York
#create a freq table for CU data
cuce_freq <- YRKc_ce_df %>% count(YearCEAdd) %>% rename(CUCE = n, Year = YearCEAdd) %>% transform(Year = as.character(Year))
#check data types
sapply(cuce_freq, class)
#print table
cuce_freq

#create a freq table for NCEd data
nced_freq <- YRKn_ce_df %>% count(year_est) %>% rename(NCED = n, Year = year_est) %>% transform(Year = as.character(Year))
#check data types
sapply(nced_freq, class)
#print table
nced_freq

#Merge the CU and NCED Data sets
YRK_Count_freq <- merge(cuce_freq,nced_freq, by = "Year")
#check data types
sapply(YRK_Count_freq, class)
#Chi-Square Test
chisq.test(YRK_Count_freq$CUCE, YRK_Count_freq$NCED)

#Poisson Model: regression coefficient would be interpretable as a correlation coefficient
#https://www.researchgate.net/post/How_do_I_assess_the_association_between_two_random_count_variables_that_have_excessive_zeros
summary(YRK_Poissson <- glm(formula = YRK_Count_freq$CUCE ~ YRK_Count_freq$NCED, family = "poisson"))
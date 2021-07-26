Predictive Analytics ANZ
================
Krutarth Patel
21/07/2021

## R Markdown

``` r
salary <- data %>% filter(txn_description == 'PAY/SALARY') %>% group_by(customer_id) %>% summarise(salary_91days=sum(amount))
salary %>% mutate(annual_salary = (salary_91days/91)*365) -> salary
hist(salary$annual_salary[!is.na(salary$annual_salary)],breaks=c(seq(27000,150000,by = 10000)),
 main = "Histogram for annual salary of customers", xlab= 'Income($)AUD')
```

![](Anz_files/figure-gfm/salary-1.png)<!-- -->

***Comments on Code chunk:*** *In the first line, I have created the
data frame called* `salary` *which has two columns of*
`Unique customer ID` *and the* `salary` *(filtered from the
txn\_description = pay/salary) also salary is summed together along with
each unique customer Id hence the data frame’s shape is* `(100,2)`. *In
the second line, I have created the third column in salary data frame
called `annual`.The new column has the annual salary scaled on the basis
of 91 days salary data provided in the original data set.*

***Comments on Statistical inference:*** *First of all we are assuming
that the consistency of the scaled annual salary is constant and there
is no major difference in actual annual salary of each customer for
instance, it could be changed in the scenario if any customer finds a
good role which pays more than previous role than we have a problem in
the annual scaled salary hence we are assuming the salary is constant
annualy which may have significant effect in the further inference.*

***Upon looking at the histogram we can see there exist some degree of
skewness which is not significant but still have to consider it while
predicting annual salary.***

``` r
join <- merge(data, salary, by.x = "customer_id", by.y = "customer_id",all.x= TRUE, all.y = FALSE)
transactions <-  data %>% filter(movement == 'debit') %>% group_by(customer_id) %>% summarise(debit_count_in91days=n(),spendings=sum(amount))
data2 <- merge(salary,transactions,by.x = "customer_id", by.y = "customer_id",all.x= TRUE, all.y = FALSE)
transaction <- data %>% group_by(customer_id) %>% summarise(balance=mean(balance),age=unique(age),gender=unique(gender))
final <- merge(data2,transaction,by.x = "customer_id",by.y = "customer_id",all.x = TRUE,all.y = FALSE)
M <- cor(final[3:7])
corrplot(M, method="color")
```

![](Anz_files/figure-gfm/segmentation-1.png)<!-- -->

``` r
p1 <- ggplot(join) +
  aes(x = age, y = annual_salary) +
  geom_point(
    size = 2.0,
    colour = "#EF562D"
  ) +
  geom_smooth(method='lm', formula= y~x) +
  theme_classic() 

p2 <- ggplot(final) +
  aes(x = spendings, y = annual_salary, colour = gender) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_manual(
    values = list(
      F = "#F8766D",
      M = "#FF61C3"
    )
  ) +
  theme_classic()+
  geom_smooth(method='lm', formula= y~x,se=F)


p3 <- ggplot(final) +
  aes(x = balance, y = annual_salary, colour = gender) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_manual(
    values = list(
      F = "#F8766D",
      M = "#FF61C3"
    )
  ) +
  theme_classic()+
  geom_smooth(method='lm', formula= y~x,se=F)

p4 <- ggplot(final) +
  aes(x = debit_count_in91days, y = annual_salary, colour = gender) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_manual(
    values = list(
      F = "#F8766D",
      M = "#FF61C3"
    )
  ) +
  theme_classic()+
  geom_smooth(method='lm', formula= y~x,se=F)

p1/p2 
```

![](Anz_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
p3/p4
```

![](Anz_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

***From the visualizations above and correlation map, we can see the
presence of some linear relationships between the variables hence we can
try fit a simple linear model for the prediction of annual salaries of
customers***

``` r
model1.lm <- lm(annual_salary ~ .-customer_id -gender-salary_91days, final)
summary(model1.lm)
```

    ## 
    ## Call:
    ## lm(formula = annual_salary ~ . - customer_id - gender - salary_91days, 
    ##     data = final)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -54002 -16272  -4144  13015  71892 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           4.845e+04  9.619e+03   5.036 2.25e-06 ***
    ## debit_count_in91days -8.282e+01  4.519e+01  -1.833    0.070 .  
    ## spendings             5.304e+00  1.257e+00   4.218 5.63e-05 ***
    ## balance               1.292e-01  7.278e-02   1.775    0.079 .  
    ## age                  -1.684e+02  2.232e+02  -0.755    0.452    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 24640 on 95 degrees of freedom
    ## Multiple R-squared:  0.2034, Adjusted R-squared:  0.1699 
    ## F-statistic: 6.066 on 4 and 95 DF,  p-value: 0.0002167

``` r
par(mfrow=c(2,2))
plot(model1.lm)
```

![](Anz_files/figure-gfm/regression%20model-1.png)<!-- -->

***From the model summary and diagnostics plots above it is evident that
the data is not enough to use for prediction we need more data points in
order to get consistency in predictions. Although we can definitely see
the assumptions of linear models are holding at some corrections but
still we need more data for reliable predictions. ***

***The model’s adjusted R^2 also shows 15% of variation in customers
annual salary which is not enough. hence, it is risky to use this linear
model to predict customer’s income. More data is required to develop a
more significant and reliable model.***

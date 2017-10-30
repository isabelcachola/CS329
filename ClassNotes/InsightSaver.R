I apologize ahead of time how long this insight is going to be. **TDLR: The distribution of the percentage of the goal reached is ridiculous**
  
  My initial goal was to see if I could predict the percentage of the goal that was met. So first I set up the data:
  
  ```{r}
sdf_select <-dplyr::mutate(sdf, net_pledged = pledged-goal,
                           percent_pledged = pledged/goal,
                           diff_create_launch=as.numeric(as.Date(as.character(sdf$launched_at), format='%m/%d/%Y') - as.Date(as.character(sdf$created_at), format='%m/%d/%Y')))
sdf_select <- dplyr::select(sdf_select, goal, backers_count, name_len, blurb_len,created_at_day,launched_at_day, net_pledged, diff_create_launch,percent_pledged)
```

I then performed forward subset selection:
  
  ```{r}
regfit.full=regsubsets(percent_pledged~.,data=sdf_select,method='forward')
reg.summary = summary(regfit.full)
reg.summary
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
which.min(reg.summary$cp)
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],pch=20,col="red")
plot(regfit.full,scale="Cp")
coef(regfit.full,1)
```
![image.png](https://media.data.world/Sf28eYSgQeixBTmtyCzB_image.png)
![image.png](https://media.data.world/dzdJ9vqeQ1ORTc8v6yzH_image.png)
![image.png](https://media.data.world/f6i20BOTHGKgATPnx6QA_image.png)

According the the subset selection, the best predictor of percent_pledged is diff_create_launch by itself, which makes no sense. By my previous insight, diff_create_launch is a terrible predictor of success so I didn't think it would be a good predictor of percentage pledged. Just in case, I ran a simple linear regression:

```{r}
lm.fit = lm(percent_pledged~diff_create_launch, data = sdf_select)
summary(lm.fit)
```

Turns out I was right:

![image.png](https://media.data.world/TMPBjAJtSDKq85p2cF01_image.png)

Only about 9% of the data can be account for by the model and if you look at the plot, it makes sense that it can't be modeled linearly:
  
  ![image.png](https://media.data.world/TMi51d6dRwOwKS9s0WvK_image.png)

It looks like there are a few outliers in percent_pledged, so I decided to take a closer look at it's distribution:

```{r}
ggplot(data=sdf_select, mapping = aes(percent_pledged)) + geom_histogram(bins = 50)
summary(sdf_select$percent_pledged)
```
![image.png](https://media.data.world/3QlMB0oRrCMFOzpvnaSr_image.png)
![image.png](https://media.data.world/mxuZrSR86MFlLEUCltOQ_image.png)

The data has such ridiculous outliers that the histogram is not helpful at all in understanding its distribution. Looking at the summary, the **3rd Quartile is 1.10** but the **max is 22603.000**. So clearly there are a few ridiculously successful outliers. In fact the **variation is 53791.71**, which is also ridiculous. 

So then I try limiting the data to be within 3 standard deviations:
```{r}
ggplot(data=dplyr::filter(sdf_select, percent_pledged < 3*sd(percent_pledged)), mapping = aes(percent_pledged)) + geom_histogram()
```
![image.png](https://media.data.world/NEYmACuRMe6k3g4AQUae_image.png)

Slightly better, still not very helpful

Let's try limiting it to less than the 3rd quartile:
  ```{r}
ggplot(data=dplyr::filter(sdf_select, percent_pledged <= 1.10), mapping = aes(percent_pledged)) + geom_histogram(bins=30)
```
![image.png](https://media.data.world/PkDESymLTiPTt3prSoFw_image.png)

This looks slightly better. It looks like the distribution could potentially exponential up until around 90% then normal for 100% plus or minus 10%. First let's look at less than 90%:
  
  ```{r}
ggplot(data=dplyr::filter(sdf_select, percent_pledged <= .9), mapping = aes(percent_pledged)) + geom_histogram(bins=30) + geom_density(colour='red')
```
This looks like it could be exponential but the density function (in red) is telling a different story. In fact, when we increase the bins to 100, we get a different result:
  ![image.png](https://media.data.world/Au4dWR9q0yhKuvixDFwL_image.png)

It looks like a large portion of Kickstarters receive close to nothing compared to their goal (sad). Then I tried limiting
#Aneeta Uppal
#lab 08
#3/22/17

#1
qPCRfile <- read.delim("/Users/aneetauppal/qPCRwSampleDays.txt", sep = '\t', header = TRUE, row.names =1)
qPCRfile
Log16s <- qPCRfile[,10]
sampleDays <- qPCRfile[,2]
plot(sampleDays, Log16s, main = "Days vs. Log16s")

treatStatus <- qPCRfile[,5]
colors <- vector()

#save each treatment as specific color 
for(i in 1:length(treatStatus))
{
  if(treatStatus[i] == "Treatment")
    colors[i] = "Pink"
  
  if(treatStatus[i] == "Recovery")
    colors[i] = "Blue"
  
  if(treatStatus[i] == "Before Treament")
    colors[i] = "Purple"
  
  if(treatStatus[i] == "stable")
    colors[i] = "Red"
}

#plot treatments and colors according to the colors in the colors vector 

plot(sampleDays, Log16s,col=colors, xlim=c(0,1200), ylim=c(6,11), pch=16, main="Status")
legend("bottomright", c("Treatment", "Recovery", "Before Treatment", "Stable"),
       col=c("pink", "blue", "purple", "red"), pch=16, cex = .75)


#2

#8 parameter model
treatment <- factor(treatStatus)
treatment <- relevel(treatment, ref="Treatment")
fullModel <- lm(Log16s ~ sampleDays * treatment, x=TRUE)
plot(fullModel)
AIC(fullModel)

#5 parameter model
reducedModel <- lm(Log16s ~ sampleDays + treatment, x=TRUE)
plot(reducedModel)
AIC(reducedModel)


#2 parameter model
plot(sampleDays, Log16s, main="Minimal")
minimalModel <- lm(Log16s ~ sampleDays)
plot(minimalModel)
AIC(minimalModel)

fullResiduals <- sum(residuals(fullModel)^2)
fullResiduals
#[1] 31.80586 = error



reducedResiduals <- sum(residuals(reducedModel)^2)
reducedResiduals
# 33.79789 = error

minResiduals <- sum(residuals(minimalModel)^2)
minResiduals
# 38.2617 error

# can check with anova
anova(fullModel)

anova(reducedModel)

anova(minimalModel)

# Full model visualization with fitted lines for treatments just to look at 
coefs <- coef(fullModel)

plot(sampleDays, Log16s,col=colors, xlim=c(0,1200), ylim=c(6,11), pch=16, main="Full W. fitted lines")
#treatment line
abline(fullModel, col="Pink") 
#before treatment
abline( a=coefs[1] + coefs[3], b=coefs[2], col="Blue") 
#recovery 
abline( a=coefs[1] + coefs[4], b=coefs[2], col="Purple") 
#stable
abline( a=coefs[1] + coefs[5], b=coefs[2], col="Red") 


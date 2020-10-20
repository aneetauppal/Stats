#Aneeta Uppal
#3/15/17
#Lab 7

rm(list = ls()) 
table <- read.csv("/Users/aneetauppal/slr01.csv")
table
x <-lm(table$List.Price ~ table$Best.Price)
plot(table$List.Price, table$Best.Price)
summary(x)


casecontrol <- read.table("/Users/aneetauppal/casecontrol.txt", header = TRUE)
casecontrol2 <- read.csv("/Users/aneetauppal/casecontrol.csv", header=TRUE)
BMI_data <- read.table("/Users/aneetauppal/BMI_data.txt", header=  TRUE, sep = "\t")


key <- sub("case", "", casecontrol$sample)
key <- sub("control", "", key)

keys <- vector()

for (i in 1:length(key)){
  keys[i] <- strsplit( key, "_")[[i]][1]
}

keystable <- data.frame(keys)
#Merge keys from samples that match with BMI table
newtable <- merge(BMI_data, keystable, by.x='studyid', by.y='keys')
#Combine keys back to OTUs case control table
secondtable<- merge(newtable, casecontrol2, by.x='studyid', by.y='sample')

pvalues <- vector()


for(i in 3:length(secondtable)) {
  myLm = lm(secondtable[,i] ~ secondtable$bmi)
  pvalues[i] <- anova(myLm)$"Pr(>F)"
}

pvalues
#Adjusted Pvalues
pvaluesadjust <- vector()
pvaluesadjust <- p.adjust(pvalues[!is.na(pvalues)], method="BH")
#adjusted pvalues < .10
pvaluesadjustgtr.10 <- p.adjust(pvalues[!is.na(pvalues)], method="BH") < 0.1



hist(pvaluesadjust)
hist(pvalues)


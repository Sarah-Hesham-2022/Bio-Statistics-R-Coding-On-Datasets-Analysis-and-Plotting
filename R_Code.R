output= file("Output.txt") # File name of output log
sink(output, append = TRUE, type = "output") # Writing console output to log file

#Load the medical data from the given file data.csv, which is a dataset of a patient
#demographic containing standard information regarding individuals from a variety of
#ancestral lines.
data = read.csv(file= "BioStatistics_Assignment One Data.csv", sep=",", header=TRUE)

#Then Perform the following tasks using R:
print("#1. Show the first 10 rows and the last 10 rows.")
print(head(data,10))
print(tail(data,10))

cat("\n")
cat("\n")
print("#2. Using Date of Birth attribute, extracts the gender, average commuting time, and ancestry data for the oldest three.")
ordered_data = data[order(data$dob),]
head(ordered_data[,c("gender","avg_commute","ancestry")],3)

cat("\n")
cat("\n")
print("#3. Identifies the gender, daily internet use, average commute time, ancestry,and diseases among those with more than two children.")
morethan2children_data = subset(data,data$children>2)
print(morethan2children_data[,c("gender","daily_internet_use","avg_commute","ancestry","disease")])

cat("\n")
cat("\n")
print("#4. Using a table , indicate the number of rows that have any missing value and the number that do not.")
NA_rows_num= sum(rowSums(is.na(data)))
NotNA_rows_num = nrow(data) - NA_rows_num
cat("FALSE  TRUE","\n",NotNA_rows_num,"\t",NA_rows_num)

cat("\n")
cat("\n")
print("#5. Provide a summary of the data for each column, showing \"Min, 1st Qu,Median Mean, 3rd Qu and Max\" for each numerical column and the Number of each Category for categorical data.")
for(i in 1:ncol(data)){
  if(class(data[,i])=="character"){
    print(names(data)[i])
    print(table(factor(data[,i])))
  }
  else{
    print(names(data)[i])
    print(summary(data[,i]))}
}

cat("\n")
cat("\n")
print("#6. Identify the columns that are having any missing values, and then remove any rows where all of the columns have missing values.")
colSums(is.na(data))
na.omit(data,c(names(data)))

cat("\n")
cat("\n")
print("#7. Show the average daily usage of the internet for each level of education.")
levels = levels(factor(data$education))
print(levels)
print("#levels [1] \"bachelors\"  \"highschool\" \"highscool\"  \"masters\"    \"phd/md\"     \"phD/MD\" That is like we need data cleaning to make highscool = highschool and phd/md = phD/MD and nlevels = 4 not 6 so")
data$education[data$education=="highscool"]= "highschool"
data$education[data$education=="phd/md"]= "phD/MD"
levels = levels(factor(data$education))
print(levels)
x =list()
for(i in 1:length(levels)){
x[length(x)+1] = (mean(subset(data , data$education == levels[i])[,"daily_internet_use"]))
}
cat(levels,"\n",unlist(x))
barplot(unlist(x),col=topo.colors(length(levels)),main="Avg Daily Internet Usage",ylab="Count",names.arg = levels)

sink()
close(output)

#8. Show the distribution of the children count using a histogram.
hist(data$children,xlab="Children Number",ylab="Count",main="Children Count Dist.",col=topo.colors(7))

#9. Utilizing line graphs, compare how men and women's avg commute
#distributions differ.
levels = levels(factor(data$gender))
print(levels)
par(mfrow=c(1:length(levels)))
for(i in 1:length(levels)){
   plot(subset(data , data$gender == levels[i])[,"avg_commute"],col=i+2,xlab=toupper(levels[i]),ylab="AvgCommute",main="Average Commute Dist.",type="b")
}

#10.Make a histogram to show the gender distribution.
par(mfrow=c(1,1))
levels = table(data$gender)
barplot(levels,col=topo.colors(length(levels)),main="Gender Dist.",ylab="Count",names.arg = c("FEMALES","MALES"))

#11. Use a histogram to show gender distribution for each disease.
levels_disease = levels(factor(data$disease))
levels_gender = levels(factor(data$gender))
myMatrix = matrix(0,ncol=length(levels_disease) , nrow=length(levels_gender), byrow=TRUE,dimnames = list(c(levels_gender),c(levels_disease)))
for(i in 1:length(levels_gender)){
 for(j in 1:length(levels_disease)){
   subsetdata = subset(data,data$gender == levels_gender[i] & data$disease == levels_disease[j])[,"gender"]
   myMatrix[i,j] = length(subsetdata)
 }
}
print(myMatrix)
barplot(myMatrix,
        main = "Gender Diseases Dist.",
        xlab = "Diseases",
        col = c("red","green"),
        beside=TRUE,cex.names=0.6,ylim=c(0,max(myMatrix)+20)
)
legend(x=0,y=max(myMatrix)+22,
       c("Female","Male"),
       fill = c("red","green")
)

#12.Use a chart to demonstrate whether there is a relationship between age
#and the type of disease.
levels_disease = levels(factor(data$disease))
dob = data$dob
age = as.Date(Sys.Date()) - as.Date(dob)
age = ceiling(age/365.25)
data$dob = age
levels_age = levels(factor(age))
myMatrix = matrix(0,nrow=length(levels_disease) , ncol=length(levels_age), byrow=TRUE,dimnames = list(c(levels_disease),c(levels_age)))
for(i in 1:length(levels_disease)){
  for(j in 1:length(levels_age)){
    subsetdata = subset(data,data$dob == levels_age[j] & data$disease == levels_disease[i])[,"dob"]
    myMatrix[i,j] = length(subsetdata)
  }
}
print(myMatrix)
barplot(myMatrix,
        main = "Age Diseases Dist.",
        xlab = "Age",
        col = c(1:length(levels_disease)),ylim=c(0,max(myMatrix)+100)
)
legend(x=0,y=max(myMatrix)+100,c(levels_disease),fill = c(1:length(levels_disease)))

#Question 12. for simplicity, draw a graph to show the age distribution.
hist(as.numeric(data$dob),xlab="Ages",ylab="Count",main="Age Dist.",col=topo.colors(length(unique(as.numeric(data$dob)))),breaks=length(unique(as.numeric(data$dob))))

#13.Make a chart to show the total number of children per disease.
levels_disease = levels(factor(data$disease))
myMatrix = matrix(0,ncol=1, nrow=length(levels_disease), byrow=TRUE,dimnames = list(c(levels_disease),c("Children Count")))
for(i in 1:length(levels_disease)){
    subsetdata = subset(data,data$disease == levels_disease[i])[,"children"]
    myMatrix[i,1] = sum(subsetdata)
}
print(myMatrix)
barplot(myMatrix[1:13],ylim=c(0,max(myMatrix)+50),main="Total Children Per Disease",xlab="Diseases",ylab="Total Children Number",names.arg=levels_disease,col=topo.colors(length(levels_disease)),cex.names=0.6)

#14.Make a chart to show the ancestry distribution
levels = table(data$ancestry)
print(levels)
barplot(levels,col=topo.colors(length(levels)),ylim=c(0,max(levels)),main="Ancestry Dist.",xlab="Ancestry Categories",ylab="Count of Ancestry Categories",cex.names=0.6,names.arg = c(names(levels)))

cat(" Plotting Basics:Jincong Han")
install.package("FSA","FSAdata","maggritr","dplyr","plotrix","ggplot2")
library(¡®FSA¡¯)
library(¡®FSAdata¡¯)
library(dplyr)
library(maggritr)
library(plotrix)
library(ggplot2)
#Now we can load our data
data(BullTroutRML2)#Load the BullTroutRML2 dataset
str(BullTroutRML2)
head(BullTroutRML2,n=3)#4. Print the first and last 3 records from the BullTroutRMS2 dataset
tail(BullTroutRML2,n=3)
BullTroutRML2_Harrison<-filter(BullTroutRML2,BullTroutRML2$lake=="Harrison")#5. Remove all records except those from Harrison Lake
BullTroutRML2_Harrison
head(BullTroutRML2_Harrison,n=5)#6. Display the first and last 5 records from the filtered BullTroutRML2 dataset
tail(BullTroutRML2_Harrison,n=5)
str(BullTroutRML2_Harrison)#7. Display the structure of the filtered BullTroutRML2dataset
summary(BullTroutRML2_Harrison)#8. Display the summary of the filtered BullTroutRML2dataset
plot(BullTroutRML2_Harrison$fl,BullTroutRML2_Harrison$age,#9.9. Create a scatterplot for ¡°age¡± (y variable) and ¡°fl¡± (x variable)
     main=" Plot1:Harrison Lake Trout",
     xlab="Fork length", ylab="Age",
     xlim=c(0, 500), ylim=c(0, 15))
hist(BullTroutRML2_Harrison$age, ylab="Frequency", xlab="Age (yrs)", #10. Plot an ¡°Age¡± histogram with the following specifications
     main="Plot 2: Harrison Fish Age Distribution", 
     xlim=c(0,15), ylim=c(0,15), 
     col="cadetblue", col.main="cadetblue")
plot(BullTroutRML2_Harrison$age~BullTroutRML2_Harrison$fl, xlab="Fork Length (mm)", ylab="Age (yrs)", xlim=c(0,500),
     ylim=c(0,15), pch=19, col="cyan", main="Plot 3: Harrison Density shaded by era") #11. Create an overdense plot using the same specifications as the previous scatterplot.
head(BullTroutRML2,n=3)
tail(BullTroutRML2,n=3)
tmp<-rbind(head(BullTroutRML2,n=3),tail(BullTroutRML2,n=3))
tmp$era#13. Display the ¡°era¡± column (variable) in the new ¡°tmp¡± object
pchs<-c("+","x")#14. Create a pchs vector with the argument values for + and x. 
cols<-c("red","gray60")#15. Create a cols vector with the two elements ¡°red¡± and ¡°gray60¡±
class(tmp$era)#16. Convert the tmp era values to numeric values. 
erac=as.numeric(tmp$era)
class(erac)
colst <- c("red","gray60")#17.Initialize the cols vector with the tmp era values
colst[factor(tmp$era)]
#18. Create a plot of ¡°Age (yrs)¡± (y variable) versus ¡°Fork Length (mm)¡± (x variable)
plot(x= tmp$fl, y = tmp$age, xlim = c(0,500), ylim = c(0,15), pch= ifelse(tmp$era =="1977-80", pchs[1], pchs[2]), col= ifelse(tmp$era == "1977-80", cols[1], cols[2]),ylab
     = "Age(yrs)", xlab = "Fork Length(mm)", main = "Plot 4: Symbol & Color by Era")

#19. Plot a regression line overlay on Plot 4 and title the new graph ¡°Plot 5: Regression Overlay¡±.
plot(x= tmp$fl, y = tmp$age, xlim = c(0,500), ylim = c(0,15), pch= ifelse(tmp$era ==
                                                                            "1977-80", pchs[1], pchs[2]), col= ifelse(tmp$era == "1977-80", cols[1], cols[2]),ylab
     = "Age(yrs)", xlab = "Fork Length(mm)", main = "Plot 5:Regression Overlay")
abline(lm(tmp$age~tmp$fl), col="green", main= "plot5:Regression Overlay")
#20. Place a legend of on Plot 5 and call the new graph ¡°Plot 6: :Legend Overlay¡±abline(lm(tmp$age~tmp$fl), col="green", main= "plot5: Regression Overlay")
plot(x= tmp$fl, y = tmp$age, xlim = c(0,500), ylim = c(0,15), pch= ifelse(tmp$era ==
                                                                            "1977-80", pchs[1], pchs[2]), col= ifelse(tmp$era == "1977-80", cols[1], cols[2]),ylab
     = "Age(yrs)", xlab = "Fork Length(mm)", main = "Plot 6:: Legend Overlay")
abline(lm(tmp$age~tmp$fl), col="green", main= "plot6:: Legend Overlay")
legend("topleft", inset=0.05, legend=levels(tmp$era), col=colst, bty="n", text.col =
         colst)

# descriptive characteristics
mean(BullTroutRML2$fl)
mean(BullTroutRML2$age)
median(BullTroutRML2$fl)
median(BullTroutRML2$age)
quantile(BullTroutRML2$fl)
quantile(BullTroutRML2$age)
var(BullTroutRML2$fl)
var(BullTroutRML2$age)
sd(BullTroutRML2$fl)
sd(BullTroutRML2$age)

#box plot
opar <- par(no.readonly=TRUE)
par(fig=c(0, 0.8, 0, 0.8))
plot(x= tmp$fl, y = tmp$age, xlim = c(0,500), ylim = c(0,15), pch= ifelse(tmp$era ==
                                                                            "1977-80", pchs[1], pchs[2]), col= ifelse(tmp$era == "1977-80", cols[1], cols[2]),ylab
     = "Age(yrs)", xlab = "Fork Length(mm)", main = "Plot 7:box plot")
abline(lm(tmp$age~tmp$fl), col="green", main= "plot7:box plot")
legend("topleft", inset=0.05, legend=levels(tmp$era), col=colst, bty="n", text.col =
         colst)
par(fig=c(0, 0.8, 0.55, 1), new=TRUE)
boxplot(BullTroutRML2_Harrison$fl, horizontal = TRUE,axes=FALSE)
par(fig=c(0.65, 1, 0, 0.8), new=TRUE)
boxplot(BullTroutRML2_Harrison$age, axes=FALSE)
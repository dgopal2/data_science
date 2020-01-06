hist(realestate$lotsize, 
     freq=F,
     col=brewer.pal(10, "Dark2"), 
     breaks=10, 
     xlab="Budget in million $",
     main="Histogram of budget")

rug(jitter(baseball$pay), col="darkgray")
lines(density(baseball$pay), col="yellow", lwd=3) # Note how the lines function is used to overlay the density plot 
box(lwd=2)

# Boxplot 
boxplot(baseball$pay, 
        col="coral", 
        main="Boxplot of budget")



par(mfrow=c(1,1)) # par is for plotting parameters; mfrow is for number of rows/columns
hist(realestate$lotsize, col=c("steelblue", "red"), freq=F, xlab="budget in Millions", main="Distribution of lotsize") 
rug(jitter(realestate$lotsize), col="darkgray")
lines(density(realestate$lotsize), col="yellow", lwd=3) # Note how the lines function is used to overlay the density plot 
box(lwd=1.5)
boxplot(realestate$bedrooms, col="orange", main="Boxplot of budget")


plot(realestate$price~realestate$lotsize) 

# Improved version 
plot(realestate$price~realestate$lotsize, data=realestate, 
     main="Relationship between price and Lotsize", 
     ylab="price", 
     xlab="lotsize", 
     pch=20, 
     col="pink",
     xlim=c(0, 20000)) 

abline(lm(realestate$price~realestate$lotsize), col="red", lwd=2, lty=2) #linetype
lines(loess.smooth(realestate$lotsize,realestate$price), col="blue", lwd=2, lty=1)#shows we have smwth of a linear reln#regrn assumption is normally distributed but this is not so trying to smooth it out
# add identifiers (label observations)
text(realestate$price~realestate$lotsize, cex=.6, col="steelblue", labels=baseball$team)
ticks <- c(35, 70, 100, 150, 200)
axis(side=1,at=ticks) # sides are clockwise from bottom=1 to right=4
abline(h=0.5, v=ticks, lty=2, col="gray") # also may use grid to add gridlines 
box(lwd=2.5, lty=3)  #wont ask in exam few things few might





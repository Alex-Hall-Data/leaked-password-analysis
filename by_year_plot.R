input<-read.csv("C:/Users/Alex/Documents/R/password analysis/passwords_by_year.csv")


library(ggplot2)
library(directlabels)

plot(1,1,ylim=c(0,50),xlim=c(2007,2017),xlab="last login year",ylab="% confidence",main="confidence for password type by year")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="lightgray")
points(input$year,input$single_word,type="l",col="red")
points(input$year,input$lowercase_only,type="l",col="blue")
points(input$year,input$text_only,type="l",col="darkgreen")

grid(ny = 10, nx = NA, col = "gray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)

text(locator(), labels = c("single word", "lowercase only","text only"))
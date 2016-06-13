rm(list=ls())
setwd("D:/LEAM/Freq_Map")

library(ggplot2)

FLAG_C = 1
FLAG_P = 10

if (FLAG_C == 1) {
  if (FLAG_P == 10) {
  my_data = read.csv("./trcostfreq/tcf_c_10.csv")
  my_data$x2 = my_data$x ** 2
  my_data$x3 = my_data$x ** 3
  my_data$x4 = my_data$x ** 4
  smod = lm(y ~ x + x2 + x3 + x4, data = my_data)
  my_data$x_new = seq (0, 100, length.out = 10)
  new_data = my_data
  new_data$x = my_data$x_new
  new_data$x2 = my_data$x_new ** 2
  new_data$x3 = my_data$x_new ** 3
  new_data$x4 = my_data$x_new ** 4
  new_data$y = my_data$y
  new_data$new_y = predict(smod, newdata=new_data)
  my_data$new_y = new_data$new_y 
  my_data$new_y[my_data$new_y < 0] = 0 
  } 
}





p1 = ggplot(data = my_data, aes(x=x,y=y)) + geom_point() + 
  geom_line(aes(x=x_new,y=new_y),colour="Blue")  +
  labs(x = "travel time to centers in min", y = "percent of commerical cells") +
  theme(text = element_text(size=15),axis.text = element_text(size=10),
        axis.title=element_text(size=15))+ggtitle("Percent of commercial cells to travel time")

p1
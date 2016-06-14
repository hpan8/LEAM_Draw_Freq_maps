rm(list=ls())
setwd("D:/LEAM/Freq_Map")

library(ggplot2)

FLAG_C = 0
FLAG_P = 50

if (FLAG_C == 1) {
  if (FLAG_P == 10) {
    my_data = read.csv("./trcostfreq/tcf_c_10.csv")

  }
} else {
    if (FLAG_P == 10) {
      my_data = read.csv("./trcostfreq/tcf_r_10.csv")  
  } else if (FLAG_P == 20){
      my_data = read.csv("./trcostfreq/tcf_r_20.csv") 
  }
  else if (FLAG_P == 50){
    my_data = read.csv("./trcostfreq/tcf_r_50.csv") 
  }
}

  my_data$x2 = my_data$x ** 2
  my_data$x3 = my_data$x ** 3
  my_data$x4 = my_data$x ** 4
  smod = lm(y ~ x + x2 + x3, data = my_data)
  new_data = seq (0, 100, length.out = 100)
  new_data = data.frame(new_data)
  new_data$x_new = seq (0, 100, length.out = 100)
  new_data$x = new_data$x_new
  new_data$x2 = new_data$x_new ** 2
  new_data$x3 = new_data$x_new ** 3
  new_data$x4 = new_data$x_new ** 4
  new_data$new_y = predict(smod, newdata=new_data)
  #my_data$new_y = new_data$new_y 
  #my_data$new_y[my_data$new_y < 0] = 0 
  a = smod$coef["(Intercept)"]
  b = smod$coef["x"]
  c = smod$coef["x2"]
  d = smod$coef["x3"]
  #e = smod$coef["x4"]
  test_fun = function (x) {
    a + b*x + c*x^2 + d*x^3}



lab = paste("y=",round(a, digits = 1), "+", round(b, digits = 2), 
                        "x","+", round(c, digits = 3), "x^2","+", round(d, digits = 7) , 
                        "x^3", sep = "")
lab = print(lab)

p1 = ggplot(data = my_data, aes(x=x,y=y)) + geom_point() + 
  labs(x = "travel time to centers in min", y = "Percent of commerical cells") +
  theme(text = element_text(size=15),axis.text = element_text(size=10),
        axis.title=element_text(size=15))+ggtitle("Percent of residential cells to travel time") +
  stat_function(fun=test_fun , color ="blue") +
  annotate("text", x = 40, y = 0.2, 
           label = lab)


p1

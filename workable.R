#q1:

blob <- function(a){
     if (a%%3==0 && a%%5==0) {
          print("Royal Blood")
     } else if (a%%3==0){
          print("Royal")
     } else if (a%%5==0){
          print("Blood")
     } else
          print(a)
}

blub <- Vectorize(blob)
q1 <- blub(1:1000)


#----------
blob <- function(a){
     print(ifelse(a%%3==0 && a%%5==0, "Royal Blood",
                  ifelse(a%%3==0, "Royal",
                         ifelse(a%%5==0, "Blood", a))))
}

blub <- Vectorize(blob)
q1 <- blub(1:1000)

#----------
for (a in seq(1000)) {
     print(ifelse(a%%3==0 && a%%5==0, "Royal Blood",
                  ifelse(a%%3==0, "Royal",
                         ifelse(a%%5==0, "Blood", a))))
}
#----------
#
for (a in seq(1000)) {
     if (a%%3==0 && a%%5==0) {
          print("Royal Blood")
     } else if (a%%3==0){
          print("Royal")
     } else if (a%%5==0){
          print("Blood")
     } else
          print(a)
}
#---------------------

#q2:

a <- c(2,6,3,9,4,5)
print(a)

for (i in (1:length(a))) {
     for (j in (1:(length(a)-1))) {
          if (a[j] > a[j+1]) {
               swap(a[j], a[j+1])
          }
     }
}

print(a)

#---------------------

#q3:

f <- function(x) x^2 - 2*x +1
x1 = -1
x3 = 3

set.seed(123)
x2 <- runif(1, -1, 3)

f1 <- f(x1)
f2 <- f(x2)
f3 <- f(x3)

b <- x3 - x2
a <- x2 - x1

if (b > a) {
     x4 <- runif(1, x2, x3)
} else {
     x4 <- runif(1, x1, x2)
}


















#----------------
## BONUS

RW <- arima.sim(model = list(order = c(0,1,0)), n=100)
ts.plot(RW)
RWd <- diff(RW)
ts.plot(RWd)









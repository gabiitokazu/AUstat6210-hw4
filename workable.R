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

a <- c(9,5,2,6,3,-9,4,5,-6,2,8,34,89,6)
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
tol <- 0.001
dif <- tol + 1
x1 = -1
x3 = 3
f_old <- f(x1)

#---survival group:

func <- function(x) x^2 - 2*x +1
tol <- 1e-5
dif <- tol + 1
x1 = -1
x3 = 3
set.seed(123)
x2 <- runif(1, x1, x3)

func_old <- c(x1,x2,x3)

while(dif > tol || dif == 0){

        if((x2-x1) > (x3 - x2)){
                x4 <- runif(1, x1, x2)
        } else {
                x4 <- runif(1, x2, x3)
        }

        if((func(x4) < func(x2)) && (func(x4) < func(x3))){
                middle_new <- x4
                #func_new <- c(x2, x4, x3)
                x1 <- x2
                x2 <- x4
                x3 <- x3
        } else {
                middle_new <- x2
                #func_new <- c(x1, x2, x4)
                x1 <- x1
                x2 <- x2
                x3 <- x4
        }

        middle_old <- func_old[2]
        dif <- abs(func(middle_old) - func(x2))

        func_old = c(x1,x2,x3)

        print(func_old)
        print(func(func_old))


}

print(middle_new)
print(func(middle_new))







#--q3:

f <- function(x) x^2 - 2*x + 1
threshold<- .Machine$double.eps^0.25
delta <- 0
x1 = -1
x3 = 3
set.seed(666)
x2 = runif(1, x1, x3)

i <- c(x1, x2, x3)
i_1 <- c(0, 0, 0)

while(delta > threshold || delta == 0){

        if((i[2]-i[1]) > (i[3] - i[2])){

                x4 <- runif(1, i[1], i[2])

                if((f(x4) < f(i[2]))){
                        i_1 <- c(i[1], x4, i[2])
                } else {
                        i_1 <- c(x4, i[2], i[3])
                }
        } else {
                x4 <- runif(1, i[2], i[3])

                if((f(x4) < f(i[2]))){
                        i_1 <- c(i[2], x4, i[3])
                } else {
                        i_1 <- c(i[1], i[2], x4)
                }
        }

        delta <- abs(f(i[2]) - f(i_1[2]))

        i = i_1
}

print(i_1[2])
print(f(i_1[2]))


## ------------------------
##

f <- function(x) x^2 - 2*x + 1
d1 <- Deriv(f,"x")                  # first derivative
d2 <- D(D(f,"x"),"x")           # second derivative
i <- 3
factor <- 1e-5

i1 <- i - factor * (d1)





































#----------------
## BONUS  --totally gave up

RW <- arima.sim(model = list(order = c(0,1,0)), n=100)
ts.plot(RW)
RWd <- diff(RW)
ts.plot(RWd)

#nao eh for porque eu nao sei quantas vezes vou fazer
#tem que ser um while
#pra parar quando chegar no acucar
# t maximo = 150

#setting up the variables (length, angle, position)
S <- runif(1, 0, 2)
angle <- runif(1, 0, 2*pi)
position <- matrix(c(runif(1, -9, 9), runif(1, -9, 9)), 2, 1)

angle_change <- matrix(c(cos(angle),sin(angle)), 2, 1)

#next step:
position_new <- position + S * angle_change

# vou fazer uma funcao entao, e correr ela pras 20 bacterias:

BacRW <- funtion(p)


#-------



#########AQUAQUIQUAIQUAQIIIIIIIII-----

walk.2d<-function(n)
{
        rw <- matrix(0, ncol = 2, nrow = n)

        # generate the indices to set the deltas
        indx <- cbind(seq(n), sample(c(1, 2), n, TRUE))

        # now set the values
        rw[indx] <- sample(c(-1, 1), 2, TRUE)

        # cumsum the columns
        rw[,1] <- cumsum(rw[, 1])
        rw[,2] <- cumsum(rw[, 2])

        rw  # return value
}


#------------

n <- 10000
rw <- matrix(0, ncol = 2, nrow = n)
# generate the indices to set the deltas
indx <- cbind(seq(n), sample(c(1, 2), n, TRUE))

# now set the values
rw[indx] <- sample(c(-1, 1), n, TRUE)
# cumsum the columns
rw[,1] <- cumsum(rw[, 1])
rw[, 2] <- cumsum(rw[, 2])

plot(0,type="n",xlab="x",ylab="y",main="Random Walk Simulation
In Two Dimensions",col=1:10,xlim=range(rw[,1]),ylim=range(rw[,2]))





n<-1000

rw <- walk.2d(n)
plot(0, type="n",xlab="x",ylab="y",main="Random Walk Simulation In
Two Dimensions",xlim=range(rw[,1]),ylim=range(rw[,2]))

# use 'segments' to color each path
segments(head(rw[, 1], -1), head(rw[, 2], -1), tail(rw[, 1], -1), tail(rw[,
                                                                          2], -1), col ="blue")


source(url("http://aliquote.org/pub/spin_plot.R"))
dd <- replicate(3, rnorm(100))
spin.plot(dd)









###----- graph----
# update every loop?
# update every "x" loops?
# update data on plot dynamically!!


#points()
#lines()

# package magick

# from discussionforum: https://towardsdatascience.com/animating-your-data-visualizations-like-a-boss-using-r-f94ae20843e3


















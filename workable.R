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
     for (j in (1:(n-1))) {
          if (a[j] > a[j+1]) {
               swap(a[j], a[j+1])
          }
     }
}

print(a)

#---------------------

#q3:

f <- function(x) x^2 - 2*x +1








#---------------------

#q4:



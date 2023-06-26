(Sigma <- matrix(c(1,-1/2,0,0,-1/2,1,0,0,0,0,1,1,0,0,1,1),nrow =4, byrow=T))

det(Sigma)

(C <- matrix(c(1,0,1,0,0,1,0,1),nrow=2,byrow=T))
(SigmaYZ <- C%*%Sigma%*%t(C))
library(MASS)
fractions(solve(SigmaYZ))



#1.5
(A <- matrix(c(1,0,1,1,0,0,0,1,1),nrow=3, byrow=T))
X <- c(2,5,3,1)
betah <- solve(t(A)%*%A)%*%t(A)%*%X



#Opgave 3
moral <- read_table2("moral.txt", 
                     col_types = cols(koen = col_factor(levels = c("mand", 
                     "kvinde")), rang = col_factor(levels = c("officer", 
                     "menig"))))
#3.1
library(tidyverse)
count(interactions(moral, koen, rang))








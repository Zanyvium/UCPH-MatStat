model.matrix(~x+x^2+x^3,data.frame(x=1:10))

model.matrix(~x+I(x^2)+I(x^3),data.frame(x=1:10))


model.matrix(~poly(x,3),data.frame(x=1:10))







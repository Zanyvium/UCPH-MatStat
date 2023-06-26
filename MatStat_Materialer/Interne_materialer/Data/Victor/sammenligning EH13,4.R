opgave13_4 <- read_table2("opgave13_4.txt", col_types = cols(Blander = col_factor(levels = c("1", "2", "3")), Bryder = col_factor(levels = c("1", "2", "3"))))


m0 <- lm(Respons ~ Blander:Bryder - 1, opgave13_4)
summary(m0)
#Opgave 13.4(b)
#I bedste fald af dimension #blander + #bryder - 1, thi vi vil altid miste et parameter!
#Se side 528 lemma 13.10, eq. (13.2).
A <- model.matrix(m0)
P <- A %*% solve(t(A) %*% A) %*% t(A)
m1 <- lm(Respons ~ Blander + Bryder, opgave13_4)
summary(m1)
#S. 23 L_B + L_B' subset L_BxB' 
#Bruder eq. (10.31)
#F = || P(X) - P^*(X) ||^2 / (dim L_BxB' - dim L_B + L_B')
# / ||X - P(X)||^2 / (|I| - dim L_BxB')
anova(m1, m0)


m0 <- lm(Respons ~ Blander:Bryder - 1, opgave13_4)
summary(m0)

m1 <- lm(Respons ~ Blander + Bryder, opgave13_4)
summary(m1)

anova(m1,m0)

model134 <- lm(Respons ~ 0 + Blander*Bryder, data = opgave13_4)
summary(model134)

add <- lm(Respons ~ 0 + Blander + Bryder, data = opgave13_4)
summary(add)

anova(add,model134)









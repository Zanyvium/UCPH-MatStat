library(tidyverse)

#'General R tips:
#'1. Use SHIFT+CTRL+C to commesnt/uncomment multiple selected lines
#'2. Use CTRL+ENTER to execute selected lines of codes
#'  2.5 Using CTRL+ENTER without selecting lines executes the current line
#'3. Use SHIFT+CTRL+ENTER to execute all lines of code of a document.
#'4. SHIFT+ALT+K gives list of keyboard shortcuts.
#'5. view('Dataset') (without the ') shows a dataset.
#'6. writing #' all thereon following "ENTER" created lines will be commented out
#'7. attach('Dataset') lets us call the column names as variables in R.
#'8. 'help.start()' gives an overview of all functions available.
#'9. SHIFT+CTRL+ALT+F12(+Fn) zooms to left panes, pressing this once again redoes the original
#'10. ALT + - (minus) gives the assignment (<-) operator
#'11. RMarkdown is nice.
#'12. testing equality is conduced with '==', and NOT '='.
#'13. 
#'
#'
#'
#'
#3 Data Visualisation
#3.1 Introduction



cardat<-mpg
attach(cardat) #attach allows us to 

#3.2: First steps----
view(mpg) #Showing the dataset

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y = hwy))

#3.2.4E
#1.
ggplot(data = mpg) #Blank plot panel

#We might want to use this "basis" in future data visualizations
p<-ggplot(data=cardat)

#2. 
nrow(mpg) #Number of rows of mpg = 234
ncol(mpg) #Number of columns of mpg = 11

dim(cardat) #nrows = 234, ncols = 11

#3.
?mpg #\equiv help(mpg)
# By ?mpg, drv describes the type of drive train

#4. 
p + geom_point(mapping = aes(hwy, cyl))


#5. 
p + geom_point(mapping = aes(class, drv))

#Plot is more of an overview than anything else, as we are dealing with 
# two types of qualitative/descriptive data <- The correct terminology is 
# "categorical data" (as opposed to "continuous (numbered/ordered) data")

# --- ? --- Interplay between categorical data, factors, and nomial/ordinal categorical variables
# see guru99.com/r-factor-categorical-continous.html



#Section 3.3: Aesthetic mappings----

p+geom_point(mapping = aes(x = displ, y = hwy, colour = class))

p+geom_point(mapping = aes(x = displ, y = hwy, size = class))

# Left
p+geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# Right
p+geom_point(mapping = aes(x = displ, y = hwy, shape = class))

p+geom_point(mapping = aes(x = displ, y = hwy), color = "blue")


#3.3.1 Excercises
#1. ---- What’s gone wrong with this code? Why are the points not blue?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

#The points are not blue, as the manual entering of colour, has to occur
# outside aes.

#2. --- Which variables in mpg are categorical?
#Which variables are continuous? (Hint: type ?mpg to read the documentation for the dataset).
#How can you see this information when you run mpg?
?mpg

#The following is reported;

# manufacturer
# -> manufacturer name
 
# model
# -> model name
 
# displ
# -> engine displacement, in litres

# year
# -> year of manufacture

# cyl
# -> number of cylinders

# trans
# -> type of transmission
 
# drv
# -> the type of drive train, where f = front-wheel drive, r = rear wheel drive, 4 = 4wd

# cty
# -> city miles per gallon

# hwy
# -> highway miles per gallon
 
# fl
# -> fuel type
 
# class
# -> "type" of car

#ie. we will have 
#Categorical;
#' manufacturer,
#' model
#' trans
#' drv
#' fl
#' class


#Continuous;
#' displ
#' year
#' cyl
#' cty
#' hwy

#********

#the mechanism for assignment of these variable into the two camps through ?mpg
# is based on the wordings of the different variables.
# For example; trans describes "the type of transmission"
# with type a priori assumed to be very much categorical based on the wording

# From the definition of a categorical variable being one which in some way intends to simple "add info" about
# the continuous data (data that takes on measurements as values, and are often stored as numeric, or integer),

# Note that we split categorical variables into nomial categorical variables, which have no ordering (cannot be
# ordered in some way amongst themselves) - such as colours "blue", "green", "red", ..., unless we maybe seek to explain
# which colour we might like the best - as such, whether the colours would even be considered a categorical variable 
# could be discussed. - 
#  We then call categorical variables which are in some way able to be ordered, ordinal.

#   Note that we might be able to spot many of the variables that could be considered as categorical by them
# being read into R as "chachater"

#********

#3.---- Map a continuous variable to color, size, and shape.
#How do these aesthetics behave differently for categorical vs. continuous variables?

#We pick a (displ, hwy)-scatterplot and pick cty as our aesthetic determining
#continous variable
#--- ? --- How do you get rid of the slider for cont. variables that are integer?
#--- ? --- How do you change the colour-range from blue to something else?

p+geom_point(mapping = aes(x = displ, y = hwy,  colour = cty)) 

p+geom_point(mapping = aes(x = displ, y = hwy,  size = cty))

p+geom_point(mapping = aes(x = displ, y = hwy,  shape = cty)) #Doesn't work

#Size --- ! --- Notice that size doesn't scale with cont. variables fully.
#Shape --- ! --- Notice that cont. variables cannot be mapped to a shape.

#We pick a (displ, hwy)-scatterplot and pick class as our aesthetic determining 
#categorical variable

p+geom_point(mapping = aes(x = displ, y=hwy, colour = class))

p+geom_point(mapping = aes(x = displ, y = hwy, size = class))

p+geom_point(mapping = aes(x = displ, y = hwy, shape = class)) 

#We get the SUV-problem from section 3.2


#4. ---- What happens if you map
#the same variable to multiple aesthetics?

#A continous variable
p+geom_point(mapping = aes(x = displ, y=hwy, colour = cty, size = cty))


#A categorical variable
p+geom_point(mapping = aes(x = displ, y=hwy, colour = class, size = class))


#We get two different ways of doing the same differentiation 
#in both the continous, and categorical case.

#Note that we get the warning-message
#"Using size for a discrete variable is not advised",
#in the categorical case, as we try to assign each class a unique size

#5. ---- What does the stroke aesthetic do?
#What shapes does it work with?
#(Hint: use ?geom_point)

#Stroke deals with the border of the points
?geom_point #recommends using the below command;
?vignette #What is a vignette?
vignette("ggplot2-specs") 

#By the above;
#"Note that shapes 21-24 have both stroke colour
#and a fill. The size of the filled part is controlled
#by size, the size of the stroke is controlled by
#stroke. Each is measured in mm, and the total
#size of the point is the sum of the two."

#6.---- What happens if you map an aesthetic to
#something other than a variable name, 
#like aes(colour = displ < 5)?
#Note, you’ll also need to specify x and y.

#We pick our usual (displ, hwy) example;
p+geom_point(mapping = aes(x = displ, y = hwy, colour = cty < mean(cty))) #<!!!!! Important!!!

#3.4 Common Problems ----

#' We can't do
#' ggplot(data = mpg) 
#' + geom_point(mapping = aes(x = displ, y = hwy))
#' 
#' it has to be 
#' 
#' ggplot(data = mpg) +
#' geom_point(mapping = aes(x = displ, y = hwy))
#' 


#3.5 Facets ----
































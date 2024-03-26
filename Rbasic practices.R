library(readr)
library(readxl)
library(googlesheets4)

getwd()
"C:\path..cereal.csv"
data <- read_csv("C:\path..cereal.csv")


#reading csv file
data <- read.csv("cereal.csv")
data

#first 6 rows of data
head(data)

#column names
colnames(data)

#a short brief of data
summary(data)


-----------
#first steps with r
#defining variables, either = or <- symbols are used

a <- 59
a

a = 59
a

b = 20

#we can make calculations on variables
sqrt(b) * a

sum.ab <- a+b
sum.ab
------------------------------#vectors----------------------
#we create single numeric, character, or logical element, single-dimensional vectors when defining.
#in R, vectors are represented by comma-separated values inside c().

x <- c(10,11,12,15,22)
x

length(x) #x is a numeric vector, and I can display its length this way
sort(x) # sorting ascending
rev(sort(x)) #sorting descending
sum(x) #sum of vectors

y <- c(1,4,2,8,3)

sum(x) - sum(y) # substraction of vectors

x - 2 # We can perform many operations such as subtracting a certain number from each value

#Factors are a special form of vectors used to store data for a variable.


#factor(x, levels=c(), labels=c(), ordered= F,...)
#We can represent income levels numerically within the income variable, define a category, and perform sorting operations


y <- c(1,2,3,2,2,2)
gelir <- factor(y, levels= 1:3, labels = c("Düşük", "Orta","Yüksek"), ordered = T)

#dataframe

str(data)

dim(data) #checking the size

names(data)

head(data, 2)

library(dplyr)

# making dataframe
df <- data.frame(
  id = c(1, 2, 3),
  name = c("A", "B", "C"),
  age = c(25, 30, 35)
)

# filtering with pipes
df_filtered <- df %>%
  filter(age > 25) %>%
  select(name, age)
print(df_filtered)

--#making dataframe

dfyeni <- data.frame(a = 1:4, b = letters[1:4])
dfyeni


-------------------------------------------------------------
#choosing one column
  
protein_column <- select(data, protein)

print(protein_column)
summary(protein_column)
------------------------------------------------

#filtering

library(dplyr)

filtered_protein_data <- data %>%
  filter(protein > 4 & calories < 150) %>%
  select(name, protein, calories)

print(filtered_protein_data)


--------------------------------------

filtered_low_sodium_calorie <- data %>%
  filter( sodium <10 & calories <100)%>%
  select(name, sodium, calories)

print(filtered_low_sodium_calorie)

---------------------------------------------
filtered_high_pr_or_low_carb <-data %>%
  filter( protein >4 | carbo <10)%>%
  select(name, protein, carbo)

print(filtered_high_pr_or_low_carb) 

------------------------------------------------
  
ordered_data <- data %>%
  arrange(desc(protein), desc(calories))

print(ordered_data)


head(ordered_data)

------------------------
  
#yadding new column and variables
  library(dplyr)

data <- data %>%
  mutate(protein_level = ifelse(protein > 5, "High", ifelse(protein >= 3, "Moderate", "Low")))

print(data)

------
data <- data %>%
  mutate(sugar_level = ifelse(sugars == 0, "ZeroSugar", "Not Sugarfree"))

print(data)



#transmutate function()
  library(dplyr)


sampledf <- data.frame(m = 1:5, n = 6:10)

new_df <- transmute(sampledf, z = m + n)

print(new_df)

------------------
  
sorted_ratings <- data %>%
  arrange(rating)

head(sorted_ratings,5)

-----------------------
best_rated_cereals <- data %>%
  arrange(desc(rating))%>%
  select(name, rating)

head(best_rated_cereals)




library(readr)
library(readxl)
library(googlesheets4)

getwd()
"C:\\Users\\medda\\OneDrive\\Belgeler\\datasets\\cereal.csv"
data <- read_csv("C:\\Users\\medda\\OneDrive\\Belgeler\\datasets\\cereal.csv")


#CSV dosyasını okutuyorum
data <- read.csv("cereal.csv")
data

#ilk 6 satırlık veri özetlemesi
head(data)

#sütun isimlerini sıralıyorum
colnames(data)

#çalışacağım verilerin kısa bir özetini görüntülüyorum
summary(data)


-----------
#first steps with r
#değişken tanımlama yapılırken = ya da <- işaretlerinden faydalanır

a <- 59
a

a = 59
a

b = 20

#değikenler üzerinden matematiksel hesaplamalar yapabiliriz
sqrt(b) * a

sum.ab <- a+b
sum.ab
------------------------------#vektörler----------------------
#tanımladığımız tek bir nümerik, karakter ya da mantıksal tek elemanlı ve tek boyutlu vektörler oluşturur.
#r'da vektörler c() içerisinde virgülle ayrılmış bir şekilde gösterilir.

x <- c(10,11,12,15,22)
x

length(x) #x bir nümerik vektördür ve bu şekilde kaç elemanlı olduğunu görüntüleyebilirim
sort(x) # küçükten büyüğe sıralama
rev(sort(x)) #büyükten küçüğe sıralama
sum(x) #vektör elemanlarının toplamı

y <- c(1,4,2,8,3)

sum(x) - sum(y) # vektör elemanlarının toplamlarının birbirinden çıkarılması

x - 2 # her bir değerden belirli sayı çıkarılması gibi pek çok işlem yapabiliriz

#faktörler vektörlerin özel biçimidir bir değişkene yönelik verileri depolamak için kullanılır


#factor(x, levels=c(), labels=c(), ordered= F,...)
#gelir değişkeni içerisinde hem gelir düzeylerini nümerik olarak gösterip, hem bir kategori
#tanımlayıp hem de sıralama işlemi yapabiliyoruz


y <- c(1,2,3,2,2,2)
gelir <- factor(y, levels= 1:3, labels = c("Düşük", "Orta","Yüksek"), ordered = T)

#dataframe yapısı

str(data)

dim(data) #verisetinin büyüklüğünü görmek için

names(data)

head(data, 2)

library(dplyr)

# Örnek bir dataframe oluşturma
df <- data.frame(
  id = c(1, 2, 3),
  name = c("A", "B", "C"),
  age = c(25, 30, 35)
)

# Filtreleme için pipes kullanımı
df_filtered <- df %>%
  filter(age > 25) %>%
  select(name, age)
print(df_filtered)

#dataframe oluşturma

dfyeni <- data.frame(a = 1:4, b = letters[1:4])
dfyeni


-------------------------------------------------------------
#sadece belirli bir kolonu seçme işlemi
  
protein_column <- select(data, protein)

print(protein_column)
summary(protein_column)
------------------------------------------------


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
  
#yeni bir sütun ekleyerek kategorilendirme yapıyorum
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

#Yine detaylı bir şekilde eklememi istediğiniz fonksiyonlar
#varsa ekstra tekrar yazarak gönderebilirim hocam dataframeler ile
#işlem yaparak gerçek veri üzerinden pratik yapmayı hedefledim.


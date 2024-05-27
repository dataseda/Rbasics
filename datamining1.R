install.packages("MASS")
library(MASS)

set.seed(123) #tekrarlanabilir aynı verileri elde edebilmek için
x1 <- rnorm(200, mean = 85, sd = 5) # Bağımsız değişken 1
x2 <- rnorm(200, mean = 102, sd = 12) # Bağımsız değişken 2
x3 <- rnorm(200, mean = 3, sd = 0.5) # Bağımsız değişken 3
x4 <- rnorm(200, mean = 6, sd = 2) # Bağımsız değişken 4
x5 <- rnorm(200, mean = 64, sd = 7) # Bağımsız değişken 5
y <- 12 + 1.5*x1 - 0.2*x2 + 2*x3 + 0.8*x4 - 1.8*x5 + rnorm(200, mean = 0, sd = 2) # Bağımlı değişken

# Oluşturulan veri setini bir veri çerçevesine dönüştürme
mydata <- data.frame(y, x1, x2, x3, x4, x5)

#ilk 6 satırı görüntüleme
head(mydata)

#birkaç değişkeni kategorik değişkene dönüştürerek faktör tanımlaması yapalım

# x3 değişkenini faktörel değişkene dönüştürme
x3_f_factor <- as.factor(x3)


#veri tiplerini kontrol etme
str(x1)
str(x4) 
str(x3_f_factor)

#bazı değişkenleri kategorik hale getirdim ve bunu da tekrar dataframe haline getiriyorum
newdata <- data.frame(y, x1, x2, x3_f_factor, x4, x5)

#sütun isimleri görüntüleme
names(newdata)


head(newdata)

# Kategorik grupların merkezlerini belirleme
category_centers <- c(1, 2, 3, 4, 5) # A, B, C, D kategorilerinin merkezleri

# Kategorilere ayırma
x3_categories <- cut(x3, breaks = category_centers, labels = c("A", "B", "C", "D"))

# Oluşturulan veri setini bir veri çerçevesine dönüştürme
newdata <- data.frame(y, x1, x2, x3_categories, x4, x5)

# İlk 6 satırı görüntüleme
head(newdata)


#kategorilere ayırma işlemi gerçekleşmiş diye unique değerleri görüntülüyorum
unique_values <- unique(newdata$x3_categories)
print(unique_values)

# Eksik veri kontrolü
#is.na() fonksiyonu ile bir hücredeki eksik verileri kontrol edebilirim ancak tüm dataframedeki eksik
#verileri kontrol edecek bir fonksiyon ile bunları tek seferde kontrol etmek istiyorum


any_missing <- apply(newdata, 2, function(x) any(is.na(x)))

#burada 2 olarak atanan kısım MARGIN parametresi olup, kontrol edilecek olan boyutu belirler.
#eğer sadece 1 yazsaydık sadece satırları kontrol edecekti ancak biz sütun üzerinden kontrol etmek
#istediğimiz için 2 yazdık. her bir eksik değer için kontrol et ve any_missing değişkeni olarak tanımlama
#işlemi gerçekleştirdik. daha sonra bunların hangi sütunlar olduğunu yazdırıyorum

missing_columns <- names(newdata)[any_missing]

print(missing_columns)



# Eksik değerlerin sayısını hesaplamak için sum() fonksiyonu kullanıyorum
missing_values_count <- sum(is.na(newdata))

# Toplam eksik değer sayısını yazdırın
print(missing_values_count)

#eksik verilerim gözükmüyor buna rağmen kategorik veriler için eksik veri olsaydı; ortalama ile doldurmak yerine 
#bunları silmeyi tercih edebilirdim. bir diğer
#yöntem de moduna göre doldurmak olabilirdi. her iki yöntemi de yazalım bu durumda

#kategorik değişkenin modunu bulma

mod <- names(sort(table(newdata$x3_categories), decreasing = TRUE))[1]

# Bulunan modu yazdırma
print(mod)

# Eksik verileri bulunan moda göre doldurma
newdata$x3_categories[is.na(newdata$x3_categories)] <- mod

any_missing_new <- apply(newdata, 2, function(x) any(is.na(x)))
missing_columns_new <- names(newdata)[any_missing_new]

print(missing_columns_new)
missing_columns_new <- names(newdata)[any_missing_new]

print(missing_columns_new)

#eğer silmek isteseydim cleaned_data <- na.omit(newdata) ile silebilirdim.



install.packages("ggplot2")
library(ggplot2)
#kategorik değişken için frekans analizi yapıyorum
category_counts <- as.data.frame(table(newdata$x3_categories))

# Çubuk grafik oluşturma
ggplot(category_counts, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  labs(x = "Kategori", y = "Frekans", title = "x3_categories Kategorik Değişkeninin Frekans Dağılımı") +
  theme_minimal()

summary(category_counts)

# "D" kategorisine sahip gözlemleri sayma
D_count <- sum(newdata$x3_categories == "D")

# Sonucu yazdırma
print(D_count)

#sadece belirli bir değişken için boxplot grafiği çizme
boxplot(newdata$y)

z_scores <- scale(mydata)
outliers <- abs(z_scores) > 3

summary(outliers)

#x1 değişkeninde 1 adet aykırı değer var gözüküyor ancak yine de boxplot grafikler ile
#hepsinin numerik değerlerinden çizim yapıyorum

boxplot(mydata)

#burada y değişkeninde de bir outlier gözüküyor

ggplot(mydata, aes(y = y)) +
  geom_boxplot() +
  labs(y = "Değer", title = "y Değişkeni İçin Kutu Grafiği")

summary(mydata$y)

#burada summary() ile max değerden de 50.000 üzerinde kalan aykırı değeri görebiliyoruz

#değişkenlerin dağılımını da grafiğe dökelim

hist(newdata$y)

hist(newdata$x1)

# Alt ve üst çeyreklikleri hesaplama
Q1 <- quantile(newdata$y, 0.25)
Q3 <- quantile(newdata$y, 0.75)

# Alt ve üst sınırı hesaplama
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

outliers <- newdata$y < lower_bound | newdata$y > upper_bound

summary(outliers)

#yine bu yöntemle de bir adet outlier bulduk hangi gözlem olduğunu da şu şekilde görüntülüyorum

# Sadece TRUE olan gözlemleri filtreleme
outlier_values <- newdata[outliers, ]

# Sonuçları görüntüleme
print(outlier_values)

# Q-Q plot oluşturma
qqnorm(newdata$x1)
qqline(newdata$x1)

qqnorm(newdata$y)
qqline(newdata$y)


#korelasyon katsayılarını numerik değerler için hesaplamak istiyorum
# Sürekli değişkenlerden oluşan bir alt veri çerçevesi oluşturma
continuous_data <- newdata[, c("y", "x1", "x2", "x4", "x5")]

# Korelasyon katsayılarını hesaplama
correlation_matrix <- cor(continuous_data)

# Korelasyon matrisini yazdırma ve görselleştirme
print(correlation_matrix)
library(corrplot)
corrplot(correlation_matrix, method = "color")

#regresyon modelini oluşturmadan önce kategorik değişkenler için dummy kodlaması yapıyorum

install.packages ('fastDummies')
library(fastDummies)
dummy_cols(newdata, select_columns = "x3_categories")
yeni_regresyon <- dummy_cols(newdata, select_columns = c("x3_categories"))
model <- lm(y ~ x1 + x2 + x3_categories + x4 + x5)


head(yeni_regresyon)

#burada bize dummy kodlaması ile kategorik değişkenlere ait A, B,C, D olmak 4 farklı kategori verdi
#regresyon modelini oluştururken bunları ayrı ayrı modele input etmemiz gerekir

reg.model<- lm(y ~ x1 + x2 + x3_categories_A + x3_categories_B+ x3_categories_C+ x3_categories_D + x4 + x5, data = yeni_regresyon)

#burada yaptığım işlem ile artık dummy kodlaması olan kolonların yer aldığı modelden veriyi çekerek bir lm oluşturdum.

# Bağımsız değişkenlerin grafiğini çizme
# Bağımsız değişkenlerin grafiğini çizme
par(mfrow=c(3,2)) # Grafiklerin yan yana yerleştirilmesi için ayar
plot(x1, y, main = "x1 vs. y", xlab = "x1", ylab = "y", col = "blue", pch = 16)
plot(x2, y, main = "x2 vs. y", xlab = "x2", ylab = "y", col = "red", pch = 16)
plot(x3, y, main = "x3 vs. y", xlab = "x3", ylab = "y", col = "purple", pch = 16)
plot(x4, y, main = "x4 vs. y", xlab = "x4", ylab = "y", col = "green", pch = 16)
plot(x5, y, main = "x5 vs. y", xlab = "x5", ylab = "y", col = "yellow", pch = 16)


#veriyi standarlaştırma

x1_standardized <- scale(x1)
x2_standardized <- scale(x2)
x3_standardized <- scale(x3)
x4_standardized <- scale(x4)
x5_standardized <- scale(x5)
y_standardized <- scale(y)

# Standartlaştırılmış veriyi kontrol etme
summary(x1_standardized)
summary(x2_standardized)
summary(x3_standardized)
summary(y_standardized)

# Veriyi test ve eğitim alt kümelerine bölme
install.packages("caTools")
library(caTools)
split <- sample.split(y, SplitRatio = 0.7) # 70% eğitim, 30% test
train_data <- subset(data.frame(x1, x2, x3,x4,x5, y), split == TRUE)
test_data <- subset(data.frame(x1, x2, x3,x4,x5, y), split == FALSE)


# Verinin boyutlarını kontrol etme
dim(train_data)
dim(test_data)

#import veri üzerinde yeni EDA 

library(readr)
library(readxl)
library(googlesheets4)

getwd()
setwd("C://Users/medda/OneDrive/Belgeler/datasets")

#reading csv file

data <- read.csv("crop.csv")
head(data)

summary(data)
dim(data)

unique_values <- unique(data$label)
print(unique_values)



#eksik verilerin kontrol edilmesi
any_missing <- apply(data, 2, function(x) any(is.na(x)))
missing_columns <- names(data)[any_missing]
print(missing_columns)

#x ve X.1 sütunlarının yer almadığı yeni dataframe oluşturuyorum

clean_data <- subset(data, select = -c(X, X.1))

head(clean_data)
dim(clean_data)


#kategorik değişken için frekans analizi yapıyorum
table(clean_data$label)

#başka yöntem
library(dplyr)
clean_data %>% count(label)

#bir diğer yöntem
summary(clean_data$label)
#buradan gördüğümüz üzere her bir mahsül çeşidi için eşit miktarda örneklem alınmış

#burada tarım ürünlerinin ihtiyaç durumlarına göre çeşitli analizler yapabiliriz. 
#Örneğin nitrojen ihtiyacını kendi içinde ayrı bir tablo haline getirebilirim

# Veriyi işleme
nitrogen_data <- aggregate(Nitrogen ~ label, data = clean_data, FUN = function(x) c(mean = mean(x), sum = sum(x), max = max(x)))


summary(nitrogen_data)


# Histogram oluşturma
ggplot(clean_data, aes(x = Nitrogen)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(x = "Nitrogen", y = "Frekans", title = "Nitrogen Dağılımı") +
  theme_minimal()
#şimdi burada öncelikle nitrogen ihtiyaçlarına baktık. Bitkilerin büyük bir bölümü
#50 seviyesinin altında yığılmış. Bir kısmı da yüksek nitrojen seviyelerine
#ihtiyaç duyabiliyormuş ama bunların sayısı daha az. Bunları boxplot ile tekrar kontrol edelim:

# Etiketleri üst üste binmemesi için yazıları yan çevirerek boxplot oluşturma theme() kullanıyorum

ggplot(clean_data, aes(x = label, y = Nitrogen, fill = label)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Crop Type", y = "Nitrogen", title = "Crop Nitrogen Levels by Type")


#burada hangi bitkilerin yüksek nitrojen seviyelerine ihtiyaç duyduğunu hızlıca görüntüleyebiliyorum.
#örneğin en yüksek cotton yani pamuk dikkati çekiyor. Yine kahve de dikkati çeken ürünlerden.
#bu analizi her değişken için gerçekleştiriyorum:

ggplot(clean_data, aes(x = phosphorus)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(x = "Phosphorus", y = "Frekans", title = "Fosfor Dağılımı") +
  theme_minimal()


ggplot(clean_data, aes(x = phosphorus)) +
  geom_density(fill = "purple", alpha = 0.5) +
  labs(x = "Phosphorus", y = "Density", title = "Phosphorus Distribution") +
  theme_minimal()





ggplot(clean_data, aes(x = label, y = phosphorus, fill=label)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Crop Type", y = "phosphorus", title = "Crop Phosphorus Levels by Type")

#elma ve üzüm fosfor ihtiyacı bakımından en yüksek seviyelere ihtiyaç duyan ürünlermiş.

#potassium
ggplot(clean_data, aes(x = potassium)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(x = "potassium", y = "Frekans", title = "Potasyum Dağılımı") +
  theme_minimal()

ggplot(clean_data, aes(x = label, y = potassium,fill = label)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Crop Type", y = "potassium", title = "Crop Potassium Levels by Type")


#yine elma ve üzüm aynı zamanda yüksek potasyum seviyelerine ihtiyaç duyuyormuş.


#temperature
ggplot(clean_data, aes(x = temperature)) +
  geom_histogram(binwidth = 5, fill = "lightsalmon", color = "black") +
  labs(x = "temperature", y = "Frekans", title = "Sıcaklık Dağılımı") +
  theme_minimal()



ggplot(clean_data, aes(x = label, y = temperature,fill = label)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Crop Type", y = "temperature", title = "Crop Temperature Need Levels by Type")

#burada ise sıcaklık dağılım aralıkları ile güzel bilgiler görebiliyoruz.
#Örneğin elma için sıcaklık aralığı çok dar iken;üzüm, portakal, papaya gibi ürünlerin daha geniş bir sıcaklık
#aralığını tolere edebildiğini görüyoruz. 

#humidity

ggplot(clean_data, aes(x = humidity)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  labs(x = "humidity", y = "Density", title = "Humidity Distribution") +
  theme_minimal()



ggplot(clean_data, aes(x = label, y = humidity, fill=label)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Crop Type", y = "humidity", title = "Crop Humidity Need Levels by Type")

#ph ölçümlerini daha iyi ayırt etmek için bindwith 1 ayarlıyorum
ggplot(clean_data, aes(x = ph)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(x = "ph", y = "Frekans", title = "Ph Dağılımı") +
  theme_minimal()

ggplot(clean_data, aes(x = label, y = ph, fill = label)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Crop Type", y = "ph", title = "Crop PH Need Levels by Type")

#rainfall
ggplot(clean_data, aes(x = rainfall)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(x = "rainfall", y = "Frekans", title = "Yağış Dağılımı") +
  theme_minimal()
ggplot(clean_data, aes(x = label, y = rainfall, fill = label)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Crop Type", y = "rainfall", title = "Crop Rainfall Levels by Type")


#label değerlerinin olmadığı sadece numerik değerlerin olduğu bir dataframe oluşturuyorum

data_numeric <- subset(clean_data, select = -c(label))
# Eksik verilerin sayısını bulma
missing_values_numeric <- sum(is.na(data_numeric))
print(missing_values_numeric)


# clean_data veri çerçevesinden label sütununu çıkararak numerik sütunları seçme
data_numeric <- subset(clean_data, select = -c(label))

#outlier nitrogen
Q1_nitrogen <- quantile(clean_data$Nitrogen, 0.25)
Q3_nitrogen <- quantile(clean_data$Nitrogen, 0.75)

IQR_nitrogen <- Q3_nitrogen - Q1_nitrogen
lower_bound_nitrogen  <- Q1_nitrogen - 1.5 * IQR_nitrogen
upper_bound_nitrogen  <- Q3_nitrogen + 1.5 * IQR_nitrogen

outliers_nitrogen  <- clean_data$Nitrogen < lower_bound_nitrogen | clean_data$Nitrogen > upper_bound_nitrogen 

summary(outliers_nitrogen)

#nitrogen için outlier tespit edilmedi

boxplot(clean_data$Nitrogen)

outlier_values_nitrogen <- clean_data[outliers_nitrogen, ]
print(outlier_values_nitrogen)

#outlier phosphorus
Q1_phosphorus <- quantile(clean_data$phosphorus, 0.25)
Q3_phosphorus <- quantile(clean_data$phosphorus, 0.75)

IQR_phosphorus <- Q3_phosphorus - Q1_phosphorus
lower_bound_phosphorus  <- Q1_phosphorus - 1.5 * IQR_phosphorus
upper_bound_phosphorus  <- Q3_phosphorus + 1.5 * IQR_phosphorus

outliers_phosphorus  <- clean_data$phosphorus < lower_bound_phosphorus | clean_data$phosphorus > upper_bound_phosphorus

summary(outliers_phosphorus)

boxplot(clean_data$phosphorus)

#burada outlier olarak tespit edilenler normalin üzerinde fosfora ihtiyaç duyan ürünler aslında. 
#normal aralıkların dışında ihtiyaç duyan ürünlerin verisini tespit edebiliyoruz.örneğin fosfor ihtiyacı normal 
#aralığın dışında olan ve  outlier olarak tespit edilen verilere bakalım:

outlier_values_phosphorus <- clean_data[outliers_phosphorus, ]
print(outlier_values_phosphorus)

#elma ve üzüm olarak geliyor bunu zaten fosfor değişkeninin boxplot grafiğinde de daha önce tespit etmiştik.
#bunlar aslında veri olarak tekil birer outlier değildir ancak kategorik olarak outlier bir davranış sergilemektedirler.

continuous_cropdata <- clean_data[, c("Nitrogen", "phosphorus", "potassium", "temperature", "humidity","ph","rainfall")]

# Korelasyon katsayılarını hesaplama
correlation_matrix_crop <- cor(continuous_cropdata)

# Korelasyon matrisini yazdırma ve görselleştirme
print(correlation_matrix_crop)
library(corrplot)
corrplot(correlation_matrix_crop, method = "color")

#bağımsız değişkenler arasında anlamlı bir korelasyon tespit edilmedi

install.packages("caTools")
library(caTools)

y_crop <-  clean_data$label
x1_c <- clean_data$Nitrogen
x2_c <- clean_data$phosphorus
x3_c <- clean_data$potassium
x4_c <- clean_data$temperature     
x5_c <- clean_data$humidity
x6_c <- clean_data$ph
x7_c <- clean_data$rainfall

crop_model <- lm (x1_c ~ x2_c+x3_c+x4_c+x5_c+x6_c+x7_c)

summary(crop_model)

###Call:
###lm(formula = x1_c ~ x2_c + x3_c + x4_c + x5_c + x6_c + x7_c)

###Residuals:
###  Min      1Q  Median      3Q     Max 
###-69.390 -27.515  -5.147  28.174  86.592 

###Coefficients:
###  Estimate Std. Error t value Pr(>|t|)    
###(Intercept) 23.30364    8.46687   2.752 0.005966 ** 
###  x2_c        -0.19353    0.03678  -5.262 1.56e-07 ***
###  x3_c        -0.02929    0.02485  -1.179 0.238642    
###  x4_c        -0.27119    0.15657  -1.732 0.083399 .  
###  x5_c         0.30302    0.03902   7.765 1.24e-14 ***
###  x6_c         3.37392    0.99668   3.385 0.000724 ***
###  x7_c         0.02363    0.01394   1.695 0.090230 .  
###---
###  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

###Residual standard error: 35.3 on 2193 degrees of freedom
###Multiple R-squared:  0.08844,	Adjusted R-squared:  0.08595 
###F-statistic: 35.46 on 6 and 2193 DF,  p-value: < 2.2e-16
###

#burada diğer değişkenlerin nitrojen seviyelerini yordama katsayılarını görüntülüyebiliyorum
#bazı değişkenler anlamlı yordayıcı olsalar da modelin kendisi anlamlı bir model değil r2 değeri oldukça düşük
#%8 lik bir yordama kapasitesine sahip ve p value anlamlı değil. regresyon analizinde daha detaylı bunları analiz edebiliriz


split <- sample.split(y_crop, SplitRatio = 0.7) # 70% eğitim, 30% test
train_data_crop <- subset(data.frame(x1_c, x2_c, x3_c,x4_c,x5_c,x6_c,x7_c,y_crop), split == TRUE)
test_data_crop <- subset(data.frame(x1_c, x2_c, x3_c,x4_c,x5_c,x6_c,x7_c,y_crop), split == FALSE)

# Veriyi bağımlı ve bağımsız değişkenlere ayırma
features <- c("Nitrogen", "phosphorus", "potassium", "temperature", "humidity", "ph", "rainfall")
X <- clean_data[, features]
y <- clean_data$label

# Model oluşturma (örneğin, destek vektör makineleri kullanarak)
library(e1071)
# Load required library
library(nnet)


clean_data$label <- as.factor(clean_data$label)

# Fit multinomial logistic regression model
model <- multinom(label ~ ., data = clean_data)

# View model summary
summary(model)

#son olarak tüm kategorik değişkenlerin summarysini alacak fonksiyon yazmak istersek cat() fonksiyonu
# döngüden seçeceğimiz her unique label için ayrı summary alabiliyoruz

for (crop in unique(clean_data$label)) {
  cat(paste("DataFrame for", crop, ":\n"))
  print(summary(subset(clean_data, label == crop)))
}
#daha önceden her crop için 100 gözlem olduğunu biliyorduk ancak bu farklı olsaydı yine aynı fonskyion ve
#cat() yöntemi ile yine bunları table() ile kategorilendirip sayabilirdik
for (crop in unique(clean_data$label)) {
  cat(paste("Value counts for", crop, ":\n"))
  print(table(clean_data$label == crop))
}

################################################################################

####################
### Kütüphaneler ###
####################

library(readr)
library(cluster)
library(clusterSim)
library(factoextra)
library(corrplot)
library(stats)
library(clustertend)
library(psych)
library(NbClust)
library(clValid)
library(plotly)
library(fpc)
library(mclust)
library(dbscan)
library(tidyverse)
library(ggpubr)

################################################################################

###################
### Data Import ###
###################

# dataset
wdbc <- read_csv("wdbc.data", col_names = FALSE)

wdbc <- wdbc[1:12]

# changing colnames
colnames <- c("ID", "Diagnosis", "radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave.points", "symmetry", "fractal.dimension" )
names(wdbc) <- colnames

#removing label column
data <- wdbc[3:12]


################################################################################

##############################
### Descriptive Statistics ###
##############################


# getting descriptive statistics for each variable
summary(data)

# When we check the descriptive statistics of the dataset:
# Mean of the radius variable is higher then than the median. This means that radius variable is right-skewed. When we compare the values of the third quantile and maximum value, it can be referred that there might be some outliers in the variable. However, it is best to make the final judgement with boxplot analysis.
# Mean of the texture variable is higher then than the median. This means that texture variable is right-skewed. When we compare the values of the third quantile and maximum value, it can be referred that there might be some outliers in the variable. However, it is best to make the final judgement with boxplot analysis.
# Mean of the perimeter variable is higher then than the median. This means that perimeter variable is right-skewed. When we compare both the values of the third quantile and maximum value, and min and first quantile value, it can be referred that there might be some outliers in the variable. However, it is best to make the final judgement with boxplot analysis.


# Radius ve perimeter değişkeninde de Area gibi aykırı değerler gözlemlenebilir. 

round(apply(data, 2, sd), 3)

#Standart sapmalar incelendiğinde bazı değişkenlerin standart sapmalarının fazla olduğu çıkarımında bulunulabilir. 
# Standart sapması en çok olan Area iken en az olan ise fractal dimension olarak saptanmıştır.


################################################################################

###################
### Korrelasyon ###
###################

corr <- cor(data, method = "pearson")
corrplot(corr, method="color" )

# Veri setindeki korrelasyon matrisi incelendiğinde, değişkenler arasındaki korrelasyonun oldukça fazla olduğu gözlemlenmiştir.
# Radius, perimeter ve area değişkenlerinin birbirleri ile korrelasyonları incelendiğinde 0.98'den fazla değerler görünüyor.
# Compactness, concavity ve concave points değişkenleri arasındaki korrelasyon de 0.83 ve üzeri.
# Fractal Dimension değişkeni negatif korrelasyonu olan tek değişken. Radius, Perimeter ve Area değişkenleri ile -0.31'den daha fazla değerlerle korrelasyona sahip.
# Texture değişkeninin diğer tüm değişkenlerle korrelasyonu sıfıra oldukça yakın. Bu da oldukça ilginç çünkü diğer tüm değişkenlerde negatif veya pozitif korrelasyon bulunmakta. Hatta bazı değişkenler arasındaki korrelasyonlar yukarıda belirtildiği gibi oldukça fazla.

################################################################################

#######################
### Kutu Grafikleri ###
#######################

par(mfrow=c(2,5))
boxplot(data$radius, xlab = "radius")
boxplot(data$texture, xlab = "texture")
boxplot(data$perimeter, xlab = "perimeter")
boxplot(data$area, xlab = "area")
boxplot(data$smoothness, xlab = "smoothness")
boxplot(data$compactness, xlab = "compactness")
boxplot(data$concavity, xlab = "concavity")
boxplot(data$concave.points, xlab = "concave points")
boxplot(data$symmetry , xlab = "symmetriy")
boxplot(data$fractal.dimension , xlab = "fractal dimension")
dev.off()

# Area değişkeni sağa çarpık, Radius değişkeni ise sola çarpık görülmekte.
# Concave points ve concavity'nin normal dağılmadığı gözlemlenmekte. 
# Aykırı değerlerin fazlalığı bariz bir şekilde görülmekte.
# Varyans oldukça fazla görülüyor.

################################################################################

### Temel Bileşen Analizi ###

data.pca <- prcomp(data, center = TRUE, scale. = TRUE)
summary(data.pca)

# Temel bileşen analizinin özet değerleri incelendiğinde en uygun bileşen sayısının iki olabileceği düşünülebilir.
# 2 bileşendeki 0.7997 açıklayıcılığın, 3 bileşende 0.887'e çıkması göz ardı edilebilecek bir yükseliş.
# Standart deviation değeri incelendiğinde 0.88 yine, en uygun değer olarak görülmektedir. 

data.eigen <- eigen(corr)
eigenvalues <- data.eigen$value
eigenvalues

# Eigen değerleri incelendiğinde de 1'e en yakın değer olan 3 bileşenin 0.8806 ile en uygun seçim olduğu söylenebilir. 

fviz_eig(data.pca)

# Screeplot da incelendiğinde 3 bileşen en uygun bileşen sayısı olarak görülmekte. 

# Bileşenlerin hangi değişkenleri ifade ettikleri incelenmek istenilmiştir. İlk adımda üç bileşene bakılacaktır. 

data.pca$rotation[,1:3]

# Bileşenlerin hangi değişkenleri ifade ettiklerini içeren çıktı incelendiğinde; üçüncü bileşenin yalnızca Texture değişkenini ifade ettiği fark edilmiştir.
# Texture değişkeninin aykırılığını korrelasyon analizinde de görmek mümkündü. Hiçibr değişken ile anlamlı bir korelasyonu olmayan bu değişkenin bir başka bileşen ile ifade edilmesi anlaşılabilir.
# Eğer bileşen sayısı iki olarak seçilecek olursa 2 bileşenin Texture değişkenini iyi ifade edemediği de gözlemlenmiştir.
# Tek bir değişken için bir bileşen eklemek, 0.9 açıklayıcılık maaliyeti ile düşünüldüğünde çok da mantıklı görülmemiştir. 
# Bu karara varmak için Texture değişkeninin diğer değişkenlerle korelasyonunun azlığı da hesaba katılmıştır.

# Bu kararların kontrolü için bileşen sayısına otomatik olarak karar veren Psych paketi de kullanılmak istenilmiştir.
# Buna ek olarak bileşenlerin hangi değişkenleri ifade ettiğine dair güzel bir görsel sunan bu paketten yararlanılmak istenilmiştir.

x <- fa.parallel(data, fm="pa", fa="both", n.iter=1)

# Bu paketin de en uygun bileşen sayısı olarak iki bileşeni seçmesi ile, iki bileşen nihai karar olarak kesinleşmiştir.

fit <- principal(data, x$ncomp, rotate="varimax") 
fa.diagram(fit)

# Bu görselden de görülebileceği üzere bileşenlerin ifade ettikleri değişkenler şu şekildedir:
# PC1 : Radius, Perimeter, Area, Concave Points, Concavity, Texture
# PC2 : Fractal Dimension, Smoothness, Compactness, Symmetry


fviz_pca_ind(data.pca,
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE   )

# Gözlemlerin PC 1 ve PC 2 grafiğindeki katkıları incelendiğinde sağ üst ve sağ altta bir kısımda kümelenme görülmekte. 
# Bu gözlemlerin benzer özellikler ifade ettiği söylenebilir.
# Örneğin en sol altta bulunan 79. gözlem değerleri incelendiğinde Radius ve Texture dışındaki bütün değişkenler için maksimuma yakın değerlere sahip olduğu görülebiliyor. 
# Tam zıt ekseninde yer alan 569. gözlem değerleri incelendiğinde, Smoothness ve Concavity'de minimum değerleri sahipken, Texture değişkeninde ise 3. kartilin üzerinde bir değere sahip.



fviz_pca_var(data.pca,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     )


# Değişkenlerin PCA grafiği incelendiğinde, pozitif korrelasyona sahip gözlemlerin aynı bölgeleri işaret ettiği söylenebilir.
# Area, aynı bileşende olduğu Perimeter ile pozitif korelasyona sahipken; farklı bileşende olduğu fractal dimension ile negatif korrelasyona sahip. 
# Değişkenlerin katkıları bu grafik sayesinde daha iyi görülebilir.

cor(data.pca$x[,1],data.pca$x[,2])

# PCA sonrası değişkenler arasında bir korelasyon kalmadığı saptanılmıştır. 

####################################################################################################
##################################### Kümeleme Analizleri ##########################################
####################################################################################################

# Veri setindeki korrelasyon oldukça fazla olduğu için analizde PCA yapılmış veri seti kullanılacaktır.
pcadata <- predict(data.pca)[,1:2]

######################################
### Kümelenme Eğiliminin Ölçülmesi ###
######################################

hopkins.data <- hopkins(pcadata, n = nrow(pcadata)-1)
hopkins.data

# Hopkins istatistiği, belirli bir veri setinin tekdüze dağılımdan üretilme olasılığını ölçerek veri kümesinin kümelenme eğilimini değerlendirmek için kullanılır.
# Bu veri seti için hopkins değeri 0.1908455 olarak çıktı. Bu da veri setinin kümelenebilir olduğunu gösteriyor.
#######################
### K - Ortalamalar ###
#######################

###################################
### Küme sayısının belirlenmesi ###
###################################

kmwss <- fviz_nbclust(pcadata , kmeans, nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Dirsek Yöntemi") 
kmsil <- fviz_nbclust(pcadata, kmeans ,method = "silhouette")+
  labs(subtitle = "Silhouette Grafiği") 
kmgap <- fviz_nbclust(pcadata, kmeans ,nstart = 25, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap Grafiği") 

ggarrange(kmwss, kmsil, kmgap, ncol = 1,
          nrow = 3)

# Dirsek Yöntemi grafiği incelendiğinde küme sayısı için kesin bir karar vermenin mümkün olmadığı söylenebilmekle birlikte, iki küme seçilebilir.
# Silhouette grafiği incelendiğinde en yüksek silhouette değerinin ikide olduğu gözlemlenebilir. Bununla birlikte 3 küme de arada çok fark olmaması sebebi ile denenebilir.
# Gap Statistics değeri de en uygun küme sayısının iki olduğunu işaret ediyor.

nb <- NbClust(pcadata , distance = "euclidean", min.nc = 2,
              max.nc = 9, method = "kmeans")
fviz_nbclust(nb) + labs(subtitle ="NbClust Grafiği")

# NbClust paketi çıktısı incelendiğinde 9 yöntemin 2 kümeyi önerdiği, 6 yöntemin ise 3 kümeyi önerdiği saptanılmıştır. 
# Bu sebeple her iki küme sayısının da denenmesine karar verilmiştir.

#####################################
### İki Küme İçin K - Ortalamalar ###
#####################################

set.seed(1993)
km_data <- kmeans(pcadata, 2, nstart=25) 
print(km_data) 

# Birinci kümede 398 eleman varken; ikinci kümedeki eleman sayısı 171.
# Sırasıyla 1. ve 2. kümeler için küme içi kareler toplamı ise 1121.768 ve 1216.540 olarak saptanmıştır.
# Açıklayıcılık ise %48.5 olarak saptanılmıştır.

fviz_cluster(km_data, data = pcadata,
             ellipse.type = "convex", 
             star.plot = TRUE, 
             repel = TRUE, 
             ggtheme = theme_minimal()
) + labs(subtitle = "İki boyutlu Küme Grafiği")


# Küme grafiği incelendiğinde çakışma olduğu gözlemlenebilir.
# Ayrışma yalnızca PC1'de gerçekleşmiş. 
# Özellikle mavi renkle ifade edilen ikinci kümenin varyansının oldukça fazla olduğu söylenebilir.

###############################################
### 2 Küme için K - Ortalamalar Geçerliliği ###
###############################################

# Silhouette

k2m_data <- eclust(pcadata, "kmeans", k = 2, nstart = 25, graph = F)
fviz_silhouette(k2m_data, palette = "jco",
                ggtheme = theme_classic())

Her bir gözlemin silhouette değerlerini içeren grafik incelendiğinde, mavi renkle gösterilen birinci kümedeki bazı gözlemlerin negatif değerler aldıkları görülmektedir.
Ortalama silhouette değeri ise 0.49 olarak gözlemlenmiştir. 

sil <- k2m_data$silinfo$widths[, 1:3]
neg_sil_index <- which(sil[, 'sil_width']<0)
sil[neg_sil_index, , drop = FALSE]

# Negatif silhouette değeri olan gözlem sayısı 10 olarak saptanılmıştır. 
# Gözlemlerin negatif silhouette değerlerine sahip olması kümelemenin geçerliliğinin sorgulanması için önemli bir ölçüttür.

# Dunn Indexi 

km_stats <- cluster.stats(dist(pcadata), k2m_data$cluster)
km_stats$dunn

# Dunn endeksi sıfırdan maksimuma kadar uzanan değerler alır. En iyi dunn değeri maksimum değerdir. 
# Kümeleme sonucunda karşılaştığımız değer ise sıfıra oldukça yakın (0.005768501). Bu da kümelemenin başarısını sorgular.
# Daha sonra bu değerler karşılaştırılacaktır.

# Connectivity

connectivity(distance = NULL, k2m_data$cluster, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# Connectivity değeri 0’dan sonsuza kadar giden değerler alır. Mümkün olduğunca küçük olmalıdır. 
# Bu kümemele için 64.96498 değerini almıştır. 
# Diğer kümeleme sonuçları ile karşılaştırılacaktır.

# Orijinal Veri İle Karşılaştırma

table(wdbc$Diagnosis, k2m_data$cluster)

# Orijinal verideki label ile kümemele sonuçları karşılaştırıldığında kümelemenin bire bir başarılı olduğu söylenememektedir.

# Harici Kümeleme Geçerliliği Ölçütleri 

# Bölümlenmiş küme ile harici referans değeri arasında uyum niceliği olarak düzeltilmiş Rand İndeksi ve Meila değişim indeksi VI değerleri kullanılabilir.
# -1 (uyum yok) ile 1 (mükemmel uyum) arasında değer alır.

diagnosis <- ifelse(wdbc$Diagnosis == "M",1,2 )
cluster.stats(d = dist(data),diagnosis, k2m_data$cluster)$corrected.rand

#Label ile veri seti arasındaki Rand değeri 0.6465881 olarak saptanılmıştır. Diğer değerlerle karşılaştırılacaktır.

cluster.stats(d = dist(data),diagnosis, k2m_data$cluster)$vi

# Label ile veri seti arasında VI değeri 0.5687046 olarak saptanmıştır. Diğer değerlerle karşılaştırılacaktır.

####################################
### Üç Küme için K - Ortalamalar ###
####################################

set.seed(1993)
k3m_data <- kmeans(pcadata, 7, nstart=25) 
print(k3m_data) 

# 1, 2 ve 3. kümeler için küme eleman sayıları sırasıyla 117, 117 ve 335 olarak saptanmıştır.
# Sırasıyla 1, 2 ve 3. kümeler için küme içi kareler toplamı ise 620.5682, 455.6488 ve 634.0446 olarak saptanmıştır.
# Açıklayıcılık ise %62.3 olarak saptanılmıştır.


fviz_cluster(k3m_data, data = pcadata,
             ellipse.type = "convex", 
             star.plot = TRUE, 
             repel = TRUE, 
             ggtheme = theme_minimal()
) + labs(subtitle = "İki boyutlu Küme Grafiği")

# Küme grafiği incelendiğinde çakışma olmadığı gözlemlenebilir.
# Ayrışma hem PC1, hem de PC2'de gerçekleşmiş. 
# Özellikle mavi renkle ifade edilen üçüncü kümenin varyansının diğer kümelere kıyasla daha az olduğu söylenebilir.


###############################################
### 3 Küme için K - Ortalamalar Geçerliliği ###
###############################################

# Silhouette

km3_data <- eclust(pcadata, "kmeans", k = 7, nstart = 25, graph = F)
fviz_silhouette(km3_data, palette = "jco",
                ggtheme = theme_classic())

# Her bir gözlemin silhouette değerlerini içeren grafik incelendiğinde, mavi renkle gösterilen birinci kümede ile sayı renkle gösterilen ikinci kümede bazı gözlemlerin negatif değerler aldıkları görülmektedir.
# Ortalama silhouette değeri ise 0.44 olarak gözlemlenmiştir. 

sil <- km3_data$silinfo$widths[, 1:3]
neg_sil_index <- which(sil[, 'sil_width']<0)
sil[neg_sil_index, , drop = FALSE]

# Negatif silhouette değeri olan gözlem sayısı 9 ( Birinic kümede 1, İkinci kümede 8) olarak saptanılmıştır. 
# Gözlemlerin negatif silhouette değerlerine sahip olması kümelemenin geçerliliğinin sorgulanması için önemli bir ölçüttür.

# Dunn Indexi 

k3m_stats <- cluster.stats(dist(pcadata), km3_data$cluster)
k3m_stats$dunn

# Dunn endeksi sıfırdan maksimuma kadar uzanan değerler alır. En iyi dunn değeri maksimum değerdir. 
# Kümeleme sonucunda karşılaştığımız değer ise sıfıra oldukça yakın (0.01145259). 
# Daha sonra bu değerler karşılaştırılacaktır.

# Connectivity

connectivity(distance = NULL, km3_data$cluster, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# Connectivity değeri 0’dan sonsuza kadar giden değerler alır. Mümkün olduğunca küçük olmalıdır. 
# Bu kümemele için 87.85166 değerini almıştır. 
# Diğer kümeleme sonuçları ile karşılaştırılacaktır.

# Orijinal Veri İle Karşılaştırma

table(wdbc$Diagnosis, km3_data$cluster)

# Orijinal verideki label ile kümemele sonuçları karşılaştırıldığında farklılıklar dikkat çekmetedir. Özellikle birinci kümede hiç B olmaması dikkat çekmiştir.

# Harici Kümeleme Geçerliliği Ölçütleri 

# Bölümlenmiş küme ile harici referans değeri arasında uyum niceliği olarak düzeltilmiş Rand İndeksi ve Meila değişim indeksi VI değerleri kullanılabilir.
# -1 (uyum yok) ile 1 (mükemmel uyum) arasında değer alır.

diagnosis <- ifelse(wdbc$Diagnosis == "M",1,2 )
cluster.stats(d = dist(data),diagnosis, km3_data$cluster)$corrected.rand

#Label ile veri seti arasındaki Rand değeri 0.4985402 olarak saptanılmıştır. Diğer değerlerle karşılaştırılacaktır.

cluster.stats(d = dist(data),diagnosis, km3_data$cluster)$vi

# Label ile veri seti arasında VI değeri 0.9305164 olarak saptanmıştır. Diğer değerlerle karşılaştırılacaktır.

###################
### K - Medoids ###
###################

#############################################
### Optimal Küme Sayısına Karar Verilmesi ###
#############################################

pamwss <- fviz_nbclust(pcadata , pam, method = "wss") +
  labs(subtitle = "Dirsek Yöntemi") 
pamsil <- fviz_nbclust(pcadata, pam ,method = "silhouette")+
  labs(subtitle = "Silhouette Grafiği") 
pamgap <- fviz_nbclust(pcadata, pam, method = "gap")+
  labs(subtitle = "Gap Grafiği") 

ggarrange(pamwss, pamsil, pamgap, ncol = 1,
          nrow = 3)
#Üç grafik de incelendiğinde en optimal küme sayısının 2 olduğu kararına varılmıştır.

############################
### K2- Medoids Kümeleme ###
############################

set.seed(1993)
pam_data <- pam(pcadata,2)
print(pam_data)
pam_data$clustering

# Küme elemanlarına bakıldığında birinci kümede 499 eleman varken; ikinci küme eleman sayısı 269 olarak görülmekte.

fviz_cluster(pam_data,
             ellipse.type = "convex", 
             repel = TRUE, 
             ggtheme = theme_classic()
)

# İki ve üç boyutlu grafikler incelendiğinde çakışma gözlemlenmemiştir.
# Tıpkı k-ortalamalarda olduğu gibi; ayrışmanın yalnızca PC1 boyutunda gerçekleştiği görülmektedir.
# Kırmızı renk ile gösterilen birinci kümedeki varyans fazla görünüyor. 

################################
### K2 - Medoids Geçerliliği ###
################################

# Silhouette

p2m_data <- eclust(pcadata, "pam", k = 2, nstart = 25, graph = F)
fviz_silhouette(p2m_data, palette = "jco",
                ggtheme = theme_classic())

# Her bir gözlemin silhouette değerlerini içeren grafik incelendiğinde, mavi renkle gösterilen birinci kümedeki bazı gözlemlerin negatif değerler aldıkları görülmektedir.
# Ortalama silhouette değeri ise 0.48 olarak gözlemlenmiştir. 

silp <- p2m_data$silinfo$widths[, 1:3]
neg_silp_index <- which(silp[, 'sil_width']<0)
silp[neg_silp_index, , drop = FALSE]

# Negatif silhouette değeri olan gözlem sayısı 26 olarak saptanılmıştır. 
# Gözlemlerin negatif silhouette değerlerine sahip olması kümelemenin geçerliliğinin sorgulanması için önemli bir ölçüttür.

# Dunn Indexi 

pam_stats <- cluster.stats(dist(pcadata), p2m_data$cluster)
pam_stats$dunn

# Kümeleme sonucunda karşılaştığımız  dunn değeri sıfıra oldukça yakın ( 0.01328364 ). 

# Connectivity

connectivity(distance = NULL, p2m_data$cluster, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# K-medoids için connectivity değer 50.08507 olarak saptanmıştır. 
# Diğer kümeleme sonuçları ile karşılaştırılacaktır.

# Orijinal Veri İle Karşılaştırma

table(wdbc$Diagnosis, p2m_data$cluster)

# Orijinal verideki label ile kümemele sonuçları karşılaştırıldığında kümelemenin bire bir başarılı olduğu söylenememektedir.
# Yine de k-ortalamara kıyasla daha iyi bir sonuç verdiği söylenilebilir. 

# Harici Kümeleme Geçerliliği Ölçütleri 

# Bölümlenmiş küme ile harici referans değeri arasında uyum niceliği olarak düzeltilmiş Rand İndeksi ve Meila değişim indeksi VI değerleri kullanılabilir.
# -1 (uyum yok) ile 1 (mükemmel uyum) arasında değer alır.

diagnosis <- ifelse(wdbc$Diagnosis == "M",1,2 )
cluster.stats(d = dist(data),diagnosis, p2m_data$cluster)$corrected.rand

#Label ile veri seti arasındaki Rand değeri 0.72 olarak saptanılmıştır. Diğer değerlerle karşılaştırılacaktır.

cluster.stats(d = dist(data),diagnosis, p2m_data$cluster)$vi

# Label ile veri seti arasında VI değeri 0.49 olarak saptanmıştır. Diğer değerlerle karşılaştırılacaktır.

############################
### K3- Medoids Kümeleme ###
############################


set.seed(1993)
pam3_data <- pam(pcadata,5)
print(pam3_data)
pam3_data$clusinfo

# Küme elemanlarına bakıldığında birinci kümede 159, ikinci küme eleman sayısı 169, üçünkü kümede ise 241 eleman bulunmaktadır.

fviz_cluster(pam3_data,
             ellipse.type = "convex", 
             repel = TRUE, 
             ggtheme = theme_classic()
)

# Küme grafiği incelendiğinde çakışma gözlemlenmiştir.
# Ayrışmanın hem PC1, hem de PC2 boyutunda gerçekleştiği görülmektedir.
# Kırmızı renk ile gösterilen birinci kümedeki varyans fazla iken; mavi renkle ifade edilen üçüncü kümedeki varyans azdır. 

################################
### K3 - Medoids Geçerliliği ###
################################

# Silhouette

p3m_data <- eclust(pcadata, "pam", k = 5, nstart = 25, graph = F)
fviz_silhouette(p3m_data, palette = "jco",
                ggtheme = theme_classic())

# Her bir gözlemin silhouette değerlerini içeren grafik incelendiğinde, mavi renkle gösterilen birinci küme ile sarı renkle gösterilen ikinci kümedeki bazı gözlemlerin negatif değerler aldıkları görülmektedir.
# Ortalama silhouette değeri ise 0.36 olarak gözlemlenmiştir. 

silp3 <- p3m_data$silinfo$widths[, 1:3]
neg_silp3_index <- which(silp3[, 'sil_width']<0)
silp3[neg_silp3_index, , drop = FALSE]

# Negatif silhouette değeri olan gözlemlerin 26 tanesi ikinci kümede iken, 21 tanesi birinci kümededir. 
# Gözlemlerin negatif silhouette değerlerine sahip olması kümelemenin geçerliliğinin sorgulanması için önemli bir ölçüttür.

# Dunn Indexi 

pam3_stats <- cluster.stats(dist(pcadata), p3m_data$cluster)
pam3_stats$dunn

# Kümeleme sonucunda karşılaştığımız  dunn değeri sıfıra oldukça yakın ( 0.004579698 ). 

# Connectivity

connectivity(distance = NULL, p3m_data$cluster, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# K-medoids için connectivity değer 109.0265 olarak saptanmıştır. 
# Diğer kümeleme sonuçları ile karşılaştırılacaktır.

# Orijinal Veri İle Karşılaştırma

table(wdbc$Diagnosis, p3m_data$cluster)

# Harici Kümeleme Geçerliliği Ölçütleri 

# Bölümlenmiş küme ile harici referans değeri arasında uyum niceliği olarak düzeltilmiş Rand İndeksi ve Meila değişim indeksi VI değerleri kullanılabilir.
# -1 (uyum yok) ile 1 (mükemmel uyum) arasında değer alır.

diagnosis <- ifelse(wdbc$Diagnosis == "M",1,2 )
cluster.stats(d = dist(data),diagnosis, p3m_data$cluster)$corrected.rand

#Label ile veri seti arasındaki Rand değeri 0.4985402 olarak saptanılmıştır. Diğer değerlerle karşılaştırılacaktır.

cluster.stats(d = dist(data),diagnosis, p3m_data$cluster)$vi

# Label ile veri seti arasında VI değeri 0.9305164 olarak saptanmıştır. Diğer değerlerle karşılaştırılacaktır.

########################
### Aşamalı Kümeleme ###
########################

############
### Ward ###
############

dist_euc <- dist(pcadata, method="euclidean")
dist_man <- dist(pcadata, method="manhattan")


hc_e <- hclust(d=dist_euc, method="ward.D2")
fviz_dend(hc_e,cex=.5) 

hc_m <- hclust(d=dist_man, method="ward.D2")
fviz_dend(hc_m,cex=.5) 

coph_e <- cophenetic(hc_e)
cor(dist_euc,coph_e)

coph_m <- cophenetic(hc_m)
cor(dist_man,coph_m)

nbward <- NbClust(pcadata , distance = "euclidean", min.nc = 2,
              max.nc = 9, method = "ward.D2")
fviz_nbclust(nbward) + labs(subtitle ="NbClust Grafiği")

# k = 2
grupward2 <- cutree(hc_e, k = 6)
grupward2
table(grupward2)


fviz_dend(hc_e, k = 6, 
          cex = 0.5, 
          color_labels_by_k = TRUE, 
          rect = TRUE )


fviz_cluster(list(data = pcadata, cluster = grupward2),
             ellipse.type = "convex", 
             repel = TRUE, 
             show.clust.cent = FALSE, ggtheme = theme_minimal())

# Küme Grafiği incelendiğinde, ayrışmanın yine yalnızca PC1 boyutunda gerçekleştiği gözlemlenmiştir.
# Mavi renkle gösterilen ikinci kümenin varyansının az olduğu gözlemlenmiştir.
# Kümeler arasındaki çakışma dikkat çekmektedir.

hc_dataward2 <- eclust(pcadata, "hclust", k = 6, hc_metric = "euclidean",hc_method = "ward.D2", graph = TRUE)

fviz_dend(hc_dataward2, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)

########################
### Küme Geçerliliği ###
########################

# Silhouette

fviz_silhouette(hc_dataward2, palette = "jco",
                ggtheme = theme_classic())

# Her bir gözlemin silhouette değerlerini içeren grafik incelendiğinde, hem mavi renkle gösterilen birinci kümede, hem de sarı renkle gösterilen ikinci kümede negatif silhouette değerleri olan gözlemlere rastlanılmıştır.
# Ortalama silhouette değeri 0.48 olarak rastlanılmıştır. 

silward2 <- hc_dataward2$silinfo$widths[, 1:3]
neg_silward2_index <- which(silward2[, 'sil_width']<0)
silward2[neg_silward2_index, , drop = FALSE]

# Negatif silhouette değerine sahip gözlemler incelendiğinde 22 gözlemin birinci kümede, 2 gözlemin ikinci kümede olduğu gözlemlenmiştir.

# Dunn Indexi 

ward2_stats <- cluster.stats(dist(pcadata), hc_dataward2$cluster)
ward2_stats$dunn

# Kümeleme sonucunda karşılaştığımız  dunn değeri sıfıra oldukça yakın ( 0.02706536 ). 

# Connectivity

connectivity(distance = NULL, hc_dataward2$cluster, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# K-medoids için connectivity değer 40.70481 olarak saptanmıştır. 
# Diğer kümeleme sonuçları ile karşılaştırılacaktır.

# Orijinal Veri İle Karşılaştırma

table(wdbc$Diagnosis, hc_dataward2$cluster)

# Orijinal label ile kümeleme sonucu arasında 42 gözlemlik bir fark gözlemlenmiştir.

# Harici Kümeleme Geçerliliği Ölçütleri 

# Bölümlenmiş küme ile harici referans değeri arasında uyum niceliği olarak düzeltilmiş Rand İndeksi ve Meila değişim indeksi VI değerleri kullanılabilir.
# -1 (uyum yok) ile 1 (mükemmel uyum) arasında değer alır.

diagnosis <- ifelse(wdbc$Diagnosis == "M",1,2 )
cluster.stats(d = dist(data),diagnosis, grupward2)$corrected.rand

#Label ile veri seti arasındaki Rand değeri 0.56 olarak saptanılmıştır. Diğer değerlerle karşılaştırılacaktır.

cluster.stats(d = dist(data),diagnosis, grupward2)$vi

# Label ile veri seti arasında VI değeri 0.70 olarak saptanmıştır. Diğer değerlerle karşılaştırılacaktır.


# k = 3
grupward3 <- cutree(hc_e, k = 3)
table(grupward3)


fviz_dend(hc_e, k = 3, 
          cex = 0.5, 
          color_labels_by_k = TRUE, 
          rect = TRUE )


fviz_cluster(list(data = pcadata, cluster = grupward3),
             ellipse.type = "convex", 
             repel = TRUE, 
             show.clust.cent = FALSE, ggtheme = theme_minimal())

# Küme grafiği incelendiğinde, ayrışmanın hem PC1, hem de PC2 boyutunda olduğu saptanılmıştır.
# Kırmızı renkle gösterilen birinci kümedeki değişim fazla iken; yeşil renkle gösterilen ikinci kümedeki varyansın az olduğu saptanılmıştır.
# Kümeler arasındaki örtüşmenin fazla olduğu gözlemlenmiştir.

hc_dataward3 <- eclust(pcadata, "hclust", k = 3, hc_metric = "euclidean",hc_method = "ward.D2", graph = TRUE)

fviz_dend(hc_dataward3, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)

########################
### Küme Geçerliliği ###
########################

# Silhouette

fviz_silhouette(hc_dataward3, palette = "jco",
                ggtheme = theme_classic())

# Her bir gözlemin silhouette değerlerini içeren grafik incelendiğinde, her üç kümede de negatif silhouette değerleri olan gözlemlere rastlanılmıştır.
# Özellikle birinci kümede negatif değeri çok fazla olan gözlemler olduğu fark edilmiştir.
# Ortalama silhouette değeri 0.48 olarak rastlanılmıştır. 

silward3 <- hc_dataward3$silinfo$widths[, 1:3]
neg_silward3_index <- which(silward3[, 'sil_width']<0)
silward3[neg_silward3_index, , drop = FALSE]

# Negatif silhouette değerine sahip gözlemler incelendiğinde, bu değerlerin 9 tanesinin birinci kümede, 2 tanesinin ikinci kümede, 19 tanesinin üçüncü kümede olduğu saptanmıştır.
# Negatif değerlerin en küçüğünün -0.37 ile birinci kümede bulunan 258. gözlem olduğu tespit edilmiştir.

# Dunn Indexi 

ward3_stats <- cluster.stats(dist(pcadata), hc_dataward3$cluster)
ward3_stats$dunn

# Kümeleme sonucunda karşılaştığımız  dunn değeri sıfıra oldukça yakın ( 0.03537124 ). 

# Connectivity

connectivity(distance = NULL, hc_dataward3$cluster, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# K-medoids için connectivity değer 60.24877 olarak saptanmıştır. 
# Diğer kümeleme sonuçları ile karşılaştırılacaktır.

# Orijinal Veri İle Karşılaştırma

table(wdbc$Diagnosis, hc_dataward3$cluster)

# Orijinal label ile kümeleme sonucu karşılaştırıldığında, orijinal label sayısı ile küme sayının birbirinden farklı olması sebebiyle bir eşitliğin olmasının zaten mümkün olmadığı bilinmekteydi.
# 1 küme M değerlerini tamamen aldığı saptanılmıştır. 3. kümede ise daha çok B değerleri bulunmaktadır.

# Harici Kümeleme Geçerliliği Ölçütleri 

# Bölümlenmiş küme ile harici referans değeri arasında uyum niceliği olarak düzeltilmiş Rand İndeksi ve Meila değişim indeksi VI değerleri kullanılabilir.
# -1 (uyum yok) ile 1 (mükemmel uyum) arasında değer alır.

diagnosis <- ifelse(wdbc$Diagnosis == "M",1,2 )
cluster.stats(d = dist(data),diagnosis, grupward3)$corrected.rand

#Label ile veri seti arasındaki Rand değeri 0.56 olarak saptanılmıştır. Diğer değerlerle karşılaştırılacaktır.

cluster.stats(d = dist(data),diagnosis, grupward3)$vi

# Label ile veri seti arasında VI değeri 0.70 olarak saptanmıştır. Diğer değerlerle karşılaştırılacaktır.

#######################
### Average linkage ###
#######################

hc_e2 <- hclust(d=dist_euc, method="average")
fviz_dend(hc_e2,cex=.5) 

hc_m2 <- hclust(d=dist_man, method="average")
fviz_dend(hc_m2,cex=.5) 

coph_e2 <- cophenetic(hc_e2)
cor(dist_euc,coph_e2)

coph_m2 <- cophenetic(hc_m2)
cor(dist_man,coph_m2)

# Cophenetic ile distance matrixi arasındaki korelasyon incelendiğinde euclidean distance ile yapılan hiyerarşiş kümelemenin daha iyi sonuç verdiği gözlemlenmiştir.

nbaverage <- NbClust(pcadata , distance = "euclidean", min.nc = 2,
              max.nc = 9, method = "average")
fviz_nbclust(nb) + labs(subtitle ="NbClust Grafiği")

# 5 ve 2 küme sayısının denenmesine karar verilmiştir.

# k = 5

grupav3 <- cutree(hc_e2, k = 7)

table(grupav3)


fviz_dend(hc_e2, k = 7, 
          cex = 0.5, 
          color_labels_by_k = TRUE, 
          rect = TRUE )


fviz_cluster(list(data = pcadata, cluster = grupav3),
             ellipse.type = "convex", 
             repel = TRUE, 
             show.clust.cent = FALSE, ggtheme = theme_minimal())

# Küme grafiği incelendiğinde ayrışmanın hem PC1, hem de PC2 boyutunda gerçekleştiği gözlemlenmiştir.
# Yeşil renk ile gösterilen 3. küme ile mor renkle gösterilen 5. kümede varyansın oldukça az olduğu saptanmıştır.
# Varyansın azlığının bir sebebi de küme eleman sayılarının oldukça az olmasıdır.
# Aksine, mavi renkle gösterilen 4. kümede eleman sayısı ve varyans diğer kümelere kıyasla daha fazla olarak gözlemlenmiştir.

hc_datav3 <- eclust(pcadata, "hclust", k = 7, hc_metric = "euclidean",hc_method = "average", graph = TRUE)

fviz_dend(hc_datav3, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)

########################
### Küme Geçerliliği ###
########################

# Silhouette

fviz_silhouette(hc_datav3, palette = "jco",
                ggtheme = theme_classic())

# Her bir gözlemin silhouette değerlerini içeren grafik incelendiğinde, sarı renkle gösterilen ikinci kümede ve kırmızı renkle gösterilen dördüncü kümede negatif silhouette değerleri olan gözlemlere rastlanılmıştır.
# Ortalama silhouette değeri 0.42 olarak rastlanılmıştır. 

silav5 <- hc_datav5$silinfo$widths[, 1:3]
neg_silav5_index <- which(silav5[, 'sil_width']<0)
silav5[neg_silav5_index, , drop = FALSE]

# Negatif silhouette değerine sahip gözlemler incelendiğinde, bu değerlerin 50 tanesinin dördüncü kümede, 2 tanesinin birinci kümede olduğu saptanmıştır.
# Negatif değerlerin en küçüğünün -0.502 ile dördüncü kümede bulunan 380. gözlem olduğu tespit edilmiştir.

# Dunn Indexi 

av5_stats <- cluster.stats(dist(pcadata), hc_datav5$cluster)
av5_stats$dunn

# Kümeleme sonucunda karşılaştığımız  dunn değeri sıfıra oldukça yakın ( 0.02939932 ). 

# Connectivity

connectivity(distance = NULL, hc_datav5$cluster, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# K-medoids için connectivity değer 68.88953 olarak saptanmıştır. Diğer kümeleme sonuçları ile karşılaştırılacaktır.

# Orijinal Veri İle Karşılaştırma

table(wdbc$Diagnosis, hc_datav5$cluster)

# Orijinal labeller ile kümeleme sonucu karşılaştırıldığında, 1, 2 ve 5. kümenin M labelını, 4. küme ise B labelını ifade ediyor.

# Harici Kümeleme Geçerliliği Ölçütleri 

# Bölümlenmiş küme ile harici referans değeri arasında uyum niceliği olarak düzeltilmiş Rand İndeksi ve Meila değişim indeksi VI değerleri kullanılabilir.
# -1 (uyum yok) ile 1 (mükemmel uyum) arasında değer alır.

diagnosis <- ifelse(wdbc$Diagnosis == "M",1,2 )
cluster.stats(d = dist(data),diagnosis, grupav5)$corrected.rand

#Label ile veri seti arasındaki Rand değeri 0.44 olarak saptanılmıştır. Diğer değerlerle karşılaştırılacaktır.

cluster.stats(d = dist(data),diagnosis, grupav5)$vi

# Label ile veri seti arasında VI değeri 0.80 olarak saptanmıştır. Diğer değerlerle karşılaştırılacaktır.

# k = 2
grupav2 <- cutree(hc_e2, k = 2)
table(grupav2)


fviz_dend(hc_e2, k = 2, 
          cex = 0.5, 
          color_labels_by_k = TRUE, 
          rect = TRUE )


fviz_cluster(list(data = pcadata, cluster = grupav2),
             ellipse.type = "convex", 
             repel = TRUE, 
             show.clust.cent = FALSE, ggtheme = theme_minimal())

# Küme grafiği incelendiğinde ayrışmanın yalnızca PC1 boyutunda gerçekleştiği gözlemlenmiştir.
# Mavi renkle gösterilen ikinci kümede hem eleman sayısı, hem de varyans kırmızı renkle gösterilen birinci kümeye kıyasla daha fazladır.

hc_datav2 <- eclust(pcadata, "hclust", k = 2, hc_metric = "euclidean",hc_method = "average", graph = TRUE)

fviz_dend(hc_datav2, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)

########################
### Küme Geçerliliği ###
########################

# Silhouette

fviz_silhouette(hc_datav2, palette = "jco",
                ggtheme = theme_classic())

# Her bir gözlemin silhouette değerlerini içeren grafik incelendiğinde, sarı renkle gösterilen ikinci kümede negatif silhouette değerleri olan gözlemlere rastlanılmıştır.
# Ortalama silhouette değeri 0.54 olarak rastlanılmıştır. 

silav2 <- hc_datav2$silinfo$widths[, 1:3]
neg_silav2_index <- which(silav2[, 'sil_width']<0)
silav2[neg_silav2_index, , drop = FALSE]

# Negatif silhouette değerine sahip gözlemler incelendiğinde, 29 değerin bulunduğu görülmüştür.
# Negatif değerlerin en küçüğünün -0.43 ile ikinci kümede bulunan 370. gözlem olduğu tespit edilmiştir.

# Dunn Indexi 

av2_stats <- cluster.stats(dist(pcadata), hc_datav2$cluster)
av2_stats$dunn

# Kümeleme sonucunda karşılaştığımız  dunn değeri sıfıra oldukça yakın (  0.063734 ). 

# Connectivity

connectivity(distance = NULL, hc_datav2$cluster, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# Average linkage ile yapılan iki kümeli hiyerarşik kümeleme için için connectivity değer 20.61224 olarak saptanmıştır. Diğer kümeleme sonuçları ile karşılaştırılacaktır.

# Orijinal Veri İle Karşılaştırma

table(wdbc$Diagnosis, hc_datav2$cluster)

# Harici Kümeleme Geçerliliği Ölçütleri 

# Bölümlenmiş küme ile harici referans değeri arasında uyum niceliği olarak düzeltilmiş Rand İndeksi ve Meila değişim indeksi VI değerleri kullanılabilir.
# -1 (uyum yok) ile 1 (mükemmel uyum) arasında değer alır.

diagnosis <- ifelse(wdbc$Diagnosis == "M",1,2 )
cluster.stats(d = dist(data),diagnosis, grupav2)$corrected.rand

#Label ile veri seti arasındaki Rand değeri 0.44 olarak saptanılmıştır. Diğer değerlerle karşılaştırılacaktır.

cluster.stats(d = dist(data),diagnosis, grupav2)$vi

# Label ile veri seti arasında VI değeri 0.80 olarak saptanmıştır. Diğer değerlerle karşılaştırılacaktır.



##############################
### Model Temelli Kümeleme ###
##############################

################
### Kümeleme ###
################

mc <- Mclust(pcadata, G = 5 )
summary(mc)
mc$classification

# Model temelli kümeleme sonucunda veri seti iki kümeye ayrılmıştır. 1 ve 2 küme için küme eleman sayıları şu şekildedir: 253, 316.

fviz_mclust(mc, "BIC", palette = "jco")

# En iyi model olarak VVI parametresi çıkmaktadır. VVV; hacmi ve şekli değişik, yönelimi eşit anlamına gelmektedir.

fviz_mclust(mc, "classification", geom = "point",
            pointsize = 1.5, palette = "jco")

# Kümeleme grafiği incelendiğinde çakışmaların olmadığı gözlemlenmiştir.
# PC1 boyunun en sağında mavi noktalar kolayca görülebilmektedir. Bu da ilginç bir sonuç olarak yorumlanabilir.
# Ayrışma yalnızca PC1 boyutunda gerçekleşmiştir.

fviz_mclust(mc, "uncertainty", palette = "jco",pos = FALSE)

# Belirsizlik grafiğinde daha büyük noktalarla ifade edilen gözlemlerin kümeleme sonuçlarının daha belirsiz olduğu ifade edilir.
# İki küme arasında belirsizliğin arttığı gözlemlenebilir. 

########################
### Küme Geçerliliği ###
########################

# Silhouette 
mc_stats <- cluster.stats(dist(pcadata), mc$classification)
mc_stats[c("avg.silwidth")]

# Ortalama silhouette değeri 0.4125059 olarak belirlenmiştir.

# Dunn Indeksi

mc_stats$dunn

# Kümeleme sonucunda karşılaştığımız  dunn değeri sıfıra oldukça yakın ( 0.002348756 ). Bu da kümelemenin başarısını sorgular.

# Connectivity

connectivity(distance = NULL, mc$classification, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# K-medoids için connectivity değer 83.61869 olarak saptanmıştır. Diğer kümeleme sonuçları ile karşılaştırılacaktır.

# Orijinal Veri ile Karşılaştırma

table(wdbc$Diagnosis, mc$classification)

# Orijinal labeller ile kümeleme sonuçları karşılaştırıldığında 77 gözlemlik bir fark olduğu saptanılmıştır.

# Harici Kümeleme Geçerliliği Ölçütleri 

# Bölümlenmiş küme ile harici referans değeri arasında uyum niceliği olarak düzeltilmiş Rand İndeksi ve Meila değişim indeksi VI değerleri kullanılabilir.
# -1 (uyum yok) ile 1 (mükemmel uyum) arasında değer alır.

diagnosis <- ifelse(wdbc$Diagnosis == "M",1,2 )
cluster.stats(d = dist(data),diagnosis, mc$classification)$corrected.rand

#Label ile veri seti arasındaki Rand değeri 0.53 olarak saptanılmıştır. Diğer değerlerle karşılaştırılacaktır.

cluster.stats(d = dist(data),diagnosis, mc$classification)$vi

# Label ile veri seti arasında VI değeri 0.75 olarak saptanmıştır. Diğer değerlerle karşılaştırılacaktır.

################################
### Density-Based Clustering ###
################################

# Kümeleme

# Yoğunluk bazlı kümelemede küme sayısının önceden belirlenmesi gerekmemektedir; ancak MinPts ve eps değerlerinin önceden belirlenmesi gerekmektedir.
# eps parametresi, x noktasının çevresindeki komşuların yarıçapını tanımlar. Buna x'in epsilon komşuluğu denir.
# MinPts parametresi, "eps" yarıçapı içindeki minimum komşu sayısıdır.
# Bu değerlere karar vermek için KNN distplot kullanılabilir.

kNNdistplot(pcadata, k = 10)
abline(h = 0.6, lty = 2)

# k, MinPts'i ifade etmektedir. Yapılan çeşitli denemeler sonrasında 5'e karar verilmiştir.
# kNNdisplot incelenirken, tıpkı Dirsek Yöntemi gibi, çizginin "dirsek" yaptığı nokta saptanılmalıdır. Bu nokta eps değeri olarak seçilmelidir.
# Çeşitli denemeler sonucunda en uygun değerin 0.7 olduğuna karar verilmiştir. 

db <- fpc::dbscan(pcadata, eps = 0.8, MinPts = 10)
print(db)

# Yoğunluk bazlı kümeleme veri setini üç kümeye ayırmıştır.
# Çıktı incelendiğinde toplam 61 gürültü değer görülmemektedir. 
# Birinci kümede 35, ikinci kümede 5, üçüncü kümede 3 sınır noktası bulunmaktadır.
# Birinci kümede 461, ikinci kümede 2, üçüncü kümede 2 çekirdek nokta bulunmaktadır.

fviz_cluster(db, data = pcadata, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

# Grafik incelendiğinde kümeler arasındaki eleman farkı çarpıcı bir şekilde görülmektedir.
# Boyutlar arasındaki açıklayıcılık varkı da oldukça fazladır. 

########################
### Küme Geçerliliği ###
########################
# Silhouette 
db_stats <- cluster.stats(dist(pcadata), db$cluster)
db_stats[c("avg.silwidth")]

# Ortalama silhouette değeri 0.1428061 olarak belirlenmiştir.

# Dunn Indeksi

db_stats$dunn

# Dunnn indeksi 0.019 olarak saptanılmıştır. Diğer değerlerle karşılaştırılacaktır.

# Connectivity

connectivity(distance = NULL, db$cluster, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# Yoğunluk bazlı kümeleme için connectivity değer 117.1417 olarak saptanmıştır. 
# Diğer kümeleme sonuçları ile karşılaştırılacaktır.

table(wdbc$Diagnosis, db$cluster)

# Orijinal labeller ile kümeleme sonuçları karşılaştırıldığında 66 gözlemlik bir fark olduğu saptanılmıştır.

# Harici Kümeleme Geçerliliği Ölçütleri 

# Bölümlenmiş küme ile harici referans değeri arasında uyum niceliği olarak düzeltilmiş Rand İndeksi ve Meila değişim indeksi VI değerleri kullanılabilir.
# -1 (uyum yok) ile 1 (mükemmel uyum) arasında değer alır.

diagnosis <- ifelse(wdbc$Diagnosis == "M",1,2 )
cluster.stats(d = dist(data),diagnosis, db$cluster)$corrected.rand

#Label ile veri seti arasındaki Rand değeri 0.53 olarak saptanılmıştır. Diğer değerlerle karşılaştırılacaktır.

cluster.stats(d = dist(data),diagnosis, db$cluster)$vi

# Label ile veri seti arasında VI değeri 0.75 olarak saptanmıştır. Diğer değerlerle karşılaştırılacaktır.

################################################################################

########################
### Küme Geçerliliği ###
########################

# Kümeleme sonrası yapılan geçerlilik ölçümleri paylaşılmadan önce clValid paketinde yer alan clValid fonksiyonundan yararlanılmak istenilmiştir.
# Bu fonksiyon verilen kümeleme yöntemleri ile kümeleme gerçekleştirerek en uygun kümeleme algoritması ve küme sayısının önerisini yapmaktadır. Üç geçerlilik ölçütü için  bu fonksiyondan yararlanılacaktır. Bu ölçütler  dahili (internal) ve harici(external) küme geçerliliği ile kümeleme kararlılığı(stability) olacaktır.

###############################
### Dahili Küme Geçerliliği ###
###############################

clmethods <- c("kmeans","pam","hierarchical", "model")
intern <- clValid(pcadata, nClust = 2:6,
                  clMethods = clmethods, validation = "internal")
summary(intern)

# Dahili kümeleme geçerliliği ölçütleri Connectivity, Dunn ve Silhouette ölçütlerini içermektedir. Bu ölçütlere her kümeleme sonrasında zaten bakılmıştı. clValid fonksiyonu bu ölçütler çerçevesinde en uygun algoritma olarak hiyerarşik kümelemeyi, optimal küme sayısı olarak ise 2 kümeyi işaret etmektedir.

############################
### Kümeleme Kararlılığı ###
############################

stab <- clValid(pcadata, nClust = 2:6, clMethods = clmethods,
                validation = "stability")
summary(stab)

optimalScores(stab)

kumegecerliligi <- data.frame( "Kümeleme Algoritması" <- c("K - Ortalamalar 2", "K - Ortalamalar 3", "K - Medoids 2", "K - Medoids 3", " Aşamalı - Ward.D2 2", "Aşamalı - Ward.D2 3", "Aşamalı - Average 5", "Aşamalı - Average 2", "Model Temelli", "Yoğunluk Temelli"),
                               "Küme Sayısı" <- c(2,3,2,3,2,3,5,2,2,3),
                               "Örtüşme" <- c("Az", "Çok", "Az", "Çok", "Az", "Çok", "Çok", "Az", "Yok", NA),
                               "Negatif Silhouette Değeri" <- c(10,9,26,47,24,30,52,29, NA, NA),
                               "Ortalama Silhouette Değeri" <- c(0.49, 0.44, 0.48, 0.36,0.48,0.48,0.42,0.54, 0.41,0.14),
                               "Dunn Değeri" <- c(0.005,0.011, 0.013, 0.004, 0.02, 0.035, 0.029, 0.063, 0.002, 0.019),
                               "Connectivity Değeri" <- c(64.96, 87.85, 50.08, 109.02, 40.70, 60.24, 68.88, 20.61, 83.61, 117.14),
                               "Rand Değeri" <- c(0.64, 0.49, 0.72, 0.39, 0.56, 0.51, 0.44, 0.60, 0.53, 0.09),
                               "VI Değeri" <- c(0.56, 0.93, 0.49, NA, 0.70, 0.86, 0.80, 0.74, 0.75, NA)
)
colnaames <- c("Kümeleme Algoritması", "Küme Sayısı", "Örtüşme", "Negatif Silhouette Değeri", "Ortalama Silhouette Değeri", "Dunn Değeri", "Connectivity Değeri", "Rand Değeri", "VI Değeri")
names(kumegecerliligi) <- colnaames
kumegecerliligi

############################################################
### En İyi Algoritma ve Optimal Küme Sayısının Seçilmesi ###
############################################################

##################
### Silhouette ###
##################

# Ortalama Silhouette Değerinin Maksimim olması gerekmektedir. Hangi kümelemede bu değerin maksimum olduğuna bakılmak istenilmiştir.

max(kumegecerliligi["Ortalama Silhouette Değeri"])
kumegecerliligi[kumegecerliligi["Ortalama Silhouette Değeri"] == 0.54] [1]

ggplot(kumegecerliligi, aes(x = `Ortalama Silhouette Değeri`, y = `Kümeleme Algoritması` )) +
  geom_point() +
  theme_minimal()+
  labs(title =  "Kümeleme Algoritmasına Göre Silhouette Değerleri")

# Silhouette değerinin en çok çıktığı kümeleme Aşamalı Kümelemedeki Average linkage methodu çıkmıştır. Küme sayısı ise ikidir.

############
### Dunn ###
############

# Dunn endeksi sıfırdan maksimuma kadar uzanan değerler alır. En iyi Dunn değeri maksimum değerdir. 

ggplot(kumegecerliligi, aes(x = `Dunn Değeri`, y = `Kümeleme Algoritması` )) +
  geom_point() +
  theme_minimal()+
  labs(title =  "Kümeleme Algoritmasına Göre Dunn Değerleri")

# Dunn değerinin en çok çıktığı kümeleme Aşamalı Kümelemedeki Average linkage methodu çıkmıştır. Küme sayısı ise ikidir.

####################
### Connectivity ###
####################

# Connectivity değeri 0’dan sonsuza kadar giden değerler alır. Mümkün olduğunca küçük olmalıdır. 

ggplot(kumegecerliligi, aes(x = `Connectivity Değeri`, y = `Kümeleme Algoritması` )) +
  geom_point() +
  theme_minimal()+
  labs(title =  "Kümeleme Algoritmasına Göre Connectivity Değerleri")

# Connectiviy değerinin en çok çıktığı kümeleme Aşamalı Kümelemedeki Average linkage methodu çıkmıştır. Küme sayısı ise ikidir.

######################
### Corrected Rand ###
######################

# Bölümlenmiş küme ile harici referans değeri arasında uyum niceliği olarak düzeltilmiş Rand İndeksi değerleri kullanılabilir.
# -1 (uyum yok) ile 1 (mükemmel uyum) arasında değer alır.

ggplot(kumegecerliligi, aes(x = `Rand Değeri`, y = `Kümeleme Algoritması` )) +
  geom_point() +
  theme_minimal()+
  labs(title =  "Kümeleme Algoritmasına Göre Rand Değerleri")

# Denenen tüm yöntemler için Rand değerleri incelendiğinde bire en yakın değerin K- Medoids algortiması çıkmıştır. Küme sayısı ise 2 olarak görünmüştür.
# İkinci sırada 2 kümeli K - Oralamalar, üçüncü sırada ise 2 kümeye average linkage ile ayrıştırılmış algoritma bulunmakta.

####################################
### Melia Değişim Indeksi ( VI ) ###
####################################

# Bölümlenmiş küme ile harici referans değeri arasında uyum niceliği olarak düzeltilmiş Meila değişim indeksi VI değerleri kullanılabilir.
# -1 (uyum yok) ile 1 (mükemmel uyum) arasında değer alır.

ggplot(kumegecerliligi, aes(x = `VI Değeri`, y = `Kümeleme Algoritması` )) +
  geom_point() +
  theme_minimal()+
  labs(title =  "Kümeleme Algoritmasına Göre VI Değerleri")

# Bütün Melia değişim değerleri incelendiğinde en iyi algoritma olarak K-Ortalamalar, küme sayısı olarak ise 3 küme çıkmıştır.

# Clvalid ve diğer tüm ölçütlerin önerilerine göre en uygun kümeleme algoritması olarak Average Linkage methodu ile yapılan Aşamalı kümeleme seçilmiştir.
# Optimal küme sayısı ise iki olarak saptanmıştır.

################################################################################

##########################
### Kümeleme Sonuçları ###
##########################

# Küme bilgilerini içeren değişken veri setine eklenmiştir.
son_data <- cbind(data, cluster= pam_data$clustering )

# Her değişkenin ortalamaları, küme ortalamaları ile karşılaştırılmak istenilmiştir.
summary(data)
aggregate(data, by=list(cluster=pam_data$clustering), mean) 

################################################
# 1. Küme İçin:               ## 2 Küme için: ##
################################################
# Radius Yüksek,              ## Ortalama     ##
# Texture Yüksek,             ## Ortalama     ##  
# Perimeter Yüksek,           ## Az           ##
# Area Yüksek,                ## Az           ##
# Smoothness Yüksek,          ## Az           ##
# Compactness Yüksek,         ## Az           ##
# Concavity Yüksek,           ## Az           ##
# Concave Points Yüksek,      ## Ortalama     ##
# Symmetry Yüksek,            ## Ortalama     ##
# Fractal Dimension Ortalama. ## Ortalama     ##
################################################

# Raporu daha fazla uzatmamak düşüncesiyle PC1 ve PC2’nin temsil ettiği ikişer değişken seçilerek her bir küme için bu değişkenlerin birbiri ile olan ilişkisinin incelendiği iki adet grafik çizdirilmiştir.

ggplot(son_data, aes(area, radius))+
  geom_point(color = "darkblue") +
  facet_grid(rows = vars(cluster))

# Birinci küme için Radius için 8, Area için 250 bandından başlayıp her iki değerin maksimum değerlerine ulaşan pozitif bir ilişki gözlemlenmiştir. İkinci küme için her iki değişkende de 0'dan başlayıp Area için 1000'de, Radius için 17'de biten pozitif bir ilişki gözlemlenmiştir.

ggplot(son_data, aes(fractal.dimension, smoothness))+
  geom_point(color = "darkolivegreen4") +
  facet_grid(rows = vars(cluster))

# Birinci küme için hem Fractal dimension değişkeninde, hem de Smoothness geniş bir aralıkta yayılım gözlemlenmekte.  İkinci küme için hem Fractal dimension değişkeninde, hem de Smoothness geniş bir aralıkta yayılım gözlemlenmekte. Bunun sebebinin ayrışmanın yalnızca PC1 değişkeninde gerçekleşmiş olması olabilir.

describeBy(wdbc[3:10], group = wdbc[2])

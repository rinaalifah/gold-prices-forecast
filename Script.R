#Input Data 
library(readxl)
dataemas <- read_excel("E:/SKRIPSI/data emas/data.xlsx", 
                       sheet = "skripsi")
dataemas
emas <- dataemas$price 
emas

#==========================================#
#Analisis Deskriptif
# Membuat vektor tanggal untuk plot time series
Tanggal <- seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = length(emas))

# Membuat data frame
data_emas <- data.frame(Tanggal = Tanggal, Harga = emas)
data_emas

# Plot time series menggunakan ggplot2
library(ggplot2)

ggplot(data_emas, aes(x = Tanggal, y = Harga)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "black",size = 2) + 
  labs(x = "Tahun", y = "Harga Emas", title = "Plot Time Series Harga Emas") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Hitung perubahan harga bulanan
perubahan <- c(NA, diff(data_emas$Harga))

# kenaikan tertinggi
max_increase <- max(perubahan, na.rm = TRUE)
bulan_tertinggi <- data_emas$Tanggal[which.max(perubahan)]

# penurunan terekstrim
min_decrease <- min(perubahan, na.rm = TRUE)
bulan_terendah <- data_emas$Tanggal[which.min(perubahan)]

# Tampilkan hasil 
cat("Kenaikan harga emas paling ekstrim terjadi pada bulan:", format(bulan_tertinggi, "%B %Y"), 
    "dengan kenaikan sebesar:", sprintf("%.0f", max_increase), "\n")
cat("Penurunan harga emas paling ekstrim terjadi pada bulan:", format(bulan_terendah, "%B %Y"), 
    "dengan penurunan sebesar:", sprintf("%.0f", min_decrease), "\n")

#Mencari Data Maksimum dan Minimum
min_emas = min(emas) 
min_emas
max_emas = max(emas) 
max_emas
jangkauan = max_emas - min_emas
jangkauan
mean(emas)
median(emas)

summary(emas)

#Menentukan Himpunan Semesta 
#==========================================#
max_emas
min_emas
#Pembentukan Interval
#==========================================#
# Jumlah dan Panjang Interval
n_emas=length(emas)
n_emas

k_emas = round(1+(3.322*log(length(emas),
                            base=10
)))
k_emas


L = round(((max_emas-min_emas)/k_emas),3)
L

#==========================================#
#Batas-batas Interval
intrv.1 = seq(min_emas,max_emas,len = k_emas+1)
intrv.1

#==========================================#
#Pembagian Interval dan Membentuk Himpunan Fuzzy
interval = data.frame(NA,nrow=length(intrv.1)-1,ncol=3)
names(interval) = c("bawah","atas","kel")
for (i in 1:length(intrv.1)-1) {
  interval[i,1]=intrv.1[i]
  interval[i,2]=intrv.1[i+1]
  interval[i,3]=i
}
interval

#==========================================#
#Nilai tengah interval
n.tengah = data.frame(tengah=(interval[,1]+interval[,2])/2,kel=interval[,3])
n.tengah

#==========================================#
#Menentukan Fuzzy Set
#==========================================#
#Fuzifikasi ke data aktual 
fuzifikasi=c() 
for (i in 1:length(emas)){
  for (j in 1:nrow(interval)){
    if (i!=which.max(emas)){
      if (emas[i]>=(interval[j,1])&emas[i]<(interval[j,2])){
        fuzifikasi[i]=j
        break
      }
    }
    else {
      if (emas[i]>=(interval[j,1])&emas[i]<=(interval[j,2])){
        fuzifikasi[i]=j
        break
      }
    }
  }
}

fuzifikasi

#==========================================#
#Fuzifikasi ke data awal
fuzzify = cbind(emas,fuzifikasi)
fuzzify

#==========================================#
#FLR dan FLRG
#==========================================#

#FLR
FLR = data.frame(fuzifikasi=0,left=NA,right =NA)
for (i in 1:length(fuzifikasi)) {
  FLR[i,1] = fuzifikasi[i]
  FLR[i+1,2] = fuzifikasi[i]
  FLR[i,3] = fuzifikasi[i]
}

FLR = FLR[-nrow(FLR),]
FLR =FLR[-1,]
FLR

#==========================================#
#FLRG
FLRG = table(FLR[,2:3])
FLRG

#==========================================#
#PERAMALAN CHEN 
#==========================================#
#Membuat matriks anggota
chen = matrix(rep(0,(nrow(FLRG)*ncol(FLRG))),ncol=ncol(FLRG))
for(i in 1:nrow(FLRG)){
  for(j in 1:ncol(FLRG)){
    if(FLRG[i,j]>0){chen[i,j]=1}
    else
      if(FLRG[i,j]==0){chen[i,j]=0}
  }
}
chen

#normalisasi matriks anggota
chen_nm = matrix(rep(0,(nrow(FLRG)*ncol(FLRG))),ncol=ncol(FLRG))
for(i in 1:nrow(chen)){
  for(j in 1:ncol(chen)){
    if(chen[i,j]==1){chen_nm[i,j]=1/(sum(chen[i,]))} 
    else
      if(chen[i,j]==0){chen_nm[i,j]=0}
  }
}
chen_nm

#==========================================#
#Perhitungan Ramalan Chen 
#Ramalan Chen 
chen_r = NULL
for(i in 1:nrow(FLR)){
  for(j in 1:(nrow(chen_nm)))
    
    
    if(fuzifikasi[i]==j)
    {chen_r[i]=sum(chen_nm[j,]*n.tengah[,1])}
  else
    if(fuzifikasi[i]==0)
    {chen_r[i]=0}
}
chen_r

predict = (chen_r)
predict

#==========================================#
#Tabel pembanding
dataa = emas[c(2:length(emas))]
galat = abs((dataa-predict)/dataa)*100
tabel = cbind(dataa, predict, galat)
tabel

#Uji ketepatan
MAPE = mean((galat))
MAPE

library(ggplot2)
library(zoo)

plotemas <- emas[2:58]
plotemas

predict
# Membuat urutan bulan dan tahun dari Februari 2020 hingga Oktober 2024 untuk data aktual
dates_aktual <- seq(as.Date("2020-02-01"), as.Date("2024-10-01"), by = "month")


# Membuat urutan bulan dan tahun dari Februari 2020 hingga Oktober 2024 untuk data prediksi
dates_prediksi <- seq(as.Date("2020-02-01"), as.Date("2024-10-01"), by = "month")


# Memastikan panjang data aktual dan data prediksi sama
panjang_data <- min(length(plotemas), length(predict))
panjang_data
# Membuat data frame untuk plot dengan panjang yang sama
plot <- data.frame(
  TAHUN = dates_aktual[1:panjang_data],
  data_aktual = plotemas[1:panjang_data],
  data_prediksi = predict[1:panjang_data]
)

# Plot time series perbandingan 
ggplot(plot, aes(x = TAHUN)) +
  geom_line(aes(y = data_aktual, color = "Data Aktual"), linewidth = 1) +
  geom_line(aes(y = data_prediksi, color = "Data Prediksi"), linewidth = 1) +
  labs(x = "Tahun", y = "Data Harga Emas di Indonesia", color = "Keterangan") +
  theme_minimal() +
  scale_color_manual(values = c("blue3", "violetred1"), labels = c("Data Aktual","Data Prediksi")) +
  ggtitle("Plot Time Series Data Aktual dan Prediksi")


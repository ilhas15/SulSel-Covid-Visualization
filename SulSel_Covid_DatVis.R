#library
library(httr)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(lubridate)
library(tidyr)

resp= GET("https://data.covid19.go.id/public/api/update.json")
status_code(resp)
resp$status_code
headers(resp)
#ekstrak data
cov_id_raw= content(resp, as = "parsed", simplifyVector= TRUE)
#ekstrak komponen
length(cov_id_raw)
names(cov_id_raw)
cov_id_update=cov_id_raw$update
#first analyze
lapply(cov_id_update, names)
##question
#### 1. tanggal update data terakhir
cov_id_update$penambahan$tanggal
#### 2. Penambahan kasus pasien sembuh
cov_id_update$penambahan$jumlah_sembuh
#### 3. Penambahan kasus pasien meninggal
cov_id_update$penambahan$jumlah_meninggal
#### 4. Total kasus positif
cov_id_update$total$jumlah_positif
#### 5. Total kasus meninggal
cov_id_update$total$jumlah_meninggal
#### 6. Total kasus sembuh
cov_id_update$total$jumlah_sembuh

## CASE COVID-19 provinsi Sulawesi Selatan
#Get Data
resp_sulsel= GET("https://data.covid19.go.id/public/api/prov_detail_SULAWESI_SELATAN.json")
#Ekstrak Data
cov_sulsel_raw= content(resp_sulsel,as = "parsed", simplifyVector=TRUE)
#view label raw
names(cov_sulsel_raw)
##first analyze
###question
### 1. Total kasus covid-19
cov_sulsel_raw$kasus_total
### 2. Persentase pasien meninggal
cov_sulsel_raw$meninggal_persen
### 3. Persentase pasien sembuh
cov_sulsel_raw$sembuh_persen

#Perkembangan kasus
# get table data 
cov_sulsel = cov_sulsel_raw$list_perkembangan

str(cov_sulsel)
head(cov_sulsel)

##Data preprocessing
new_cov_sulsel=
  cov_sulsel %>%
  select(-starts_with("AKUMULASI")) %>% 
  rename(
    perawatan = DIRAWAT_OR_ISOLASI,
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh= SEMBUH
    ) %>%
  mutate(
    tanggal = as.POSIXct(tanggal/1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )
View(new_cov_sulsel)
str(new_cov_sulsel)

# BAR plot Visualization kasus positif harian covid-19 SULSEL
ggplot(new_cov_sulsel,aes(tanggal,new_cov_sulsel$kasus_baru))+
  geom_col(fill="orangered2")+
  labs(
    x=NULL,
    y= "Jumlah Kasus",
    title = "Kasus Harian Positif COVID-19 di Sulawesi Selatan",
    subtitle = expression(
      atop(
        "Update data terakhir: 11 October 2020"
      )
    ),
    caption = "Sumber data covid.19.go.id"
  )+
  theme_ipsum(
    base_size = 14,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  )+
  theme(plot.title.position="plot")
ggsave('Pos-SulSel-harian.png', width = 8, height = 7)
## save bar plot diagram
cov_bar_positif = ggplot(new_cov_sulsel,aes(tanggal,new_cov_sulsel$kasus_baru))+
  geom_col(fill="orangered2")+
  labs(
    x=NULL,
    y= "Jumlah Kasus",
    title = "Kasus Harian Positif COVID-19 di Sulawesi Selatan",
    subtitle = "Update data terakhir: 11 October 2020",
    caption = "Sumber data covid.19.go.id"
  )+
theme_ipsum(
  base_size = 14,
  plot_title_size = 21,
  grid = "Y",
  ticks = TRUE
)+
theme(plot.title.position="plot")
ggsave('Pos-SulSel-harian.png', width = 8, height = 7)
#view diagram
cov_bar_positif
# BAR plot Visualization kasus pasien sembuh harian covid-19 SULSEL
ggplot(new_cov_sulsel,aes(tanggal,sembuh))+
  geom_col(fill="aquamarine2")+
  labs(
    x=NULL,
    y= "Jumlah Kasus",
    title = "Kasus Harian Sembuh Dari COVID-19 di Sulawesi Selatan",
    subtitle = "Update data terakhir: 11 October 2020",
    caption = "Sumber data covid.19.go.id"
  )+
  theme_ipsum(
    base_size = 14,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  )+
  theme(plot.title.position="plot")
ggsave("sembuh-Sulsel-harian.png", width = 9, height = 8)
## save bar plot diagram
cov_bar_sembuh = ggplot(new_cov_sulsel,aes(tanggal,sembuh))+
  geom_col(fill="aquamarine2")+
  labs(
    x=NULL,
    y= "Jumlah Kasus",
    title = "Kasus Harian Sembuh Dari COVID-19 di Sulawesi Selatan",
    subtitle = "Update data terakhir: 11 October 2020",
    caption = "Sumber data covid.19.go.id"
  )+
  theme_ipsum(
    base_size = 14,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  )+
  theme(plot.title.position="plot")
#view diagram
cov_bar_sembuh
# BAR plot Visualization kasus kematian harian covid-19 SULSEL
ggplot(new_cov_sulsel,aes(tanggal,meninggal))+
  geom_col(fill="gray76")+
  labs(
    x=NULL,
    y= "Jumlah Kasus",
    title = "Kasus Harian Meninggal akibat COVID-19 di Sulawesi Selatan",
    subtitle = "Update data terakhir: 11 October 2020",
    caption = "Sumber data covid.19.go.id"
  )+
  theme_ipsum(
    base_size = 14,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  )+
  theme(plot.title.position="plot")
ggsave("die-Sulsel-harian.png", width = 9, height = 8)
## save bar plot diagram
cov_bar_die=ggplot(new_cov_sulsel,aes(tanggal,meninggal))+
  geom_col(fill="gray76")+
  labs(
    x=NULL,
    y= "Jumlah Kasus",
    title = "Kasus Harian Meninggal akibat COVID-19 di Sulawesi Selatan",
    subtitle = "Update data terakhir: 11 October 2020",
    caption = "Sumber data covid.19.go.id"
  )+
  theme_ipsum(
    base_size = 14,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  )+
  theme(plot.title.position="plot")

cov_bar_die

cov_sulsel_pekan= new_cov_sulsel %>%
  count(
    tahun = year(tanggal),
    pekan_ke = week(tanggal),
    wt = kasus_baru,
    name = "jumlah"
    )
glimpse(cov_sulsel_pekan)

#add new column
cov_sulsel_pekan= cov_sulsel_pekan %>%
  mutate(
    jumlah_pekan_lalu= dplyr::lag(jumlah,1),
    jumlah_pekan_lalu= ifelse(is.na(jumlah_pekan_lalu),0,jumlah_pekan_lalu),
    lebih_baik= jumlah<jumlah_pekan_lalu
    )
glimpse(cov_sulsel_pekan)
View(cov_sulsel_pekan) 
## bar plot kasus pekanan
# question: apakah pekan ini lebih baik dari pekan yang lalu?
ggplot(cov_sulsel_pekan,aes(pekan_ke, jumlah, fill= cov_sulsel_pekan$lebih_baik))+ geom_col(show.legend = FALSE)+
  scale_x_continuous(breaks = 9:41,expand = c(0,0))+
  scale_fill_manual(values = c("TRUE" = "gold", "FALSE" = "firebrick3"))+
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Pekanan Positif COVID-19 di Sulawesi Selatan ",
    subtitle = "Kolom emas menunjukan penambahan kasus baru lebih sedikit dibandingkan satu pekan sebelumnya",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 11,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")
ggsave('Pos-Sulsel-pekan.png', width = 8, height = 7)
#save bar plot diagram
cov_bar_pekanan = ggplot(cov_sulsel_pekan,aes(pekan_ke, jumlah, fill= cov_sulsel_pekan$lebih_baik))+ geom_col(show.legend = FALSE)+
  scale_x_continuous(breaks = 9:40,expand = c(0,0))+
  scale_fill_manual(values = c("TRUE" = "gold", "FALSE" = "firebrick3"))+
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Pekanan Positif COVID-19 di Sulawesi Selatan ",
    subtitle = "Kolom emas menunjukan penambahan kasus baru lebih sedikit dibandingkan satu pekan sebelumnya",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

cov_sulsel_akumulasi = new_cov_sulsel %>%
  transmute(
    tanggal,
    akumulasi_aktif= cumsum(kasus_baru)-cumsum(sembuh)-cumsum(meninggal),
    akumulasi_sembuh= cumsum(sembuh),
    akumulasi_meninggal= cumsum(meninggal)
  )
tail(cov_sulsel_akumulasi)

##line chart akumulasi kasus 
ggplot(cov_sulsel_akumulasi, aes(x=tanggal,y= akumulasi_aktif))+ geom_line(color='orangered2', size=1.5)+
  labs(
    x="Tanggal", 
    y= "Akumulasi_aktif",
    title = "Akumulasi kasus aktif"
  )+
  theme_ipsum(
    ticks = TRUE
  )
ggsave('Akumulasi_aktif.png', width = 8, height = 7)
ggplot(cov_sulsel_akumulasi, aes(x=tanggal,y= akumulasi_sembuh))+ geom_line(color='aquamarine2', size=1.5)+
  labs(
    x="Tanggal", 
    y= "Akumulasi_sembuh",
    title = "Akumulasi kasus sembuh"
  )+
  theme_ipsum(
    ticks = TRUE
  )
ggsave('Akumulasi_sembuh.png', width = 8, height = 7)
ggplot(cov_sulsel_akumulasi, aes(x=tanggal,y= akumulasi_meninggal))+ geom_line(color='gray76', size=1.5)+
  labs(
    x="Tanggal", 
    y= "Akumulasi_meninggal",
    title = "Akumulasi kasus meninggal"
  )+
  theme_ipsum(
    ticks = TRUE
  )
ggsave('Akumulasi_meninggal.png', width = 8, height = 7)
##Transform Data
dim(cov_sulsel_akumulasi)
cov_sulsel_akumulasi_pivot= cov_sulsel_akumulasi %>%
  gather(
    key = "kategori",
    value = "jumlah",
    -tanggal
  )%>%
  mutate(
  kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
  )
#cek jumlah baris&kolom
dim(cov_sulsel_akumulasi_pivot)
#cek isi table
glimpse(cov_sulsel_akumulasi_pivot)

##Final plot
ggplot(cov_sulsel_akumulasi_pivot,aes(tanggal,jumlah,colour= (kategori)))+
  geom_line(size=1.5)+
  scale_y_continuous(sec.axis = dup_axis(name = NULL))+
  scale_colour_manual(
    values = c(
      "aktif"= "orangered2",
      "meninggal"= "gray76",
      "sembuh"= "aquamarine2"
    ),
    labels= c("Aktif","Meninggal", "Sembuh")
  )+
  labs(
    x = NULL,
    y = "Jumlah kasus akumulasi",
    colour = NULL,
    title = "Dinamika Kasus COVID-19 di Sulawesi Selatan",
    caption = "Sumber data: covid.19.go.id"
  )+
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )
ggsave('Final-SulSel-Covid.png',width = 8, height = 7)

#Prediction
predict_cov_sulsel_pos= new_cov_sulsel %>%
  count(
    tahun = year(tanggal),
    pekan_ke = week(tanggal),
    wt= kasus_baru,
    name= "kasus_baru"
  )
View(predict_cov_sulsel_pos)

#new_case
reglin= lm(kasus_baru~pekan_ke, data= predict_cov_sulsel_pos)
summary(reglin)

plot(kasus_baru~pekan_ke, data = predict_cov_sulsel_pos)+abline(reglin)

ggplot(predict_cov_sulsel_pos,aes(x=pekan_ke,y=kasus_baru))+geom_point()+
  stat_smooth(method = "lm",col= "red")+
  labs(
    title = "Prediksi Peningkatan Kasus Covid-19 di Sulawesi Selatan",
    subtitle = "Berdasarkan data per-Pekan",
    caption ="by: Ilhas F"
  )
#sembuh
predict_cov_sulsel_sembuh= new_cov_sulsel %>%
  count(
    tahun = year(tanggal),
    pekan_ke = week(tanggal),
    wt= sembuh,
    name= "Sembuh"
  ) 

reglin_sembuh= lm(Sembuh~pekan_ke, data= predict_cov_sulsel_sembuh)
summary(reglin_sembuh)

plot(Sembuh~pekan_ke, data = predict_cov_sulsel_sembuh)+abline(reglin)

ggplot(predict_cov_sulsel_sembuh,aes(x=pekan_ke,y=Sembuh))+geom_point()+
  stat_smooth(method = "lm",col= "darkcyan")+
  labs(
    title = "Prediksi Tingkat Kesembuhan Covid-19 di Sulawesi Selatan",
    subtitle = "Berdasarkan data per-Pekan",
    caption ="by: Ilhas F"
  )

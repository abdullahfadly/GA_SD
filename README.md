# Optimal Unified Variable Importance WebApps Using Rshiny
#### Arranged by:
- Abdullah Fadly
- Achmad Ismail Mufrodi
- Retno Wahyuningsih
- Bagus Sartono
- Farit M. Efendi
- Rahma Anisa

#### Table of content:
* I. Intorduction
* II. Web Apps Overview
* III. Program for calculating methods
* IV. Programs for displaying visualizations
* V. Program to create User Interface and Server


## I. Introduction
One primary question in a statistical modeling, as well as in a supervised machine learning study, is about the contribution of each explanatory variable in the model.  The topic about the variable importance has been received a huge consideration especially when the analyst was interested in revealing which variables are the most important in affecting the change in the response variable of the regression model or determining the class of the response variable of the classification model.  This present paper would focus on a binary classification problem, rather than regression model, therefore the response variable has only two classes.

There are several different approaches to identify the importance of an explanatory variables in a classification model.  We call the methods to measure of this as the variable importance measurements (VIM’s).   Even though it is not completely true, some discussions about variable importance is overlapping with the discussions on variables/features selection.  It is because in a certain circumstance, the result of identifying variable importance could be used as an input to do the variable selection.

The first type of the VIM approaches is the filter method that was computed prior to the model development step.  This includes Information Gain (IG) dan Information Value (IV).  The more importance the variable, the higher values for both measurements.  The IG and IV were calculated independently among explanatory variables, in the meaning that the process to obtain the IG and IV values for a certain explanatory variable Xi would not involve other explanatory variables Xj, j != i.

The second type of the approaches is the one which computed when the model has been developed. This includes Mean Decrease Accuracy (MDA) and Mean Decrease Gini (MDG). MDA and MDG are variable importance values generated through the Random Forest classification process. 

With the abundance of alternatives in determining the variable importance, analysts and researchers may get confuse in using the result of one of them.  It is well known that four methods of VIM may come up with different order of importance.

This WebApps is using an optimization approach to obtain the rank of the importance of the explanatory variables based on several different VIM’s.  The basic idea is to find an arrangement of ranks that agrees to all VIM’s under interest as much as possible.  We employ genetic algorithm and simulated annealing methodology to result that optimal arrangement.  By applying this optimal approach, the analyst would end up with a single rank and be easier to conclude the result or to use it for a follow-up analysis.  Therefore, it sounds to unify several VIM’s into a single measurement.

### II. WebApps Overview 

![Uploading OVIMP Overview.gif…]()

You can see the apps on this link: http://shiny.stat.ipb.ac.id/bagusco/ovimp/

### III. Program for calculating methods (stored in METODE.R)
```R
iv <- function(data, inputvary){

 infvalue <- create_infotables(data=data, y = inputvary,
 parallel = FALSE)
 data.frame(infvalue$Summary)
}
ig <- function(data, inputvary){
 varx <- setdiff(colnames(data),inputvary)
 infogain <- information.gain(formula = as.formula(paste(inputvary, "~",
paste(varx, collapse = "+"))), data = data, unit = 'log2')
 peubah <- rownames(infogain)
 Variable <- peubah[order(infogain, decreasing = TRUE)]
 IG <- infogain[order(infogain, decreasing = TRUE),]
 cbind(Variable, IG)
}
mdgmda <- function(data, inputvary){

 Variable <- setdiff(colnames(data),inputvary)
 set.seed(43)
 model.rf <- randomForest(formula = as.formula(paste(inputvary," ~ ",

paste(Variable,collapse="+"))),
 data = data,
proximity = TRUE, importance = TRUE, nodesize =
5)
 a <- importance(model.rf)
 b <- a[,3:4]
 data.frame(Variable, b)
}
```

###IV. Programs for displaying visualizations (stored in PLOT.R)
```R
plotgab <- function(data){
 colnames(data) <- c("Variabel", "IV", "IG", "MDA", "MDG", "GA")
 iv <- data[,1:2]
 ig <- data[,c(1,3)]
 mda <- data[,c(1,4)]
 mdg <- data[,c(1,5)]

 iv$piv <- as.integer(rank(iv$IV, ties.method ='average'))
 ig$pig <- as.integer(rank(ig$IG, ties.method ='average'))
 mda$pmda <- as.integer(rank(mda$MDA, ties.method ='average'))
 mdg$pmdg <- as.integer(rank(mdg$MDG, ties.method ='average'))


 drank <-
as.data.frame(cbind(iv$Variabel,iv$piv,ig$pig,mda$pmda,mdg$pmdg,data$GA))
 colnames(drank) <- c('Variabel','IV','IG','MDA','MDG','GA')

 long <- melt(setDT(drank), id.vars = c('Variabel','GA'), variable.name =
'Metode')
 long$value <- as.numeric(long$value)
 long$GA <- as.numeric(as.character(long$GA))


 ggplot(long, aes(x = reorder(Variabel,GA), fill = Metode, weight =
value)) +
 geom_bar(position = "dodge") +
 scale_fill_viridis_d(option = "plasma") +
 theme_minimal() +
 labs(x='Variabel',y='Peringkat Kepentingan', title = 'Diagram
Kepentingan Variabel')+
 theme(axis.text.x = element_text(angle = 45, size = 12),
 axis.text.y = element_blank())
}
plotiv <- function(data){
 IV <- data[,2]
 Variable <- data[,1]
 GA <- data[,6]
 ggplot(data, aes(x=reorder(Variable,GA), y=IV)) +
 geom_segment( aes(x=reorder(Variable,GA), xend=reorder(Variable,GA),
y=0, yend=IV), color="grey") +
 geom_point( color="blue", size=6) +
 theme_light() +
 theme(
 panel.grid.major.x = element_blank(),
 panel.border = element_blank(),
 axis.ticks.x = element_blank()
 ) +
 labs(x='Variabel',y='IV', title = 'Diagram Information Value')+
 theme(axis.text.x = element_text(angle = 45, size = 12))
}
plotig <- function(data){
 IG <- data[,3]
 IG <- as.numeric(IG)
 Variable <- data[,1]
 GA <- data[,6]
 ggplot(data, aes(x=reorder(Variable,GA), y=IG)) +
 geom_segment( aes(x=reorder(Variable,GA), xend=reorder(Variable,GA),
y=0, yend=IG), color="grey") +
 geom_point( color="purple", size=6) +
 theme_light() +
 theme(
 panel.grid.major.x = element_blank(),
 panel.border = element_blank(),
 axis.ticks.x = element_blank()
 ) +
 labs(x='Variabel',y='Information Gain', title = 'Diagram Information
Gain')+
 theme(axis.text.x = element_text(angle = 45, size = 12))
}
plotmda <- function(data){

 MDA <- data[,4]
 Variable <- data[,1]
 GA <- data[,6]
 ggplot(data, aes(x=reorder(Variable,GA), y=MDA)) +
 geom_segment( aes(x=reorder(Variable,GA), xend=reorder(Variable,GA),
y=0, yend=MDA), color="grey") +
 geom_point( color="orange", size=6) +
 theme_light() +
 theme(
 panel.grid.major.x = element_blank(),
 panel.border = element_blank(),
 axis.ticks.x = element_blank()
 ) +
 labs(x='Variabel',y='Mean Decrease Accuracy', title = 'Diagram Mean
Decrease Accuracy')+
 theme(axis.text.x = element_text(angle = 45, size = 12))
}
plotmdg <- function(data){

 MDG <- data[,5]
 Variable <- data[,1]
 GA <- data[,6]
 ggplot(data, aes(x=reorder(Variable,GA), y=MDG)) +
 geom_segment( aes(x=reorder(Variable,GA), xend=reorder(Variable,GA),
y=0, yend=MDG), color="grey") +
 geom_point( color="yellow", size=6) +
 theme_light() +
 theme(
 panel.grid.major.x = element_blank(),
 panel.border = element_blank(),
 axis.ticks.x = element_blank()
 ) +
 labs(x='Variabel',y='Mean Decrease Gini', title = 'Diagram Mean
Decrease Gini')+
 theme(axis.text.x = element_text(angle = 45, size = 12))

}
```


###V. Program to create User Interface and Server (stored in app.R)
```R
library(shiny)
library("shinythemes")
library(dplyr)
library(Information)
library(classInt)
library(FSelector)
library(randomForest)
library(ggplot2)
library(shinyWidgets)
library(data.table)
library('DT')
source("METODE.R")
source("PLOT.R")
options(shiny.maxRequestSize = 30*1024^2)
ui <- fluidPage(theme = shinytheme("flatly"),
 title = "Kepentingan Variabel",
 tags$head(tags$link(rel="shortcut icon", href="ipb.png")),
 img(src='header.png',height=165,width=1320),
 br(),
 column(3,
 fileInput("data", "Pilih Data", width="100%",
 buttonLabel = "Unggah...", placeholder =
"File belum dipilih"),
 radioButtons("delim", "Delimiter", inline = TRUE,
 choiceNames = c("Koma", "Spasi",
"Titik koma"),
choiceValues = c(",", " ", ";"), width
= "100%"),
 checkboxInput("header","Baris Pertama Sebagai Nama
Kolom", value=TRUE, width = "100%"),
 selectInput("vary", "Pilih Variabel Respon (Harus
Biner)", choices = "", multiple = F, width = "100%"),
 numericInput("num", label = ("Banyak Variabel yang
Ditampilkan di Hasil"), value = 5),
 submitButton('Proses'),
 hr(),
 p('Jika terdapat kendala atau pertanyaan terkait
aplikasi ini, hubungi', tags$a(href="mailto:rgas.statipb@gmail.com",
tags$u("rgas.statipb@gmail.com"))),
 p('Copyright', HTML("&copy;") ,'Departemen
Statistika IPB',style='color:blue')
 ),
 column(9,
 tabsetPanel(type = "tabs",
 tabPanel("Pratinjau Data",
dataTableOutput("preview_dt")),
 tabPanel("Tabel Hasil",
dataTableOutput("gab")),
 tabPanel("Visualisasi",
 fluidRow(verticalLayout(

(verticalLayout((plotOutput("plots5")),

tags$span(style='color:blue','Catatan: Ketinggian batang tidak menunjukkan
nilai asli dari perhitungan masing-masing metode, melainkan menunjukkan 
urutan peringkatnya, dimana semakin tinggi peringkat maka semakin tinggi
pula batang.'),

(downloadButton(outputId = "Gabungan", label = "Download")))),
 splitLayout(
 cellWidths = 400,
 cellArgs = list(style =
"padding: 6px"),
(verticalLayout((plotOutput("plots1")),

(downloadButton(outputId = "IV", label = "Download")))),

(verticalLayout((plotOutput("plots2")),

(downloadButton(outputId = "IG", label = "Download"))))),
 splitLayout(
 cellWidths = 400,
 cellArgs = list(style =
"padding: 6px"),

(verticalLayout((plotOutput("plots3")),

(downloadButton(outputId = "MDA", label = "Download")))),

(verticalLayout((plotOutput("plots4")),

(downloadButton(outputId = "MDG", label = "Download"))))
 )))),
 tabPanel("Panduan
Pengguna",h4(strong('Dataset',style='color:blue')),
 tags$ul(
 tags$li('Format file dari
dataset yaitu',em('comma separated value'),'(csv)'),
tags$li('Nilai dari variabel
respon (variabel Y) harus biner'),
tags$span(style='color:orange','contoh:'), '"Yes" atau "No", 1 atau 0,
"Sukses" atau "Gagal"',
 tags$li('Tidak ada',
tags$i('missing value'), 'pada dataset'),
tags$span(style='color:red','Baris dengan', tags$i('missing value'),'tidak
diikutkan dalam pembentukan model')
 ),br(),
h4(strong('Prosedur penggunaan
web',style='color:blue')),
 tags$ol(
 tags$li('Klik "Unggah" untuk
memilih dataset yang ingin digunakan dari direktori anda (format harus
csv)'),
 tags$li('Pilih delimiter
(pembatas) dari dataset yang dipilih'),
 tags$li('Cekllis keterangan
"Baris Pertama Nama Kolom" apabila baris pertama pada dataset merupakan
nama kolom atau nama variabel'),
 tags$li('Pada tab "Pratinjau
Data", akan ditampilkan 6 baris pertama dari dataset'),
tags$li('Pilih variabel yang
akan dijadikan variabel respon. Nilai dari variabel harus biner (contoh:
"Yes" atau "No", 1 atau 0, "Sukses" atau "Gagal")'),
 tags$li('Pilih banyak
variabel prediktor teratas yang ingin ditampilkan pada hasil. (Nilai
default = 5 variabel)'),
 tags$li('Klik "Proses" untuk
melakukan proses perhitungan',em('Genetic Algorithm')),
tags$li('Pada tab "Tabel
Hasil", akan ditampilkan tabel berisi nilai-nilai dari
perhitungan',em('Information Value, Information Gain, Mean Decrease Gini,
Mean Decreas Accuracy,'), 'dan', em('Genetic Algorithm.'),'Nilai
kepentingan diurutkan dari yang terbesar hingga terkecil berdasarkan
proses dari', em('Genetic Algorithm')),
 tags$li('Pada tab
"Visualisasi", akan ditampilkan', em('Bar Plot'), 'berdasarkan hasil
perhitungan', em('Genetic Algorithm')),
 tags$li('Klik "Download"
untuk mengunduh plot yang diinginkan')
 )),
 tabPanel("Tentang Aplikasi",p('Aplikasi
ini disusun untuk membantu pengguna dalam mengidentifikasi tingkat
kepentingan variabel yang akan dilibatkan pada suatu pemodelan
klasifikasi. Terdapat empat ukuran', tags$i('(metric)'), 'yang digunakan
dalam mengukur tingkat kepentingan variabel tersebut yaitu'),
 p(em('- Information
Value')),p(em('- Information Gain')),p(em('- Mean Decrease
Accuracy')),p(em('- Mean Decrease Gini')),
 p('Karena terdapat 4 (empat)
macam', tags$i('metric,'), 'sangat mungkin antar ukuran ini berbeda urutan
tingkat kepentingannya. Suatu variabel dapat saja menjadi yang paling
tinggi di suatu ukuran, namun hanya menempati peringkat ketiga berdasarkan
ukuran yang lain.'),
 p('Pada aplikasi ini,
diberikan urutan/peringkat tingkat kepentingan secara global dengan
memperhatikan keempat ukuran yang digunakan. Teknik',
tags$i(strong('Genetic Algorithm')), 'digunakan untuk mendapatkan
peringkat global yang optimum, sehingga peringkat tersebut memiliki
tingkat kesetujuan yang besar dengan semua',em('metric'), 'yang
digunakan.'),
 p(em('Genetic
Algorithm'),'merupakan teknik optimasi yang bekerja dengan meniru proses
evolusi makhluk hidup. Algoritma ini bekerja dalam beberapa tahapan yang
berulang sampai konvergen yaitu:'),
 p('- Pembangkitan
populasi'),p('- Seleksi individu terbaik'),p('- Mutasi gen'),p('- Kawin
silang'),
 p('Pada aplikasi ini, individu
pada', em('Genetic Algorithm'), 'adalah urutan tingkat kepentingan 
variabel yang berbentuk gen sebanyak variabel prediktor. Fungsi objektif
atau', em('fitness value'), 'yang digunakan adalah rata-rata korelasi dan
minimum korelasi antara urutan dengan empat buah', em('metric'), 'yang
telah disebutkan sebelumnya.'),
 br(),
p('Penjelasan lebih detail
dari masing-masing', em('metric'), 'tingkat kepentingan variabel adalah
sebagai berikut.'),
 p(strong(em('Information
Value'))),
 p('Istilah', em('Information
Value'), '(IV) tidak dapat dipisahkan dengan istilah', em('Weight of
Evidence'), '(WoE), dimana WoE menggambarkan hubungan antara prediktor dan
variabel dependen biner, sementara IV merupakan ukuran kekuatan hubungan
tersebut.'),
 p(strong(em('Information
Gain'))),
p(em('Information Gain'),
'(IG) biasanya digunakan dalam membangun', em('decision tree'), 'dari
dataset latih, dengan mengevaluasi nilai IG untuk tiap variabelnya, dan
memilih variabel yang memaksimumkan nilai IG, yang selanjutnya akan
meminimumkan entropi sehingga akan ditentukan pembagi terbaik', em('(best
splits)'), 'dataset menjadi grup untuk klasifikasi yang efektif. IG juga
dapat dijadikan sebagai indikator dalam seleksi fitur, dengan mengevaluasi
nilai', em('gain'), 'tiap variabel prediktor terhadap variabel dependen.
Perhitungannya mengacu pada', em('mutual information'), 'antara dua
variabel.'),
 p(strong(em('Mean Decrease
Accuracy'),'(MDA) dan', em('Mean Decrease Gini'), '(MDG)')),
p('MDA dan MDG merupakan nilai
yang dihasilkan melalui adanya proses metode', em('Random Forest'),'. Pada
MDA, nilainya ditentukan selama fase kalkulasi eror. Semakin berkurang
akurasi dari', em('random forest'), 'yang disebabkan oleh suatu variabel,
semakin penting pula variabel tersebut untuk dipertimbangkan. Oleh karena
itu, variabel dengan nilai MDA yang tinggi dapat dikatakan memiliki
peranan yang penting dalam klasifikasi. Sementara itu, MDG merupakan nilai
ukuran seberapa besar kontribusi suatu variabel pada homogenitas',
em('enodes'), 'dan', em('leaves'), 'dalam menghasilkan', em('random
forest'),'. Setiap kali variabel tertentu digunakan untuk
membagi',em('node'),', koefisien Gini untuk', em('child nodes'), 'akan
dihitung dan dibandinkan dengan', em('original nodes'),'. Koefisien Gini
adalah ukuran homogenitas dengan rentang 0 (homogen) hingga 1 (heterogen).
Perubahan dalam Gini dijumlahkan untuk setiap variabel dan dinormalisasi
pada akhir perhitungan. Variabel yang menghasilkan',em('node'), 'dengan
kemurnian',em('(purity)'),'lebih tinggi memiliki MDG yang lebih tinggi.'),
 br(),
p('Dengan adanya nilai
kepentingan variabel berdasarkan perhitungan metode',
tags$i(strong('Genetic Algorithm')),', pengguna dapat menentukan variabel
prediktor mana saja yang relevan atau yang memiliki pengaruh tinggi
terhadap variabel respon, sehingga dapat dijadikan acuan dalam menyeleksi
variabel prediktor.'),
 p(strong('R-GAS V
1.0',style='color:blue'), align='center'),
 p(strong('Agustus
2020',style='color:blue'), align='center')
 ),
tabPanel("Pengembang",h3('Tim
Pengembang',align='center'),
 br(),h4('Pembimbing',
align='center'),
fluidRow(verticalLayout(splitLayout(cellWidths = 330,

cellArgs = list(style = "padding: 6px"),

p(h3(img(src='bagus.png',height=100,width=100), align='center'), h5('Bagus
Sartono', align='center')),
p(h3(img(src='rahma-anisa.png',height=100,width=100), align='center'),
h5('Rahma Anisa', align='center')),

p(h3(img(src='farit.png',height=100,width=100), align='center'), h5('Farit
M. Afendi', align='center'))),

br(),h4('Pelaksana', align='center'),
splitLayout(cellWidths = 330,

cellArgs = list(style = "padding: 6px"),

p(h3(img(src='abdullah.png',height=100,width=100), align='center'),
h5('Abdullah Fadly', align='center')),

p(h3(img(src='retno.png',height=100,width=100), align='center'), h5('Retno
Wahyuningsih', align='center')),

p(h3(img(src='ismail.png',height=100,width=100), align='center'),
h5('Achmad Ismail Mufrodi', align='center')))
 )))
 )
 )
)
server <- function(input,output,session) {

 dfn<-reactive({
 if(is.null(input$data)){
 return(NULL)
 } else {
 read.csv(input$data$datapath, header = input$header, sep =
input$delim)
 }
 })

 observe({
 updateSelectInput(session,"vary","Pilih Variabel Respon (Harus
Biner)", choices = names(dfn()))
 
 })

 imp <- reactive({
 claim <- na.omit(dfn())
 getmode <- function(v) {
 uniqv <- unique(v)
 uniqv[which.max(tabulate(match(v, uniqv)))]
 }
 modus <- getmode(claim[,input$vary])
 l <- length(unique(claim[,input$vary]))
 if (l > 2){
 showModal(modalDialog("Variabel Respon Harus Biner"))
 }
 else if(modus != 0 || modus != 1){
 claim[,input$vary] <- ifelse(claim[,input$vary] == modus, 0 ,1)
 }
 claim
 })

 imp2 <- reactive({
 claim2 <- na.omit(dfn())
 varx <- setdiff(colnames(claim2),input$vary)
 for (i in varx){
 claim2[,i] <- as.numeric(claim2[,i])

 eqwidth <- classIntervals(claim2[,i], style = 'equal', 2)
 claim2[,i] <- cut(claim2[,i], breaks = eqwidth$brks)
 }
 claim2
 })

imp3 <- reactive({
 claim3<- na.omit(dfn())
 claim3[,input$vary] <- as.factor(claim3[,input$vary])
 Variable <- setdiff(colnames(claim3),input$vary)
 claim3
 })

 output$preview_dt <- renderDataTable({
 if(is.null(input$data)){
 return(NULL)
 } else {
 dfn <- dfn()
 dfn
 }
 })

 result <- reactive({
 set.seed(43)
 IV <- iv(imp(), inputvary = input$vary)
 MDGMDA<- mdgmda(data = imp3(), inputvary = input$vary)
 IG <- ig(data = imp2(), inputvary = input$vary)

 result <- merge(IV,IG,by = "Variable")
 result <- merge(result, MDGMDA, by = "Variable")

 skor1 <- (result[,2])
 skor2 <- as.numeric(result[,3])
 skor3 <- (result[,4])
 skor4 <- (result[,5])

 ngitung.korelasi <- function(x){
 k <- nrow(x)
 kor <- NULL
 for (j in 1:k){
 kor <- c(kor, min(c(cor(x[j,], skor1), cor(x[j,], skor2),
 cor(x[j,], skor3), cor(x[j,], skor4))))
 }
 return(kor)
 }

 n <- length(skor1)
 npop <- 10
 populasi <- NULL
 for (i in 1:npop){
 populasi <- rbind(populasi,sample(1:n, n))
 }

 kawin <- function(x, batas, semua){
 hasilkawin <- x
 k <- nrow(x)
 for (ii in 1:(k-1)){
 for (jj in (ii+1):k){
 ayah <- x[ii,]
 ibu <- x[jj,]
 anak1 <- c(ayah[1:batas],ibu[(batas+1):semua])+runif(semua,-
0.2,0.2)
 anak2 <- c(ibu[1:batas],ayah[(batas+1):semua])+runif(semua,-
0.2,0.2)
 anak1 <- rank(anak1)
 anak2 <- rank(anak2)
 hasilkawin <- rbind(hasilkawin, anak1, anak2)
 }
 }
 return(hasilkawin)
 }
 mutasi <- function(x){
 hasilmutasi <- x + replicate(ncol(x), runif(nrow(x),-0.8,0.8))
 for (kk in 1:nrow(hasilmutasi)){
 hasilmutasi[kk,]=rank(hasilmutasi[kk,])
 }
 return(hasilmutasi)
 }
 tujuanterbaik <- NULL
 for (generasi in 1:100){
 tujuan <- ngitung.korelasi(populasi)
 terpilih <- populasi[order(tujuan, decreasing=T)[1:5],]
 populasi <- kawin(terpilih, 4,length(skor1))
 tujuanterbaik[generasi] <- (tujuan[order(tujuan, decreasing=T)[1]])
 populasi <- mutasi(populasi)
 }
 tujuan <- ngitung.korelasi(populasi)
 terbaik <- populasi[order(tujuan, decreasing=T)[1],]
 urutanterbaik <- n - terbaik + 1
 GA <- order(urutanterbaik)
 tujuan[order(tujuan, decreasing=T)[1]]
 result <- cbind(result, urutanterbaik)
 result <- result[order(urutanterbaik, decreasing = F),]
 colnames(result) <- c("Variabel", "Information Value", "Information
Gain", "Mean Decrease Accuracy", "Mean Decrese Gini", "Peringkat Genetic
Algorithm")

 n <- length(skor1)
 if ((0 < input$num) & (input$num < n+1)){
 result <- result[1:input$num,]
 }
 else {
 showModal(modalDialog(h2("Proses tidak dapat dijalankan"), "Banyak
variabel prediktor pada dataset:", n, "variabel", sep=""))
 }
 })

 # ===== PLOT GAB =====#
 plotInput5 <- reactive({
 if(is.null(input$data ))
 {return(NULL)}
 else{result <- result()
 plotgab(result)}
 })
 output$plots5 <- renderPlot({
 print(plotInput5())
 })
 output$Gabungan <- downloadHandler(
 filename = function(){
 paste("Kepentingan Variabel", ".png", sep="")
 },
 content = function(file){
 device <- function(..., width, height) grDevices::png(..., width =
width, height = height, res = 300, units = "in")
 ggsave(file,plot = plotInput5(), device = device)
 }
 )

 # ===== PLOT IV =====#
 plotInput1 <- reactive({
 if(is.null(input$data))
 {return(NULL)}
 else{result <- result()
 plotiv(result)}
 })
 output$plots1 <- renderPlot({
 print(plotInput1())
 })
 output$IV <- downloadHandler(
 filename = function(){
 paste("IV", ".png", sep="")
 },
 content = function(file){
 device <- function(..., width, height) grDevices::png(..., width =
width, height = height, res = 300, units = "in")
 ggsave(file,plot = plotInput1(), device = device)
 }
 )

 # ===== PLOT IG =====#
 plotInput2 <- reactive({
 if(is.null(input$data))
 {return(NULL)}
 else{result <- result()
 plotig(result)}
 })
 output$plots2 <- renderPlot({
 print(plotInput2())
 })
 output$IG <- downloadHandler(
 filename = function(){
 paste("IG", ".png", sep="")
 },
 content = function(file){
 device <- function(..., width, height) grDevices::png(..., width =
width, height = height, res = 300, units = "in")
 ggsave(file,plot = plotInput2(), device = device)
 }
 )

 #==== PLOT MDA ====#
 plotInput3 <- reactive({
 if(is.null(input$data))
 {return(NULL)}
 else{result <- result()
 plotmda(result)}
 })
 output$plots3 <- renderPlot({
 print(plotInput3())
 })
 output$MDA <- downloadHandler(
 filename = function(){
 paste("MDA", ".png",sep = "")
 },
 content = function(file){
 device <- function(..., width, height) grDevices::png(..., width =
width, height = height, res = 300, units = "in")
 ggsave(file,plot = plotInput3(), device = device)
 }
 )
 
 #==== Plot MDG ====#
 plotInput4 <- reactive({
 if(is.null(input$data))
 {return(NULL)}
 else{result <- result()
 plotmdg(result)}
 })
 output$plots4 <- renderPlot({
 print(plotInput4())
 })

 output$MDG <- downloadHandler(
 filename = function(){
 paste("MDG", ".png",sep = "")
 },
 content = function(file){
 device <- function(..., width, height) grDevices::png(..., width =
width, height = height, res = 300, units = "in")
 ggsave(file,plot = plotInput4(), device = device)
 }
 )

 output$gab <- renderDataTable({
 if(is.null(input$data))
 {return(NULL)}
 else{
 withProgress(message = 'Proses sedang berjalan...',
 value = 1.0,{
 result <- result()
 })
 n <- nrow(result)
 if ((0 < input$num) & (input$num < n+1)){
 result <- result[1:input$num,]
 }
 else {
 showModal(modalDialog(h2("Proses tidak dapat dijalankan"), "Banyak
variabel prediktor pada dataset:", n, "variabel", sep="", footer =
modalButton('Tutup')))
 }
 if(!is.null(result)){
 showModal(modalDialog("Proses berhasil",footer =
modalButton('Tutup')))
 return(result)
 }
 }
 },
 options=list(dom='ft')
 )
}
shinyApp(ui,server)
```


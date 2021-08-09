#Atur Working Directory
setwd("C:/Users/vania frederica/Documents/Semester 4/Sistem Informasi Manajemen/R-SHINY/Project R-Shiny")


#Panggil library
library(lubridate)
library (dplyr)
library(shiny)
library(shinydashboard)
library(summarytools)
library(shinydashboardPlus)
library(DT)
library(plotly)
library(ggplot2)
library(leaflet)
library(rgdal)
library(padr)
library(rlang)
library(tidyr)
library(data.table)

#Baca Data
datakondisicovid=read.csv("Provinsi1.csv",sep=",",check.names=FALSE)
datajeniskelamin=read.csv("Jeniskelamin1.csv",sep=",",check.names=FALSE)
dataumur=read.csv("Umur1.csv",sep=",",check.names=FALSE)
datagejala=read.csv("gejalapositif1.csv",sep=";",check.names=FALSE)
datakondisipenyerta=read.csv("kondisipenyerta1.csv",sep=";",check.names=FALSE)
df=read.csv("dt_vaksin.csv",sep=";",check.names = F)


#Preprocessing data
datakondisicovid$Tanggal=as.Date(datakondisicovid$Tanggal)
datajeniskelamin$Tanggal=as.Date(datajeniskelamin$Tanggal)
dataumur$Tanggal=as.Date(dataumur$Tanggal)
datagejala$Tanggal=as.Date(datagejala$Tanggal)
datakondisipenyerta$Tanggal=as.Date(datakondisipenyerta$Tanggal)
df$Tanggal=as.Date(df$Tanggal,format="%Y-%m-%d")

datakondisicovid$Provinsi=as.factor(datakondisicovid$Provinsi)
datajeniskelamin$Provinsi=as.factor(datajeniskelamin$Provinsi)
dataumur$Provinsi=factor(dataumur$Provinsi)
datagejala$Provinsi=as.factor(datagejala$Provinsi)
datakondisipenyerta$Provinsi=as.factor(datakondisipenyerta$Provinsi)

datakondisicovid$`Status Covid-19`=as.factor(datakondisicovid$`Status Covid-19`)
datajeniskelamin$`Jenis Kelamin`=as.factor(datajeniskelamin$`Jenis Kelamin`)
dataumur$`Rentang Usia`=factor(dataumur$`Rentang Usia`,levels=c("0-5","6-18","19-30","31-45","45-59",">=60","tidak diketahui"))
datagejala$`Gejala Positif`=factor(datagejala$`Gejala Positif`,levels=c("Tidak ada","Batuk","Demam","Sesak nafas","Sakit kepala","Sakit dada"))
datakondisipenyerta$`Kondisi Penyerta`=factor(datakondisipenyerta$`Kondisi Penyerta`,levels=c("Tidak ada", "Hipertensi", "Diabetes Melitus","Jantung Koroner", "Tuberkolosis", "Kanker"))

#preprocessing data vaksinasi dan shp files
shp <- readOGR(dsn = "SHP Indonesia", layer = "prov")
df_merge= df %>%                                 #merge provinsi untuk membuat peta
  group_by(Provinsi) %>%                            # multiple group columns
  summarise(Jumlah_Target = sum(Jumlah.Target), Jumlah_Vaksinasi1=sum(Jumlah.Vaksinasi.1),
            Jumlah_Vaksinasi2=sum(Jumlah.Vaksinasi.2),.groups = 'drop')
df_merge=data.frame(df_merge)
colnames(df_merge) <- c("NAME_1", "Jumlah Target","Jumlah Vaksinasi 1", "Jumlah Vaksinasi 2")
df_merge[df_merge == "Dki Jakarta"] <- "Jakarta Raya"  #Jakarta Raya, Bangka Belitung, Yogyakarta tidak sama namanya dengan shp file
df_merge[df_merge == "Kepulauan Bangka Belitung"] <- "Bangka Belitung" 
df_merge[df_merge == "Daerah Istimewa Yogyakarta"] <- "Yogyakarta" 
shp_data=merge(shp@data, df_merge,by="NAME_1",sort=F) # Merge data
tes=df %>%                                 #merge provinsi dan target untuk infobox
  group_by(Target,Provinsi) %>%                       
  summarise(Jumlah_Target = sum(Jumlah.Target), Jumlah_Vaksinasi1=sum(Jumlah.Vaksinasi.1),
            Jumlah_Vaksinasi2=sum(Jumlah.Vaksinasi.2),.groups = 'drop')

#Preprocessing data covid untuk membuat peta
cov_merge= datakondisicovid %>%                                 #merge provinsi untuk membuat peta
  group_by(Provinsi,`Status Covid-19`) %>%                            # multiple group columns
  summarise(Total = sum(Total),.groups = 'drop')
cov_merge$Provinsi=tolower(cov_merge$Provinsi)
cov_merge$Provinsi=gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",cov_merge$Provinsi,perl = TRUE) 
cov_merge[cov_merge == "Dki Jakarta"] <- "Jakarta Raya"  #Jakarta Raya, Bangka Belitung, Yogyakarta tidak sama namanya dengan shp file
cov_merge[cov_merge == "Kepulauan Bangka Belitung"] <- "Bangka Belitung" 
cov_merge[cov_merge == "Daerah Istimewa Yogyakarta"] <- "Yogyakarta" 
cov_merge=spread(cov_merge,`Status Covid-19`,Total)
colnames(cov_merge) <- c("NAME_1", "Dirawat","Kumulatif", "Meninggal","Sembuh")
shp_data=merge(shp_data, cov_merge,by="NAME_1",sort=F)
#SHINY
#Header Item
headerItem<-dashboardHeader(title = "Dashboard COVID-19",
                            dropdownMenu(type = "notifications",notificationItem(text = "Website Komite Penanganan Covid-19",href = "https://covid19.go.id/")),
                            dropdownMenu(type = "message",messageItem(from="Temenmu",message = "Ayo ngerjain SIM"))
)

#Sidebar Item
sidebarItem<-dashboardSidebar(
  sidebarMenu(
    menuItem("Pendahuluan", tabName = "Pendahuluan",icon = icon("play")),
    menuItem("Dataset", tabName = "Dataset",icon = icon("table")),
    menuItem("Identitas Pasien", tabName = "Identitas", icon = icon("hospital-user")),
    menuItem("Status COVID-19", tabName = "Status",icon = icon("viruses")),
    menuItem("Vaksinasi", tabName = "Vaksinasi",icon = icon("syringe")),
    menuItem("Peta Persebaran", tabName = "Peta",icon = icon("globe-asia")),
    menuItem("Penyusun", tabName = "Penyusun",icon = icon("users"))
  )
)

#Body Item
bodyItem=dashboardBody(
  tabItems(
    tabItem(tabName = "Pendahuluan",
            column(width = 12,
                   box(
                     title = strong("Latar Belakang"), width = NULL, solidHeader = TRUE,style="text-align: justify;",style = "font-family: 'times'; font-si16pt",
                     "Pandemi COVID-19 telah menjadi bencana kemanusiaan dan mengubah segala aspek kehidupan. Sejak Presiden Joko Widodo mengonfirmasi kasus pertama COVID-19 di Indonesia pada tanggal 
           20 Maret 2020 hingga sekarang, terus terjadi penambahan kasus COVID-19. Mengingat berbahayanya COVID-19 maka pemerintah mengupayakan berbagai cara untuk mencegah penularan COVID-19 yakni dengan menggencarkan gerakan protokol kesehatan dan vaksinasi.
           Baik kondisi COVID-19 maupun vaksinasi di setiap daerah tentunya selalu terbaharui secara real time sehingga dengan adanya beragam informasi dan kecepatan keterbaruan data tersebut maka akan lebih baik apabila semua data COVID-19 dan vaksinasi dijadikan dalam satu database dan dapat divisualisasikan secara menarik dan user friendly secara transparan sehingga masyarakat dapat memantau secara real time."
                   )
            ),
            column(width = 12,
                   box(
                     title = strong("Tujuan"), width = NULL, solidHeader = TRUE,style="text-align: justify;",
                     p("Adapun yang menjadi tujuan dari penelitian ini adalah :",style = "font-family: 'times'; font-si16pt"),
                     p("1.Merancang dashboard dengan tujuan mengetahui perkembangan kondisi pasien, situasi COVID-19, dan perkembangan vaksinasi di Indonesia.",style = "font-family: 'times'; font-si16pt;margin-top: 2px"),
                     p( "2.Mengetahui profil identitas pasien seperti persebaran penduduk berdasarkan usia, jenis kelamin, kondisi penyerta dan gejala. ",style = "font-family: 'times'; font-si16pt"),
                     p("3.Memonitor kondisi dan mengetahui pembaharuan perkembangan situasi COVID-19 dan vaksinasi dari waktu ke waktu. ",style = "font-family: 'times'; font-si16pt"),
                     p(" 4.Mengetahui persebaran wilayah/provinsi dari situasi COVID-19 dan vaksinasi di Indonesia.",style = "font-family: 'times'; font-si16pt")
                   )
            ),
            column(width = 12,
                   box(
                     title = strong("Manfaat"), width = NULL, solidHeader = TRUE,style="text-align: justify;",
                     p("Adapun yang menjadi manfaat dari penelitian ini adalah :",style = "font-family: 'times'; font-si16pt"),
                     p("1.Sebagai dasar pengambilan keputusan bagi pemerintah dan instansi kesehatan.",style = "font-family: 'times'; font-si16pt"),
                     p("2.Sebagai media informasi yang dapat menyajikan informasi secara efisien kepada pemerintah/instansi terkait mengenai identitas pasien, status COVID-19 dan vaksinasi di Indonesia. ",style = "font-family: 'times'; font-si16pt"),
                     p("3.Sebagai media monitoring untuk dapat memantau progress atau perkembangan dari situasi covid dan vaksinasi di Indonesia. ",style = "font-family: 'times'; font-si16pt"),
                     p("4.Sebagai dasar untuk pembaharuan informasi terkini perkembangan status COVID-19 dan vaksinasi di Indonesia",style = "font-family: 'times'; font-si16pt")
                   )
            )
    ),
    tabItem(tabName = "Dataset",
            tabsetPanel(
              tabPanel("DataFrame",
                       fluidPage( selectInput("dataset",label = "Dataset",
                                              choices = c("Kondisi covid","Jenis kelamin","Umur","Gejala Penyakit","Kondisi Penyerta","Vaksinasi")),
                                  DTOutput("data_table"))
              ),
              tabPanel("Summary",verbatimTextOutput("summary_stat1"))
            ))
    
    ,
    tabItem(tabName="Identitas",
            fluidRow(
              box(
                title = "", status = "success", solidHeader = F,
                collapsible = F,width=6,
              selectInput(inputId = "prov1",label = "Provinsi:",
                          choices = unique(datajeniskelamin$Provinsi),
                          selected =unique(datajeniskelamin$Provinsi)[1]))
              ,
              box(
                title = "", status = "success", solidHeader = F,
                collapsible = F,width=6,
              selectInput("date1",
                          label = "Tanggal:",
                          choices = unique(datajeniskelamin$Tanggal),
                          selected =unique(datajeniskelamin$Tanggal)[1]))
              
              ,
              box(
                title = "Pie Chart Jenis Kelamin", status = "success", solidHeader = F,
                collapsible = TRUE,
                plotOutput(outputId = "graph_1", height = 250)
              )
              
              ,
              box(
                title = "Histogram", status = "success", solidHeader = F,
                collapsible = TRUE,
                plotlyOutput("graph_2", height = 250)
              )
              ,
              box(
                title = "Bar Chart Kondisi Penyerta", status = "success", solidHeader = F,
                collapsible = TRUE,
                plotlyOutput("graph_3", height = 250)
              )
              ,
              
              box(
                title = "Bar Chart Gejala Penyakit", status = "success", solidHeader = F,
                collapsible = TRUE,
                plotlyOutput("graph_4", height = 250)
              )
            )
    )
    ,
    tabItem(tabName = "Status",
            fluidRow(
              box(
                title = "Overview Situasi COVID-19 di Indonesia", 
                closable = FALSE, 
                enable_label = TRUE,
                label_status = "danger",
                status = "success", 
                solidHeader = FALSE, 
                collapsible = TRUE,
                width = 12,
                
                selectInput(inputId = "pro1",label = "Provinsi:",
                            choices = unique(datakondisicovid$Provinsi),
                            selected =unique(datakondisicovid$Provinsi)[1]),
                
                infoBoxOutput(
                  outputId = "rw", 
                  width = 4
                ),
                
                infoBoxOutput(
                  outputId = "sm",
                  width = 4
                ),
                infoBoxOutput(
                  outputId = "mg",
                  width = 4))),
         fluidRow(
            box(
              title = "Line Chart Kumulatif berdasarkan Provinsi", 
              closable = FALSE, 
              enable_label = TRUE,
              label_status = "danger",
              status = "success", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              width = 12,
              plotlyOutput(outputId = "cum_line")),
           box(
             title = "Status Pasien COVID-19 se-Indonesia (Harian)",
             closable = FALSE, 
             enable_label = TRUE,status = "success", solidHeader = F,
             collapsible = TRUE,width=12
             ,
             selectInput(inputId = "status1",label = "Status Pasien COVID-19:",
                         choices = c("dirawat","sembuh","meninggal"),
                         selected ="dirawat")
             ,
             plotlyOutput(outputId = "graph11", height = 250)
           ),
         box(
             title = "Top 10 Provinsi Kasus COVID-19", status = "success", solidHeader = F,
             closable = FALSE, 
             enable_label = TRUE,
             collapsible = TRUE,width=12,
             plotlyOutput(outputId = "graph12", height = 500)
           ))),
    tabItem(tabName = "Vaksinasi",
            fluidRow(
              box(
                title = "Overview Vaksinasi di Indonesia", 
                closable = FALSE, 
                enable_label = TRUE,
                label_status = "danger",
                status = "success", 
                solidHeader = FALSE, 
                collapsible = TRUE,
                width = 12,
                
                selectInput(inputId = "prov",label = "Provinsi:",
                            choices = unique(tes$Provinsi),
                            selected =unique(tes$Provinsi)[1]),
                
                infoBoxOutput(
                  outputId = "TK", 
                  width = 4
                ),
                
                infoBoxOutput(
                  outputId = "Lansia",
                  width = 4
                ),
                infoBoxOutput(
                  outputId = "PP",
                  width = 4
                )
              )
            ),
            fluidRow( 
              box(title = "Top 3 Provinsi dengan Jumlah Terbanyak", 
                  closable = FALSE, 
                  enable_label = TRUE,
                  label_status = "danger",
                  status = "success", 
                  solidHeader = FALSE, 
                  collapsible = TRUE,
                  width=12,
                  
                  selectInput(inputId = "var",label = "Pilih yang ingin ditampilkan:",
                              choices = c("Target","Vaksinasi 1","Vaksinasi 2"),
                              selected ="Vaksinasi 1"),
                  
                  selectInput(inputId = "target_cat",label = "Target: ",
                              choices = c("Tenaga Kesehatan","Lansia","Pelayan Publik"),selected ="Tenaga Kesehatan"),
                  plotlyOutput(outputId = "graph"))),
            fluidRow(
              box(title = "Penduduk yang Telah Divaksinasi (Harian)", 
                  closable = FALSE, 
                  enable_label = TRUE,
                  label_status = "danger",
                  status = "success", 
                  solidHeader = FALSE, 
                  collapsible = TRUE,
                  width=12,height = 1500,
                  sliderInput("date",
                              label = "Tanggal :",
                              min = as.Date("2021-03-05","%Y-%m-%d"),
                              max = as.Date("2021-06-14","%Y-%m-%d"),
                              value=as.Date("2021-03-05"),timeFormat="%Y-%m-%d",ticks=F),
                  splitLayout(cellWidths = c("40%","60%"),plotlyOutput(outputId = "graph_group"),
                              plotlyOutput(outputId = "graph_group1"))
                  
                  
              )
            )
    ),
    tabItem(tabName = "Peta",
            fluidRow(
              box(
                title = "Peta (Kumulatif)", 
                closable = FALSE, 
                enable_label = TRUE,
                label_status = "danger",
                status = "success", 
                solidHeader = FALSE, 
                collapsible = TRUE,
                width = 12,
                height = "600px",
                selectInput(inputId = "Choose",label = "Pilih yang ingin ditampilkan:",
                            choices = c("Pasien COVID-19 yang dirawat","Pasien COVID-19 yang sembuh","Pasien COVID-19 yang meninggal",
                                        "Target Vaksinasi","Vaksinasi 1", "Vaksinasi 2"),
                            selected ="Pasien COVID-19 yang dirawat"),
                leafletOutput('graph_map', height = 600))))
    ,
    
    tabItem(tabName = "Penyusun",
              column(width=6,
                   box(
                     title = strong("Anggota 1"), width = NULL, solidHeader = TRUE,style="text-align: justify;",style = "font-family: 'times'; font-si16pt",
                     div(imageOutput("foto1"),style="text-align: center;",style = "height:205px;"),
                     p("Nama     = Vania Frederica"),
                     p("Nrp      = 06211940000006"),
                     p("Email    = vaniafrederica777@gmail.com"),
                     p("LinkedIn = https://www.linkedin.com/in/vania-frederica/")
                   )),
            column(width = 6,
                   box(
                     title = strong("Anggota 2"), width = NULL, solidHeader = TRUE,style="text-align: justify;",style = "font-family: 'times'; font-si16pt",
                     div(imageOutput("foto2"),style="text-align: center;",style = "height:205px;"),
                     p("Nama     = Nanda Novenia Shinta Hapsari"),
                     p("Nrp      = 06211940000008"),
                     p("Email    = nandanoveniash@gmail.com"),
                     p("LinkedIn = https://www.linkedin.com/in/nanda-novenia-s-14a709113/")
                   )
            )
           
    )))




#Server
server = function(input, output,session) {
  
  data_kondisi_covid=data.frame(datakondisicovid)
  data_umur=data.frame(dataumur)
  data_jenis_kelamin=data.frame(datajeniskelamin)
  data_gejala=data.frame(datagejala)
  data_kondisi_penyerta=data.frame(datakondisipenyerta)
  data_vaksinasi=data.frame(df)
  
  datasetInput <- reactive({
    if (input$dataset == "Kondisi covid"){
      dataset <- data_kondisi_covid
    }
    else if (input$dataset == "Jenis kelamin"){
      dataset <- data_jenis_kelamin
    }
    else if (input$dataset == "Umur"){
      dataset <- data_umur
    }
    else if (input$dataset == "Kondisi Penyerta"){
      dataset <- data_kondisi_penyerta
    }
    else if(input$dataset == "Gejala Penyakit"){
      dataset <- data_gejala
    }
    else if(input$dataset == "Vaksinasi"){
      dataset <- data_vaksinasi
    }
    return(dataset)
  })
  output$data_table <- renderDT({ datasetInput()})
  output$summary_stat1=renderPrint(dfSummary({datasetInput()}))
  
  #Identitas
  output$graph_1=renderPlot({
  data1=datajeniskelamin%>%
      select(Tanggal,Provinsi,`Jenis Kelamin`,Jumlah)%>%
      filter(Provinsi==input$prov1,Tanggal==input$date1)
    
    data2=data1%>%
      group_by(`Jenis Kelamin`)%>%
      summarise(sum((Jumlah)))%>%
      mutate(persen=round((`sum((Jumlah))`/sum(`sum((Jumlah))`)*100),2))%>%
      mutate(ypos = cumsum(persen)- 0.5*persen )
    
    pie=ggplot(data=data2,aes(x="", y=persen, fill=`Jenis Kelamin`))+
      geom_bar( stat="identity", width = 1)+
      coord_polar("y")+theme_void()+
      geom_text(aes(label = paste(round((`sum((Jumlah))`/sum(`sum((Jumlah))`)*100),2), "%")),
                position = position_stack(vjust = 0.5))
    pie
    
  })
  output$graph_2=renderPlotly({
    data_2=dataumur%>%
      select(Tanggal,Provinsi,`Rentang Usia`,Jumlah)%>%
      filter(Provinsi==input$prov1, Tanggal==input$date1)%>%
      group_by(`Rentang Usia`)%>%
      summarise(sum((Jumlah)))%>%
      mutate(jumlah= `sum((Jumlah))`)
    
    data_2$label <- scales::percent(data_2$jumlah)
    
    bar=ggplot(data=data_2,aes(x=`Rentang Usia`, y=jumlah, fill=`Rentang Usia`,text=paste("Rentang usia :",`Rentang Usia`,"<br>Jumlah :",jumlah)))+
      geom_bar( stat="identity", width = 1)+theme_void()
    ggplotly(bar,tooltip="text")%>% 
      config(displayModeBar = F) %>% 
      layout(autosize = TRUE)
  })
  output$graph_3=renderPlotly({
   data3=datakondisipenyerta%>%
      select(Tanggal,Provinsi,`Kondisi Penyerta`,Total)%>%
      filter(Provinsi==input$prov1,Tanggal==input$date1)%>%
      group_by(`Kondisi Penyerta`)%>%
      summarise(sum((Total)))%>%
      mutate(jumlah= `sum((Total))`)
    
    data3$label <- scales::percent(data3$jumlah)
    
    bar1=ggplot(data=data3)+
      geom_bar(aes(x=`Kondisi Penyerta`, y=jumlah, fill=`Kondisi Penyerta`,text=paste("Kondisi penyerta :",`Kondisi Penyerta`,"<br>Jumlah :",jumlah)), stat="identity", width = 1)+
      theme_void()
    ggplotly(bar1,tooltip="text")%>% 
      config(displayModeBar = F) %>% 
      layout(autosize = TRUE)
  })
  output$graph_4=renderPlotly({
   data4=datagejala%>%
      select(Tanggal,Provinsi,`Gejala Positif`,Total)%>%
      filter(Provinsi==input$prov1,Tanggal==input$date1)%>%
      group_by(`Gejala Positif`)%>%
      summarise(sum((Total)))%>%
      mutate(jumlah= `sum((Total))`)
    
    data4$label <- scales::percent(data4$jumlah)
    
    bar2=ggplot(data=data4)+
      geom_bar(aes(x=`Gejala Positif`, y=jumlah, fill=`Gejala Positif`,text=paste("Gejala positif :",`Gejala Positif`,"<br>Jumlah :",jumlah)), stat="identity", width = 1)+
      theme_void()
    ggplotly(bar2,tooltip="text")%>% 
      config(displayModeBar = F) %>% 
      layout(autosize = TRUE)
  })
  
  #Situasi COVID-19  
  output$rw <- renderInfoBox({
    summ <- datakondisicovid %>% 
      filter(Provinsi==input$pro1)
    DR1=summ %>% filter(`Status Covid-19`=='dirawat')%>%
      summarise(DIR=sum(Total))
    infoBox(title = tags$p("Total Dirawat",style="font-weight:bold;"),
            value = DR1 %>% pull(DIR),
            color = "blue",
            icon = icon("wheelchair"))
  })
  output$sm <- renderInfoBox({
    summ <- datakondisicovid %>% 
      filter(Provinsi==input$pro1)
    
    summ1 <- datakondisicovid %>% 
      filter(Provinsi==input$pro1)%>%
      filter(`Status Covid-19`!='kumulatif')%>%
      summarise(SUM1=sum(Total))
    
    SM1=summ %>% filter(`Status Covid-19`=='sembuh')%>%
      summarise(SEM=sum(Total))
    infoBox(title = tags$p("Total Sembuh",style="font-weight:bold;"),
            value = SM1 %>% pull(SEM),
            tags$p(paste(round((SM1/summ1),2)*100,"%"),style="font-weight:bold;"),
            color = "green",
            icon = icon("child"))
  })
  output$mg <- renderInfoBox({
    summ <- datakondisicovid %>% 
      filter(Provinsi==input$pro1)
    
    summ2 <- datakondisicovid %>% 
      filter(Provinsi==input$pro1)%>%
      filter(`Status Covid-19`!='kumulatif')%>%
      summarise(SUM1=sum(Total))
    
    MN1=summ %>% filter(`Status Covid-19`=='meninggal')%>%
      summarise(MEN=sum(Total))
    infoBox(title = tags$p("Total Meninggal",style="font-weight:bold;"),
            value = MN1 %>% pull(MEN),
            tags$p(paste(round((MN1/summ2),2)*100,"%"),style="font-weight:bold;"),
            color = "red",
            icon = icon("people-carry"))
  })
  output$cum_line=renderPlotly({
    summ <- datakondisicovid %>% 
      filter(Provinsi==input$pro1,`Status Covid-19` != "kumulatif")%>%mutate(Total1=Total/1000)
    summ <- setDT(summ)
    summ[,cumulsum :=cumsum(Total1), by = `Status Covid-19`]
    plot6=summ%>%ggplot(aes(x = Tanggal, y = cumulsum, group = `Status Covid-19`,group=1,
                        text=paste("Tanggal :",Tanggal,"<br>Status COVID-19 :",`Status Covid-19`,"<br>Jumlah (dalam ribuan):",cumulsum)))+
      geom_line(colour="cornsilk2")+ylab("Total Pasien (dalam ribuan)")+geom_ribbon(aes(ymin=0,ymax=cumulsum,fill=`Status Covid-19`),alpha=0.5)+
      scale_fill_manual(values = c("steelblue","#E63946","#B1E8A9"))+theme_minimal()+ theme(axis.title = element_text(face="bold"))
    ggplotly(plot6,tooltip="text")%>% 
      config(displayModeBar = F)%>% 
      layout(autosize = TRUE)})
  output$graph11=renderPlotly({
    data_11=datakondisicovid%>%
      select(Tanggal,Provinsi,`Status Covid-19`,Total)%>%
      filter(`Status Covid-19`==input$status1)%>%
      group_by(Tanggal)%>%
      summarise(sum((Total)))%>%
      mutate(jumlah= `sum((Total))`)
    
    line=ggplot( data_11,aes(x = Tanggal, y =jumlah)) +
      geom_line(colour="red")+xlab("Tanggal") +ylab("Total Pasien")+geom_vline(xintercept=as.numeric(data_11$Tanggal[27]), linetype="dashed", color = "blue")+
      geom_text(aes(x=data_11$Tanggal[29], label=paste0("Larangan mudik berakhir"), y=mean(jumlah)),size=3.4,colour="blue")+
      theme_minimal()+theme(plot.title = element_text(face='bold'),axis.title = element_text(size=11, face = "bold"))
  
      ggplotly(line)%>% 
      config(displayModeBar = F) %>% 
      layout(autosize = TRUE)
  })
  output$graph12=renderPlotly({
    data_13=as.character(datakondisicovid%>%
      filter(`Status Covid-19`!="kumulatif")%>%
      group_by(Provinsi)%>%
      summarise(Jumlah = sum(Total))%>% 
      arrange(desc(Jumlah)) %>% 
      head(10)%>%pull(Provinsi))
    
    data_12=datakondisicovid%>%
      filter(Provinsi%in%data_13,`Status Covid-19`!="kumulatif")%>%
      mutate(total1=Total/1000)
    data_12$Provinsi=as.character(data_12$Provinsi)
    bar2=ggplot(data_12,aes(fill=`Status Covid-19`,x=reorder(Provinsi,-total1),y=total1,text=paste("Provinsi :",reorder(Provinsi,-total1),"<br>Status COVID-19 :",`Status Covid-19`,"<br>Jumlah (dalam ribuan):",sum(total1))))+
      geom_bar(position='stack', stat='identity')+
      scale_fill_manual(values=c('steelblue','coral2', '#B1E8A9'))+labs(x = 'Provinsi',y = 'Total Pasien (dalam ribuan)',title ="")+
      theme_minimal()+theme(axis.text.y = element_text(size=6.5),plot.title = element_text(hjust = 0.5,face='bold'),legend.title = element_blank(),
            axis.title = element_text(size=9,face="bold"),
            axis.text.x = element_text(angle = 45))
    ggplotly(bar2,tooltip="text")%>% 
      config(displayModeBar = F) %>% 
      layout(autosize = TRUE)
    
    
  })
  
  #Vaksinasi
  output$TK <- renderInfoBox({
    summ <- tes %>% 
      filter(Provinsi==input$prov)
    TK1=summ %>% filter(Target=='Tenaga Kesehatan')
    infoBox(title = tags$p("Total Tenaga Kesehatan",style="font-weight:bold;"),
            tags$p(paste("Target      = ",TK1$Jumlah_Target),tags$br(),paste("Vaksinasi 1 = ",TK1$Jumlah_Vaksinasi1),tags$br(),
                   paste("Vaksinasi 2 = ",TK1$Jumlah_Vaksinasi2),style="font-size:75%;font-weight:lighter;"),
            color = "red",
            icon = icon("user-md"))
  })
  output$Lansia <- renderInfoBox({
    summ <- tes %>% 
      filter(Provinsi==input$prov)
    L1=summ %>% filter(Target=='Lansia')
    infoBox(title = tags$p("TOTAL LANSIA",style="font-weight:bold;"),
            tags$p(paste("Target      = ",L1$Jumlah_Target),tags$br(),paste("Vaksinasi 1 = ",L1$Jumlah_Vaksinasi1),tags$br(),
                   paste("Vaksinasi 2 = ",L1$Jumlah_Vaksinasi2),style="font-size:75%;font-weight:lighter;"),
            color = "blue",
            icon = icon("blind"))
    
  })
  output$PP <- renderInfoBox({
    summ <- tes %>% 
      filter(Provinsi==input$prov)
    PP1=summ %>% filter(Target=='Pelayan Publik')
    infoBox(title = tags$p("TOTAL PELAYAN PUBLIK",style="font-weight:bold;"),
            tags$p(paste("Target      = ",PP1$Jumlah_Target),tags$br(),paste("Vaksinasi 1 = ",PP1$Jumlah_Vaksinasi1),tags$br(),
                   paste("Vaksinasi 2 = ",PP1$Jumlah_Vaksinasi2),style="font-size:75%;font-weight:lighter;"),
            color = "green",
            icon = icon("user-tie"))
  })
  output$graph=renderPlotly({
    if(input$var=="Vaksinasi 1"){
      top_province <- df %>% 
        filter(Target==input$target_cat)%>%
        group_by(Target, Provinsi)%>%
        summarise(
          Jumlah = sum(Jumlah.Vaksinasi.1)
        )%>% arrange(desc(Jumlah)) %>% 
        head(3)%>% 
        pull(Provinsi)
      
      data_viz <- df %>% 
        filter(
          Provinsi %in% top_province,
          Target==input$target_cat
        )%>% 
        group_by(Tanggal, Provinsi) %>% 
        summarise(Jumlah = sum(Jumlah.Vaksinasi.1)) %>% 
        group_by(Provinsi) %>% 
        pad() %>% 
        ungroup()
    }else if(input$var=="Vaksinasi 2"){
      top_province <- df %>% 
        filter(Target==input$target_cat)%>%
        group_by(Target, Provinsi)%>%
        summarise(
          Jumlah = sum(Jumlah.Vaksinasi.2)
        )%>% arrange(desc(Jumlah)) %>% 
        head(3)%>% 
        pull(Provinsi)
      
      data_viz <- df %>% 
        filter(
          Provinsi %in% top_province,
          Target==input$target_cat
        )%>% 
        group_by(Tanggal, Provinsi) %>% 
        summarise(Jumlah = sum(Jumlah.Vaksinasi.2)) %>% 
        group_by(Provinsi) %>% 
        pad() %>% 
        ungroup()
    }else{
      top_province <- df %>% 
        filter(Target==input$target_cat)%>%
        group_by(Target, Provinsi)%>%
        summarise(
          Jumlah = sum(Jumlah.Target)
        )%>% arrange(desc(Jumlah)) %>% 
        head(3)%>% 
        pull(Provinsi)
      
      data_viz <- df %>% 
        filter(
          Provinsi %in% top_province,
          Target==input$target_cat
        )%>% 
        group_by(Tanggal, Provinsi) %>% 
        summarise(Jumlah = sum(Jumlah.Target)) %>% 
        group_by(Provinsi) %>% 
        pad() %>% 
        ungroup()
    }
    plot1 <- data_viz %>% 
      ggplot(aes(x = Tanggal,y =Jumlah,colour=Provinsi)) +
      geom_line()+
      facet_wrap(facets = vars(Provinsi), nrow = 3, scales = "free_y") +
      theme_minimal()+theme(plot.title = element_text(face='bold'),axis.title.x = element_text(size=11, face = "bold"))
    ggplotly(plot1) %>% 
      config(displayModeBar = F) %>% 
      layout(autosize = TRUE)})
  output$graph_group=renderPlotly({
    df1=df %>% 
      filter(Tanggal==input$date)
    graph_1=ggplot(df1,aes(fill=Target,x=reorder(Provinsi,Jumlah.Vaksinasi.1), y=Jumlah.Vaksinasi.1,group=1,
                           text=paste("Provinsi :",reorder(Provinsi,Jumlah.Vaksinasi.1),"<br>Target :",Target,"<br>Jumlah :",Jumlah.Vaksinasi.1))) + 
      geom_bar(position='stack', stat='identity')+coord_flip()+
      scale_fill_manual('Position', values=c("#118AB2", "#06D6A0", "#EF476F"))+labs(x = ' ',y = 'Jumlah Orang',title ="Vaksinasi 1")+
      theme_minimal()+theme(legend.position = "none",axis.text.y = element_text(size=6.5),plot.title = element_text(hjust = 0.5,face='bold'),
            axis.title.y = element_blank(),axis.title.x = element_text(size=9,face="bold"))
    ggplotly(graph_1,tooltip="text")%>% 
      config(displayModeBar = F) %>% 
      layout(autosize = TRUE) 
  })
  output$graph_group1=renderPlotly({
    df2=df %>% 
      filter(Tanggal==input$date)
    graph_2=ggplot(df2,aes(fill=Target,x=reorder(Provinsi,Jumlah.Vaksinasi.2), y=Jumlah.Vaksinasi.2,group=1,
                           text=paste("Provinsi :",reorder(Provinsi,Jumlah.Vaksinasi.2),"<br>Target :",Target,"<br>Jumlah :",Jumlah.Vaksinasi.2))) + 
      geom_bar(position='stack', stat='identity')+coord_flip()+
      scale_fill_manual(values=c("#118AB2", "#06D6A0", "#EF476F"))+labs(x = ' ',y = 'Jumlah Orang',title ="Vaksinasi 2")+
      theme_minimal()+theme(axis.text.y = element_text(size=6.5),plot.title = element_text(hjust = 0.5,face='bold'),legend.title = element_blank(),
            axis.title.x = element_text(size=9,face="bold"))
    ggplotly(graph_2,tooltip="text")%>% 
      config(displayModeBar = F) %>% 
      layout(autosize = TRUE) 
  })
  output[["foto1"]]<-renderImage({
    list(src = "www/vania.jpeg", height = 200, width = 180)
    },deleteFile = F)
  output[["foto2"]]<-renderImage({
      list(src = "www/nanda.jpg", height = 200, width = 200)
    },deleteFile = F)
  
  #Peta
  output$graph_map=renderLeaflet({
    if(input$Choose=="Vaksinasi 1"){
      n=5
    }
    else if(input$Choose=="Vaksinasi 2"){
      n=6
    }
    else if(input$Choose=="Pasien COVID-19 yang dirawat"){
      n=7
    }
    else if(input$Choose=="Pasien COVID-19 yang sembuh"){
      n=10
    }
    else if(input$Choose=="Pasien COVID-19 yang meninggal"){
      n=9
    }else{
      n=4
    }
    mypalette <- colorNumeric( palette="OrRd", domain=shp_data[,n], na.color="transparent")
    mytext <- paste("Provinsi:", shp_data$NAME_1,", Jumlah :", shp_data[,n])
    m <- leaflet(shp) %>% 
      addTiles()  %>% 
      
      setView( lat=-3, lng=117.7, zoom=5) %>%
      addPolygons( 
        fillColor = ~mypalette(shp_data[,n]), 
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="white", 
        weight=0.3,
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegend( pal=mypalette, values=~shp_data[,n], opacity=0.9, 
                 title = "Jumlah orang", position = "bottomleft" )
    m
  })
  
  }


#Jalankan program shiny
ui<-dashboardPage(skin="green-light",
                  header=headerItem,
                  sidebar = sidebarItem,
                  body = bodyItem)


shinyApp(ui,server)


library(ggplot2)
library(ggthemes)
library(readxl)
library(shinydashboard)
library(shiny)
library(shinyjs)
library(scales)
library(hrbrthemes)
library(dplyr)
library(readr)
library(ggmap)
library(leaflet)
#library(rMaps)
library(plotly)
library(data.table)
library(reshape2)
library(tidyr)
library(directlabels)
library(DT)
#library(ggrepel)
#library(gglabeller)
#Times_d1 <- read_excel("~/Desktop/HOMEWORK - R/R_PROJECT/Test_Times_World_University_dataset_V2.xlsx", 
#                       sheet = "Times_World_University_Rankings"
#Times_d1_lonlat <- read_excel("~/Desktop/HOMEWORK - R/R_PROJECT/Test_Times_World_University_dataset_V2.xlsx", 
#                              sheet = "Distinct_University_Geocodes")
#Times_Full <- left_join(Times_d1, Times_d1_lonlat, by = c("University_Name", "University_Name"))

#Links
ylinks = matrix(c(
  '<b>Times Higher Education</b>', '<b>IPEDS</b>','<b>The World Bank</b>', '<a href="https://www.timeshighereducation.com/">Times Higher Education</a>',
  '<a href="https://nces.ed.gov/ipeds/Home/UseTheData">IPEDS</a>',
  '<a href="https://data.worldbank.org/">The World Bank</a>' 
), 3)
colnames(ylinks) = c('<span style="color:red">Name</span>', '<em>Link</em>')
datatable(ylinks, escape = FALSE)

Times_Full_1 <- read_excel("~/Desktop/HOMEWORK/data_with_countrycode.xlsx", na = "null")

ROI_84Univ <- read_excel("~/Desktop/HOMEWORK/ROI_84Univ.xlsx", 
                         col_types = c("numeric", "text", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "skip", "skip", "skip", "skip", 
                                       "skip", "skip", "skip", "skip", 
                                       "skip", "skip", "skip", "skip", 
                                       "numeric", "skip", "skip"), na = "null")

Times_Full <-merge(x=Times_Full_1, y = ROI_84Univ, by  =c("University_Name","Year"), all.x = TRUE)

GDP_Growth <- read_excel("~/Desktop/HOMEWORK/GDP_Growth_Unemployment.xlsx",sheet = "GDP_Growth_Rate", na = "null")
colnames(GDP_Growth)[1] <- "Country_Code"
GDP_Growth_Long <- gather(GDP_Growth,Year,'GDP_Growth_Rate',-Country_Code)

Unemp_Only <- read_excel("~/Desktop/HOMEWORK/GDP_Growth_Unemployment.xlsx",sheet = "Umemployment_Rate", na = "null")
colnames(Unemp_Only)[1] <- "Country_Code"
Unemp_Only_Long <- gather(Unemp_Only,Year,'Umemployment_Rate',-Country_Code)

Edu_GDP_Only <- read_excel("~/Desktop/HOMEWORK/GDP_Growth_Unemployment.xlsx",sheet = "Education_Expenditure_GDP",na = "null")
colnames(Edu_GDP_Only)[1] <- "Country_Code"
Edu_GDP_Only_Long <- gather(Edu_GDP_Only,Year,'Education_Expenditure_GDP',-Country_Code)

Edu_All_Only <- read_excel("~/Desktop/HOMEWORK/GDP_Growth_Unemployment.xlsx",sheet = "Education_Exp_Tertiary",na = "null")
colnames(Edu_All_Only)[1] <- "Country_Code"
Edu_All_Only_Long <- gather(Edu_All_Only,Year,'Education_Exp_Tertiary',-Country_Code)

Imp_All_Only <- read_excel("~/Desktop/HOMEWORK/GDP_Growth_Unemployment.xlsx",sheet = "Import_Growth_Rate",na = "null")
colnames(Imp_All_Only)[1] <- "Country_Code"
Imp_All_Only_Long <- gather(Imp_All_Only,Year,'Import_Growth_Rate',-Country_Code)

Exp_All_Only <- read_excel("~/Desktop/HOMEWORK/GDP_Growth_Unemployment.xlsx",sheet = "Export_Growth_Rate",na = "null")
colnames(Exp_All_Only)[1] <- "Country_Code"
Exp_All_Only_Long <- gather(Exp_All_Only,Year,'Export_Growth_Rate',-Country_Code)

Ind_All_Only <- read_excel("~/Desktop/HOMEWORK/GDP_Growth_Unemployment.xlsx",sheet = "Industry_Growth_Rate",na = "null")
colnames(Ind_All_Only)[1] <- "Country_Code"
Ind_All_Only_Long <- gather(Ind_All_Only,Year,'Industry_Growth_Rate',-Country_Code)

GNI_All_Only <- read_excel("~/Desktop/HOMEWORK/GDP_Growth_Unemployment.xlsx",sheet = "GNI_Growth_Rate",na = "null")
colnames(GNI_All_Only)[1] <- "Country_Code"
GNI_All_Only_Long <- gather(GNI_All_Only,Year,'GNI_Growth_Rate',-Country_Code)

GDP_Actual_Only <- read_excel("~/Desktop/HOMEWORK/GDP_Growth_Unemployment.xlsx",sheet = "GDP_Actual", na = "null")
colnames(GDP_Actual_Only)[1] <- "Country_Code"
GDP_Actual_Long <- gather(GDP_Actual_Only,Year,'GDP_Actual',-Country_Code)

GDP_All_1 <- merge(GDP_Growth_Long, Unemp_Only_Long, by=c("Country_Code","Year"))
GDP_All_2 <- merge(GDP_All_1, Edu_GDP_Only_Long, by=c("Country_Code","Year"))
GDP_All_3 <- merge(GDP_All_2, Edu_All_Only_Long, by=c("Country_Code","Year"))
GDP_All_4 <- merge(GDP_All_3, Imp_All_Only_Long, by=c("Country_Code","Year"))
GDP_All_5 <- merge(GDP_All_4, Exp_All_Only_Long, by=c("Country_Code","Year"))
GDP_All_6 <- merge(GDP_All_5, Ind_All_Only_Long, by=c("Country_Code","Year"))
GDP_All_7 <- merge(GDP_All_6, GNI_All_Only_Long, by=c("Country_Code","Year"))

GDP_All_8 <- merge(GDP_All_7, GDP_Actual_Long, by=c("Country_Code","Year"))

Country_Times_Full <- unique(Times_Full %>% select(Country,Country_Code))

GDP_All <- left_join(GDP_All_8, Country_Times_Full, by=c("Country_Code","Country_Code"))

#Top rank universities dataset

Times_Merge_GDP_All <- merge(x=Times_Full, y=GDP_All, by=c("Country_Code","Year"), all.x = TRUE)
#View(Times_Merge_GDP_All)
Times_All_High_5 <- Times_Merge_GDP_All %>% group_by(Country_Code,Year) %>% top_n(n=5, wt = -World_Rank)
#View(Times_All_High_5)

#Grad Rates
rev_tuition_grate2012_2016 <- read_excel("~/Desktop/HOMEWORK/mano_grad_tui_rev_exp_adm.xlsx", na = "null")

Times_Grad_Full <- merge(x=Times_Full,y=rev_tuition_grate2012_2016, by = c("University_Name","Year"), all.x = TRUE)

Times_Grad_Full$avg_tuition_amt <- 
  with(Times_Grad_Full,((((as.numeric(In_Tuition)+as.numeric(In_Fees))*Domestic_Students_Per)+
                           ((as.numeric(Out_Tuition)+as.numeric(Out_Fees))*Inter_Students_Per))*0.01))

Times_Grad_Full$avg_tuition_amt_aid <- 
  with(Times_Grad_Full,((((as.numeric(In_Tuition)+as.numeric(In_Fees))*Domestic_Students_Per)+
((as.numeric(Out_Tuition)+as.numeric(Out_Fees))*Inter_Students_Per))*0.01)-(Avg_amt_aids*(Num_of_student_aids/Tot_Undergrads)))

#Times_Grad_Full_UTSA <- Times_Grad_Full %>% filter(University_Name  == "University of Texas at San Antonio")

Times_Grad_Full_UTSA <- filter (Times_Grad_Full, grepl('University of Texas', University_Name))


#unique university list USA only as it is default
Times_Full_USA <- Times_Full %>% filter(Country  == "United States of America")

U_univ <- sort(unique(Times_Full_USA$University_Name))

#Unique year
U_year <- sort(unique(Times_Full$Year))

#Unique Country
U_country <- sort(unique(Times_Full$Country))

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "World Class Education", titleWidth = 1400),
  dashboardSidebar(
    tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    sidebarMenu( id = "sbmenu",
                 tags$style(HTML(
                   '.selectize-input { padding: 5px; min-height: 5;}', 
                   '.selectize-dropdown { line-height: 7px; }')),
                 menuItem(tabName = "welcome1", "Welcome", icon = icon("magnet")),
                 menuItem(tabName = "worldmap", "World/USA Map", icon = icon("map")),
                 menuItem(tabName = "Unemp_GDP_5_year", "Country Metrics", icon = icon("dashboard")),
                 menuItem(tabName = "univ_comp", "University Compare (Trend & YoY)", icon = icon("dashboard")),
                 menuItem(tabName = "univ_comp1", "Expand Menu", icon = icon("arrow-down"),
                          selectizeInput("Univsel1", "Choose First University", U_univ, selected = "University of Texas at San Antonio", multiple = FALSE),
                          selectizeInput("Country1", "Country", U_country, selected = "United States of America", multiple = FALSE),
                          selectizeInput("Univsel2", "Choose Second University", U_univ, selected = "University of Texas at Dallas", multiple = FALSE),
                          selectizeInput("Country2", "Country", U_country, selected = "United States of America", multiple = FALSE),
                          selectizeInput("metric1", "Choose University Metric", 
                                         c("Teaching_Rating" ,"Inter_Outlook_Rating" ,	 "Research_Rating" ,
                                           "Citations_Rating" ,	 "Industry_Income_Rating",
                                           "Student_Staff_Ratio" , 
                                           "ADM_RATE", "Inter_Students_Per" ,
                                           "Female_Students_Per" , "Male_Students_Per"), 
                                         selected = "Teaching_Rating"),
                          selectInput("metric2", "Choose Country Metric", 
                                      c("GDP_Growth_Rate", "Umemployment_Rate" , "Education_Expenditure_GDP" , "Education_Exp_Tertiary", 
                                        "Import_Growth_Rate", "Export_Growth_Rate", "Industry_Growth_Rate", "GNI_Growth_Rate" ), 
                                      selected = "Education_Expenditure_GDP"),
                          
                          checkboxInput("colorcheck1", "University Metric color change",value = FALSE ),
                          conditionalPanel(
                            condition = "input.colorcheck1==true", 
                            radioButtons("rad1", "select a color",
                                         choices = c("Red", "Green", "aquamarine"), selected = "Green")
                          ),
                          checkboxInput("colorcheck2", "Country Metric color change",value = FALSE ),
                          conditionalPanel(
                            condition = "input.colorcheck2==true", 
                            radioButtons("rad2", "select a color",
                                         choices = c("Red", "Green", "aquamarine"), selected = "Red")
                          )),
                 menuItem(tabName = "ROI_Plot", "USA ROI Scatter Plot", icon = icon("dashboard")),
                 menuItem(tabName = "Top_25_TX", "Top 25 USA ROI Scatter", icon = icon("dashboard")),
                 menuItem(tabName = "Grad_Rates_USA", "Graduation Rates Vs Score", icon = icon("dashboard")),
                 menuItem(tabName = "ExPend_Grad", "College Expenses Vs Performance", icon = icon("dashboard")),
                 menuItem(tabName = "refer", "References", icon = icon("list")), 
                 selected = "univ_comp", width = "300px")
    , width = "300px")
  ,
  dashboardBody(
    tabItems(
      tabItem( tabName = "welcome1",
               fluidRow(Column=1,style = "background-color:transparent;", div(style = "height:0px;"),
                        box(verbatimTextOutput("welcome"),  align = "center", height = "0px" ,width = "0px",
                     tags$style(type="text/css", "#welcome { background-color: transparent;border-color: transparent;
                                height: 1px; width: 600px; text-align:middle;font-size: 40px; height: 20%;}"))),
               fluidRow(Column=8, style = "background-color:transparent;", div(style = "height:0px;"),
                        box(verbatimTextOutput("team"),  align = "center", height = "0px" ,width = "0px",
                     tags$style(type="text/css", "#team { background-color: transparent;border-color: transparent;
                                height: 50px; width: 200px; text-align:middle;font-size: 20px; height: 50%;}"))
                 )
               ),
      tabItem(tabName = "worldmap",
              tabBox(tabPanel( "World Map (Ranking)",box(plotlyOutput("worldmapout", height = "700px"), width = "800px", height = "700px")),
                     tabPanel( "World Map (Top 5 Universities Per Country)",box(plotlyOutput("worldTop5", height = "700px"), width = "800px", height = "700px")),
                     tabPanel( "USA Map (Ranking)",box(plotlyOutput("usamapout", height = "700px"), width = "800px", height = "700px")),
                     tabPanel( "Educational Spend (%GDP)",box(plotlyOutput("worldEDUSPEND", height = "700px"), width = "800px", height = "700px")),
                     width = "1200px", height = "800px")),
      tabItem(tabName = "Unemp_GDP_5_year",
              tabBox(tabPanel( "Unemployment vs Score",box(plotlyOutput("UnempTOut", height = "700px"), width = "800px", height = "700px")),
                     tabPanel( "GDP vs Score",box(plotlyOutput("GDPTOut", height = "700px"), width = "800px", height = "700px")),
                     tabPanel( "Tertiary Education Spend vs Score",box(plotlyOutput("EDUTTOut", height = "700px"), width = "800px", height = "700px")),
                     width = "1200px", height = "800px")),
      tabItem(tabName = "univ_comp1",box(textOutput("welcome1"), align = "center", height = "700px" ,width = "800px")),
      tabItem(tabName = "univ_comp",class='active',
              (fluidRow(
                tabBox(
                  title = textOutput("myPlot1text"),
                  id = "tabset1", height = "410px",
                  tabPanel("Trend", plotOutput("myplot1", height = "360px"), icon = icon("signal")),
                  #tabPanel("Pie", plotOutput("myPlot1Pie")),
                  tabPanel("Info", valueBoxOutput("myPlot1Tab1"), 
                           valueBoxOutput("myPlot1Tab2"), 
                           valueBoxOutput("myPlot1Tab3"), 
                           valueBoxOutput("myPlot1Tab4"), 
                           valueBoxOutput("myPlot1Tab5"), 
                           valueBoxOutput("myPlot1Tab6"), 
                           valueBoxOutput("myPlot1Tab7"),
                           valueBoxOutput("myPlot1Tab8"), 
                           valueBoxOutput("myPlot1Tab9"),icon = icon("table"))
                ),
                tabBox(
                  title = textOutput("myPlot2text"),
                  id = "Info", height = "410px",
                  tabPanel("Trend", plotOutput("myplot2" , height = "360px"), icon = icon("signal")),
                  #tabPanel("Pie", plotOutput("myPlot2Pie")),
                  tabPanel("Info", valueBoxOutput("myPlot2Tab1"), 
                           valueBoxOutput("myPlot2Tab2"), 
                           valueBoxOutput("myPlot2Tab3"), 
                           valueBoxOutput("myPlot2Tab4"), 
                           valueBoxOutput("myPlot2Tab5"), 
                           valueBoxOutput("myPlot2Tab6"), 
                           valueBoxOutput("myPlot2Tab7"),
                           valueBoxOutput("myPlot2Tab8"), 
                           valueBoxOutput("myPlot2Tab9"),icon = icon("table"))
                )
              )),
              fluidRow(
                tabBox(
                  title = textOutput("myPlot3text"),
                  id = "tabset3", height = "410px",
                  tabPanel("Trend", plotOutput("myplot3" , height = "360px"), icon = icon("signal")),
                  #tabPanel("Pie", plotOutput("myPlot3Pie")),
                  tabPanel("Info", valueBoxOutput("myPlot3Tab1"), 
                           valueBoxOutput("myPlot3Tab2"), 
                           valueBoxOutput("myPlot3Tab3"), 
                           valueBoxOutput("myPlot3Tab4"), 
                           valueBoxOutput("myPlot3Tab5"), 
                           valueBoxOutput("myPlot3Tab6"), 
                           valueBoxOutput("myPlot3Tab7"), 
                           valueBoxOutput("myPlot3Tab8"), 
                           valueBoxOutput("myPlot3Tab9"),icon = icon("table"))
                ),
                tabBox(
                  title = textOutput("myPlot4text"),
                  id = "tabset4", height = "410px",
                  tabPanel("Trend", plotOutput("myplot4",  height = "360px" ), icon = icon("signal")),
                  #tabPanel("Pie", plotOutput("myPlot4Pie")),
                  tabPanel("Info", valueBoxOutput("myPlot4Tab1"), 
                           valueBoxOutput("myPlot4Tab2"), 
                           valueBoxOutput("myPlot4Tab3"), 
                           valueBoxOutput("myPlot4Tab4"), 
                           valueBoxOutput("myPlot4Tab5"), 
                           valueBoxOutput("myPlot4Tab6"), 
                           valueBoxOutput("myPlot4Tab7"), 
                           valueBoxOutput("myPlot4Tab8"), 
                           valueBoxOutput("myPlot4Tab9"), icon = icon("table"))
                )
              )
      ),
      tabItem(tabName = "ROI_Plot",
              tabBox(tabPanel( "USA In State ROI",box(plotlyOutput("InROI", height = "700px"), width = "800px", height = "700px")),
                     tabPanel( "USA Out State ROI",box(plotlyOutput("OutROI", height = "700px"), width = "800px", height = "700px")),
                     tabPanel( "USA Map (In Sate ROI)",box(plotlyOutput("usamaproiin", height = "700px"), width = "800px", height = "700px")),
                     tabPanel( "USA Map (Out State ROI)",box(plotlyOutput("usamaproiout", height = "700px"), width = "800px", height = "700px")),
                     width = "1200px", height = "800px")),
      tabItem(tabName = "Top_25_TX",
              tabBox(tabPanel( "Top 25 In State ROI",box(plotlyOutput("InROI25", height = "700px"), width = "800px", height = "700px")),
                     tabPanel( "Top 25 Out State ROI",box(plotlyOutput("OutROI25", height = "700px"), width = "800px", height = "700px")),
                     tabPanel( "Texas & Top 25 In State ROI",box(plotlyOutput("InROITX", height = "700px"), width = "800px", height = "700px")),
                     tabPanel( "Texas & Top 25 Out State ROI",box(plotlyOutput("OutROITX", height = "700px"), width = "800px", height = "700px")),
                     width = "1200px", height = "800px")),
      tabItem(tabName = "Grad_Rates_USA",
              tabBox(tabPanel( "4/5/6 Year Grad Rate vs Score",box(plotlyOutput("Grad4Out", height = "700px"), width = "800px", height = "700px")),
                     tabPanel( "Total Graduation Rate vs Avg Amount of Aid",box(plotlyOutput("GradTVsAis", height = "700px"), width = "800px", height = "700px")),
                     tabPanel( "Total Graduation Rate vs Weighted Tution Average",box(plotlyOutput("GradTVTuition", height = "700px"), width = "800px", height = "700px")),
                     tabPanel( "Admission Rate Vs Graduation Rate",box(plotlyOutput("Admission_Graduation_Rate", height = "700px"), width = "800px", height = "700px")),
                     tabPanel( "Male/Female Admission Rate Vs Graduation Rate",box(plotlyOutput("Male_Female_Graduation_Rate", height = "700px"), width = "800px", height = "700px")),
                     width = "1200px", height = "800px")),
      tabItem(tabName = "ExPend_Grad",
              tabBox(tabPanel( "Institutional Expenses vs Graduation Rates",box(plotlyOutput("Inst_exp_Grad_Rate", height = "700px"), width = "800px", height = "700px")),
                     tabPanel( "Research Expenses vs Graduation Rates",box(plotlyOutput("Res_exp_Grad_Rate", height = "700px"), width = "800px", height = "700px")),
                     tabPanel( "Academic Expenses vs Graduation Rates",box(plotlyOutput("Academic_exp_Grad_Rate", height = "700px"), width = "800px", height = "700px")),
                     tabPanel( "TX - UTSA Expenses vs Graduation Rates",box(plotlyOutput("UTSA_exp_Grad_Rate", height = "700px"), width = "800px", height = "700px")),
                     width = "1200px", height = "800px")),
      tabItem(tabName = "refer",box(dataTableOutput("references"), width = "700px"))
    )
  )
)

server <- function(input, output, session) {
  
  options(shiny.sanitize.errors = TRUE)
  
  observeEvent(input$sbmenu,{
    print(input$sbmenu)
    #print("manu")
  })
  output$welcome <- renderText(paste(" "," ","World Class Education", "UTSA MSDA FALL 2017",sep = "\n"))
  
  output$team <- renderText(paste(" "," "," "," "," "," "," "," ","Team Members", 
                                     "Manoranjan Kumar", "Gary Sepulveda",
                                     "Hejin Shin", "Keira Phifer"," "," ",sep = "\n"))
  
  output$welcome2 <- renderText(print("Hi All, This is Manu, shiny and html is just wow!"))
  
  output$references <- renderDataTable(
    datatable(ylinks, escape = FALSE)
  )
  
  
  Univ1_Full <- reactive(Times_Full %>% filter(University_Name  == input$Univsel1) %>% arrange(Year, World_Rank) )
  U_country1 = reactive(sort(unique(Univ1_Full()$Country)))
  observe({
    updateSelectizeInput(session, "Country1",
                         #label = paste("Choose First Country: ", length(x)),
                         choices = U_country,
                         selected = (head(U_country1(), 1))
    )
  })
  
  Univ1_Country_Full <- reactive(Times_Full %>% filter(Country == input$Country1)  %>% arrange(World_Rank, Year))
  U_UNIV1 = reactive(unique(Univ1_Country_Full()$University_Name))
  observe({
    if (input$Country1 != "United States of America" ) 
      updateSelectizeInput(session, "Univsel1",
                           #label = paste("Choose First Country: ", length(x)),
                           choices = U_UNIV1(),
                           selected = (head(U_UNIV1(), 1))
      )
  })
  
  observe({
    if (input$Country1 == "United States of America" )
      updateSelectizeInput(session, "Univsel1",
                           #label = paste("Choose First Country: ", length(x)),
                           choices = U_univ,
                           selected = "University of Texas at San Antonio"
      )
  })
  
  Univ2_Full <- reactive(Times_Full %>% filter(University_Name  == input$Univsel2) %>% arrange(World_Rank, Year))
  U_country2 = reactive(sort(unique(Univ2_Full()$Country)))
  observe({
    #if (input$Univsel2 != "University of Texas at Dallas") 
    #if (input$Country1 != "United States of America" )
    updateSelectizeInput(session, "Country2",
                         #label = paste("Choose First Country: ", length(x)),
                         choices = U_country,
                         selected = head(U_country2(), 1)
                         
    )
  })
  
  Univ2_Country_Full <- reactive(Times_Full %>% filter(Country == input$Country2) %>% arrange(World_Rank, Year))
  U_UNIV2 = reactive(unique(Univ2_Country_Full()$University_Name))
  observe({
    if (input$Country2 != "United States of America" )
      updateSelectizeInput(session, "Univsel2",
                           #label = paste("Choose First Country: ", length(x)),
                           choices = U_UNIV2(),
                           selected = head(U_UNIV2(), 1)
      )
  })
  
  observe({
    if (input$Country2 == "United States of America" )
      #if (input$Univsel1 != "University of Texas at Dallas")
      updateSelectizeInput(session, "Univsel2",
                           #label = paste("Choose First Country: ", length(x)),
                           choices = U_univ,
                           selected = "University of Texas at Dallas"
      )
  })
  
  
  
  Country1_GDP <- reactive((GDP_All %>% filter(Country  == input$Country1)))
  Country2_GDP <- reactive((GDP_All %>% filter(Country  == input$Country2)))
  
  
  #output$myPlot1text <- renderText({paste0(input$Univsel1 ,"\n",input$metric1 , " (", input$rad1, ") and ", "Total_Score_Calculated (Blue)")})
  output$myPlot1text <- renderText({input$Univsel1})
  output$myplot1 <- renderPlot({
    p <- 
      ggplot() +
      theme_bw()  +
      ggtitle("",paste0(input$metric1 , " (", input$rad1, ") and ", "Total_Score_Calculated (Blue)")) +
      geom_smooth(data = Univ1_Full(), aes_string(x="Year", y = input$metric1),  position = "identity",  color  = input$rad1) +
      geom_dl(data = Univ1_Full(), aes_string(x="Year", y = paste('as.numeric(',input$metric1,')') , label=paste('round(as.numeric(',input$metric1,'),digits = 2)')),method=list("last.points",rot=30)) +
      geom_smooth(data = Univ1_Full(), aes_string(x="Year", y = "Total_Score_Calculated"), se = TRUE, color  = "blue") +
      geom_dl(data = Univ1_Full(), aes_string(x="Year", y = "as.numeric(Total_Score_Calculated)", label="round(as.numeric(Total_Score_Calculated), digits = 2)"),method=list("last.points",rot=30)) +
      #geom_text(data = Univ1_Full(), aes_string(x="Year", y = "Total_Score_Calculated", label = "Total_Score_Calculated"), vjust = "outward", hjust = "outward", color  = "black") +
      scale_y_log10() +
      #ylim(15, 100) +
      xlim(2012,2016) +
      labs (x= "Year", y = NULL ) +
      theme(panel.border = element_rect(size = 1),
            plot.background = element_rect(fill = 'lightblue', colour = 'black'),
            axis.title.x = element_text(hjust=0.98),
            axis.title.y = element_text(hjust=1),
            axis.text = element_text(size=12),
            axis.ticks = element_blank()
      )
    p
  })
  
  #tdmaxyear <- reactive((head(unique(subset(Univ1_Full() %>% arrange(desc(Year)), select=c("Year"))),1)))
  #print(tdmaxyear())
  
  output$myPlot1Tab1 <- renderValueBox({ 
    td <- unique(subset(Univ1_Full() %>% arrange(desc(Year)), select=c("World_Rank"))) %>% filter(World_Rank != '..')
    t <- head(td, 1)- head(td, 2)[2,]
    p <- 
      valueBox(
        value = tags$div("World",tags$br(),"Rank", style = "font-size:50%"),
        paste(head(td, 1), "vs (Prev:",
              head(td, 2)[2,], ")",sep = "\n"),
        # print(Univ1_Full)
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t < 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t < 0) { "green" } else { "red" }))
        
      )
    p    
  })
  
  output$myPlot1Tab2 <- renderValueBox({ 
    td <- unique(subset(Univ1_Full() %>% arrange(desc(Year)), select=c("Teaching_Rating"))) %>% filter(Teaching_Rating != '..')
    t <- head(td, 1)- head(td, 2)[2,]
    tperc <- round((t/head(td, 1))*100,digits=1)
    p <- 
      valueBox(
        value = tags$div("Teaching",tags$br(),"Rating", style = "font-size:50%"),
        paste(head(td, 1), "(", tperc, "%)",sep = "\n"),
        # print(Univ1_Full)
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
      )
    p    
  })
  
  output$myPlot1Tab3 <- renderValueBox({ 
    td <- unique(subset(Univ1_Full() %>% arrange(desc(Year)), select=c("Inter_Outlook_Rating"))) %>% filter(Inter_Outlook_Rating != '..')
    t <- head(td, 1)- head(td, 2)[2,]
    tperc <- round((t/head(td, 1))*100,digits=1)
    p <- 
      valueBox(
        value = tags$div("International",tags$br(),"Outlook", style = "font-size:50%"),
        paste(head(td, 1), "(", tperc, "%)",sep = "\n"),
        # print(Univ1_Full)
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
      )
    p    
  })
  
  output$myPlot1Tab4 <- renderValueBox({ 
    td <- unique(subset(Univ1_Full() %>% arrange(desc(Year)), select=c("Research_Rating"))) %>% filter(Research_Rating != '..')
    t <- head(td, 1)- head(td, 2)[2,]
    tperc <- round((t/head(td, 1))*100,digits=1)
    p <- 
      valueBox(
        value = tags$div("Research",tags$br(),"Rating", style = "font-size:50%"),
        paste(head(td, 1), "(", tperc, "%)",sep = "\n"),
        # print(Univ1_Full)
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
      )
    p    
  })
  
  
  output$myPlot1Tab5 <- renderValueBox({ 
    td <- unique(subset(Univ1_Full() %>% arrange(desc(Year)), select=c("Citations_Rating"))) %>% filter(Citations_Rating != '..')
    t <- head(td, 1)- head(td, 2)[2,]
    tperc <- round((t/head(td, 1))*100,digits=1)
    p <- 
      valueBox(
        value = tags$div("Citations",tags$br(),"Rating", style = "font-size:50%"),
        paste(head(td, 1), "(", tperc, "%)",sep = "\n"),
        # print(Univ1_Full)
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
      )
    p    
  })
  
  output$myPlot1Tab6 <- renderValueBox({ 
    td <- unique(subset(Univ1_Full() %>% arrange(desc(Year)), select=c("Industry_Income_Rating"))) %>% filter(Industry_Income_Rating != '..')
    t <- head(td, 1)- head(td, 2)[2,]
    tperc <- round((t/head(td, 1))*100,digits=1)
    p <- 
      valueBox(
        value = tags$div("Industry",tags$br(),"Income", style = "font-size:50%"),
        paste(head(td, 1), "(", tperc, "%)",sep = "\n"),
        # print(Univ1_Full)
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
      )
    p    
  })
  
  output$myPlot1Tab7 <- renderValueBox({ 
    td <- unique(subset(Univ1_Full() %>% arrange(desc(Year)), select=c("Total_Score_Calculated"))) %>% filter(Total_Score_Calculated != '..')
    t <- head(td, 1)- head(td, 2)[2,]
    tperc <- round((t/head(td, 1))*100,digits=1)
    p <- 
      valueBox(
        value = tags$div("Total",tags$br(),"Score", style = "font-size:50%"),
        paste(head(td, 1), "(", tperc, "%)",sep = "\n"),
        # print(Univ1_Full)
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
      )
    p    
  })
  
  output$myPlot1Tab8 <- renderValueBox({
    p <- 
      valueBox(
        value = tags$div("InState",tags$br(),"ROI", style = "font-size:50%"),
        round(unique(subset(Univ1_Full() %>% filter(Year ==2016 ), select=c("ROI_InState"))),digits =2),
        # print(Univ2_Full)
        icon = icon("magnet"),
        color = ("blue")
      )
    p    
  })
  
  output$myPlot1Tab9 <- renderValueBox({
    p <- 
      valueBox(
        value = tags$div("OutState",tags$br(),"ROI", style = "font-size:50%"),
        round(unique(subset(Univ1_Full() %>% filter(Year ==2016 ), select=c("ROI_OutState"))),digits =2),
        icon = icon("magnet"),
        color = ("blue")
      )
    p    
  })
  
  output$myPlot2text <- renderText({input$Univsel2})
  output$myplot2 <- renderPlot({
    p <- 
      ggplot() +
      theme_bw()  +
      ggtitle("",paste0(input$metric1 , " (", input$rad1, ") and ", "Total_Score_Calculated (Blue)")) +
      geom_smooth(data = Univ2_Full(), aes_string(x="Year", y = paste('as.numeric(',input$metric1,')')), position = "identity",  color  = input$rad1) +
      geom_dl(data = Univ2_Full(), aes_string(x="Year", y = paste('as.numeric(',input$metric1,')') , label=paste('round(as.numeric(',input$metric1,'),digits = 2)')),method=list("last.points",rot=30)) +
      geom_smooth(data = Univ2_Full(), aes_string(x="Year", y = "Total_Score_Calculated"), se = TRUE, color  = "blue") +
      geom_dl(data = Univ2_Full(), aes_string(x="Year", y = "as.numeric(Total_Score_Calculated)", label="round(as.numeric(Total_Score_Calculated), digits = 2)"),method=list("last.points",rot=30)) +
      #geom_text(data = Univ2_Full(), aes_string(x="Year", y = "Total_Score_Calculated", label = "Total_Score_Calculated"), vjust = "outward", hjust = "outward", color  = "black") +
      scale_y_log10() +
      #ylim(15, 100) +
      xlim(2012,2016) +
      labs (x= "Year", y = NULL ) +
      theme(
        #panel.border = element_rect(size = 1),
        plot.background = element_rect(fill = 'lightblue', colour = 'black'),
        axis.title.x = element_text(hjust=0.98),
        axis.title.y = element_text(hjust=1),
        axis.text = element_text(size=12),
        axis.ticks = element_blank()
      )
    p
  })
  
  output$myPlot2Tab1 <- renderValueBox({ 
    td <- unique(subset(Univ2_Full() %>% arrange(desc(Year)), select=c("World_Rank"))) %>% filter(World_Rank != '..')
    t <- head(td, 1)- head(td, 2)[2,]
    p <- 
      valueBox(
        value = tags$div("World",tags$br(),"Rank", style = "font-size:50%"),
        paste(head(td, 1), "vs (Prev: ",
              head(td, 2)[2,], ")", sep = "\n"),
        # print(Univ2_Full)
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t < 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t < 0) { "green" } else { "red" }))
        
      )
    p    
  })
  
  output$myPlot2Tab2 <- renderValueBox({ 
    td <- unique(subset(Univ2_Full() %>% arrange(desc(Year)), select=c("Teaching_Rating"))) %>% filter(Teaching_Rating != '..')
    t <- head(td, 1)- head(td, 2)[2,]
    tperc <- round((t/head(td, 1))*100,digits=1)
    p <- 
      valueBox(
        value = tags$div("Teaching",tags$br(),"Rating", style = "font-size:50%"),
        paste(head(td, 1), "(", tperc, "%)",sep = "\n"),
        # print(Univ2_Full)
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
      )
    p    
  })
  
  output$myPlot2Tab3 <- renderValueBox({ 
    td <- unique(subset(Univ2_Full() %>% arrange(desc(Year)), select=c("Inter_Outlook_Rating"))) %>% filter(Inter_Outlook_Rating != '..')
    t <- head(td, 1)- head(td, 2)[2,]
    tperc <- round((t/head(td, 1))*100,digits=1)
    p <- 
      valueBox(
        value = tags$div("International",tags$br(),"Outlook", style = "font-size:50%"),
        paste(head(td, 1), "(", tperc, "%)",sep = "\n"),
        # print(Univ2_Full)
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
      )
    p    
  })
  
  output$myPlot2Tab4 <- renderValueBox({ 
    td <- unique(subset(Univ2_Full() %>% arrange(desc(Year)), select=c("Research_Rating"))) %>% filter(Research_Rating != '..')
    t <- head(td, 1)- head(td, 2)[2,]
    tperc <- round((t/head(td, 1))*100,digits=1)
    p <- 
      valueBox(
        value = tags$div("Research",tags$br(),"Rating", style = "font-size:50%"),
        paste(head(td, 1), "(", tperc, "%)",sep = "\n"),
        # print(Univ2_Full)
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
      )
    p    
  })
  
  
  output$myPlot2Tab5 <- renderValueBox({ 
    td <- unique(subset(Univ2_Full() %>% arrange(desc(Year)), select=c("Citations_Rating"))) %>% filter(Citations_Rating != '..')
    t <- head(td, 1)- head(td, 2)[2,]
    tperc <- round((t/head(td, 1))*100,digits=1)
    p <- 
      valueBox(
        value = tags$div("Citations",tags$br(),"Rating", style = "font-size:50%"),
        paste(head(td, 1), "(", tperc, "%)",sep = "\n"),
        # print(Univ2_Full)
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
      )
    p    
  })
  
  output$myPlot2Tab6 <- renderValueBox({ 
    td <- unique(subset(Univ2_Full() %>% arrange(desc(Year)), select=c("Industry_Income_Rating"))) %>% filter(Industry_Income_Rating != '..')
    t <- head(td, 1)- head(td, 2)[2,]
    tperc <- round((t/head(td, 1))*100,digits=1)
    p <- 
      valueBox(
        value = tags$div("Industry",tags$br(),"Income", style = "font-size:50%"),
        paste(head(td, 1), "(", tperc, "%)",sep = "\n"),
        # print(Univ2_Full)
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
      )
    p    
  })
  
  output$myPlot2Tab7 <- renderValueBox({ 
    td <- unique(subset(Univ2_Full() %>% arrange(desc(Year)), select=c("Total_Score_Calculated"))) %>% filter(Total_Score_Calculated != '..')
    t <- head(td, 1)- head(td, 2)[2,]
    tperc <- round((t/head(td, 1))*100,digits=1)
    p <- 
      valueBox(
        value = tags$div("Total",tags$br(),"Score", style = "font-size:50%"),
        paste(head(td, 1), "(", tperc, "%)",sep = "\n"),
        # print(Univ2_Full)
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
      )
    p    
  })
  
  output$myPlot2Tab8 <- renderValueBox({
    p <- 
      valueBox(
        value = tags$div("InState",tags$br(),"ROI", style = "font-size:50%"),
        round(unique(subset(Univ2_Full() %>% filter(Year ==2016 ), select=c("ROI_InState"))),digits =2),
        # print(Univ2_Full)
        icon = icon("magnet"),
        color = ("blue")
      )
    p    
  })
  
  output$myPlot2Tab9 <- renderValueBox({
    p <- 
      valueBox(
        value = tags$div("OutState",tags$br(),"ROI", style = "font-size:50%"),
        round(unique(subset(Univ2_Full() %>% filter(Year ==2016 ), select=c("ROI_OutState"))),digits =2),
        icon = icon("magnet"),
        color = ("blue")
      )
    p    
  })
  
  # Output the data
  output$myPlot3text <- renderText({input$Country1})
  output$myplot3 <- renderPlot({
    ggplot() +
      theme_bw()  +
      #geom_rect(colour = "grey30") +
      ggtitle("",paste0(input$metric2 , " (", input$rad2, ") and ", "GDP_Growth_rate (Blue)")) +
      geom_smooth(data = Country1_GDP(), aes_string(x="as.numeric(Year)", y ="as.numeric(GDP_Growth_Rate)", group = "Country"), position = position_dodge(1)) +
      geom_dl(data = Country1_GDP(), aes_string(x="as.numeric(Year)", y ="as.numeric(GDP_Growth_Rate)", label="round(as.numeric(GDP_Growth_Rate), digits = 2)"),method=list("last.points",rot=30)) +
      geom_smooth(data = Country1_GDP(), aes_string(x="as.numeric(Year)",y = paste('as.numeric(',input$metric2,')')), color = input$rad2 , fullrange=TRUE) +
      geom_dl(data = Country1_GDP(), aes_string(x="as.numeric(Year)", y = paste('as.numeric(',input$metric2,')') , label=paste('round(as.numeric(',input$metric2,'),digits = 2)')),method=list("last.points",rot=30)) +
      scale_y_log10()+
      ylim(0, 20) +
      xlim(2012, 2016) +
      labs (x= "Year", y = NULL ) +
      theme(
        panel.border = element_rect(size = 1),
        plot.background = element_rect(fill = 'lightblue', colour = 'black'),
        axis.title.x = element_text(hjust=0.98),
        axis.title.y = element_text(hjust=1),
        axis.text = element_text(size=12),
        axis.ticks = element_blank()
      )
  })
  
  output$myPlot3Tab1 <- renderValueBox({
    td <- unique(subset(Country1_GDP() %>% arrange(desc(Year)), select=c("GDP_Growth_Rate"))) %>% filter(GDP_Growth_Rate != '..')
    t <- as.numeric(head(td, 1)[1,])- as.numeric(head(td, 2)[2,])
    #tperc <- round((t/as.numeric(head(td, 1)[1,])*100),digits=1)
    p <- 
      valueBox(
        value = tags$div("GDP",tags$br(),"Growth", style = "font-size:50%"),
        paste(round(as.numeric(head(td, 1)[1,]),digits = 2), "vs ", round(as.numeric(head(td, 2)[2,]), digits = 2), sep = "\n"),
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
        
      )
    p    
  })
  
  output$myPlot3Tab2 <- renderValueBox({
    td <- unique(subset(Country1_GDP() %>% arrange(desc(Year)), select=c("Education_Expenditure_GDP"))) %>% filter(Education_Expenditure_GDP != '..')
    t <- as.numeric(head(td, 1)[1,])- as.numeric(head(td, 2)[2,])
    tperc <- round((t/as.numeric(head(td, 1)[1,])*100),digits=1)
    p <- 
      valueBox(
        value = tags$div("Education",tags$br(),"Spend(GDP)", style = "font-size:50%"),
        #round(as.numeric(unique(subset(Country1_GDP() %>% filter(Year ==2014 ), select=c("Education_Expenditure_GDP")))), digits =2),
        paste((round(as.numeric(head(td, 1)[1,]),digits = 2)), "(", tperc, "%)",sep = "\n"),
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
        
      )
    p    
  })
  
  output$myPlot3Tab3 <- renderValueBox({
    td <- unique(subset(Country1_GDP() %>% arrange(desc(Year)), select=c("Umemployment_Rate"))) %>% filter(Umemployment_Rate != '..')
    t <- as.numeric(head(td, 1)[1,])- as.numeric(head(td, 2)[2,])
    p <- 
      valueBox(
        value = tags$div("Unemployment",tags$br(),"Rate", style = "font-size:50%"),
        paste((round(as.numeric(head(td, 1)[1,]),digits = 2)), "vs", round(as.numeric(head(td, 2)[2,]),digits = 2),sep = "\n"),
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
        
      )
    p    
  })
  
  output$myPlot3Tab4 <- renderValueBox({
    td <- unique(subset(Country1_GDP() %>% arrange(desc(Year)), select=c("Education_Exp_Tertiary"))) %>% filter(Education_Exp_Tertiary != '..')
    t <- as.numeric(head(td, 1)[1,])- as.numeric(head(td, 2)[2,])
    tperc <- round((t/as.numeric(head(td, 1)[1,])*100),digits=1)
    p <- 
      valueBox(
        value = tags$div("Education",tags$br(),"Spend(Tertiary)", style = "font-size:50%"),
        paste((round(as.numeric(head(td, 1)[1,]),digits = 2)), "(", tperc, "%)",sep = "\n"),
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
        
      )
    p    
  })
  
  output$myPlot3Tab5 <- renderValueBox({
    td <- unique(subset(Country1_GDP() %>% arrange(desc(Year)), select=c("Import_Growth_Rate"))) %>% filter(Import_Growth_Rate != '..')
    t <- as.numeric(head(td, 1)[1,])- as.numeric(head(td, 2)[2,])
    p <- 
      valueBox(
        value = tags$div("Import",tags$br(),"Growth Rate", style = "font-size:50%"),
        paste((round(as.numeric(head(td, 1)[1,]),digits = 2)), "vs", round(as.numeric(head(td, 2)[2,]),digits = 2),sep = "\n"),
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
        
      )
    p    
  })
  
  output$myPlot3Tab6 <- renderValueBox({
    td <- unique(subset(Country1_GDP() %>% arrange(desc(Year)), select=c("Export_Growth_Rate"))) %>% filter(Export_Growth_Rate != '..')
    t <- as.numeric(head(td, 1)[1,])- as.numeric(head(td, 2)[2,])
    p <- 
      valueBox(
        value = tags$div("Export",tags$br(),"Growth Rate", style = "font-size:50%"),
        paste((round(as.numeric(head(td, 1)[1,]),digits = 2)), "vs", round(as.numeric(head(td, 2)[2,]),digits = 2),sep = "\n"),
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
        
      )
    p    
  })
  
  output$myPlot3Tab7 <- renderValueBox({
    td <- unique(subset(Country1_GDP() %>% arrange(desc(Year)), select=c("Industry_Growth_Rate"))) %>% filter(Industry_Growth_Rate != '..')
    t <- as.numeric(head(td, 1)[1,])- as.numeric(head(td, 2)[2,])
    p <- 
      valueBox(
        value = tags$div("Industry",tags$br(),"Growth Rate", style = "font-size:50%"),
        paste((round(as.numeric(head(td, 1)[1,]),digits = 2)), "vs", round(as.numeric(head(td, 2)[2,]),digits = 2),sep = "\n"),
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
        
      )
    p    
  })
  
  output$myPlot3Tab8 <- renderValueBox({
    td <- unique(subset(Country1_GDP() %>% arrange(desc(Year)), select=c("GNI_Growth_Rate"))) %>% filter(GNI_Growth_Rate != '..')
    t <- as.numeric(head(td, 1)[1,])- as.numeric(head(td, 2)[2,])
    p <- 
      valueBox(
        value = tags$div("GNI",tags$br(),"Growth Rate", style = "font-size:50%"),
        paste((round(as.numeric(head(td, 1)[1,]),digits = 2)), "vs", round(as.numeric(head(td, 2)[2,]),digits = 2),sep = "\n"),
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
        
      )
    p    
  })
  
  # Output the data
  output$myPlot4text <- renderText({input$Country2})
  output$myplot4 <- renderPlot({
    ggplot() +
      theme_bw()  +
      #geom_rect(colour = "grey30") +
      ggtitle("",paste0(input$metric2 , " (", input$rad2, ") and ", "GDP_Growth_rate (Blue)")) +
      geom_smooth(data = Country2_GDP(), aes_string(x="as.numeric(Year)", y ="as.numeric(GDP_Growth_Rate)", group = "Country"), position = "identity") +
      geom_dl(data = Country2_GDP(), aes_string(x="as.numeric(Year)", y ="as.numeric(GDP_Growth_Rate)", label="round(as.numeric(GDP_Growth_Rate), digits = 2)"),method=list("last.points",rot=30)) +
      geom_smooth(data = Country2_GDP(), aes_string(x="as.numeric(Year)",y = paste('as.numeric(',input$metric2,')')), color = input$rad2 , fullrange=TRUE) +
      geom_dl(data = Country2_GDP(), aes_string(x="as.numeric(Year)", y = paste('as.numeric(',input$metric2,')') , label=paste('round(as.numeric(',input$metric2,'),digits = 2)')),method=list("last.points",rot=30)) +
      scale_y_log10()+
      ylim(0, 20) +
      xlim(2012, 2016) +
      labs (x= "Year", y = NULL ) +
      theme(
        panel.border = element_rect(size = 1),
        plot.background = element_rect(fill = 'lightblue', colour = 'black'),
        axis.title.x = element_text(hjust=0.98),
        axis.title.y = element_text(hjust=1),
        axis.text = element_text(size=12),
        axis.ticks = element_blank()
      )
  })
  
  
  output$myPlot4Tab1 <- renderValueBox({
    td <- unique(subset(Country2_GDP() %>% arrange(desc(Year)), select=c("GDP_Growth_Rate"))) %>% filter(GDP_Growth_Rate != '..')
    t <- as.numeric(head(td, 1)[1,])- as.numeric(head(td, 2)[2,])
    #tperc <- round((t/as.numeric(head(td, 1)[1,])*100),digits=1)
    p <- 
      valueBox(
        value = tags$div("GDP",tags$br(),"Growth", style = "font-size:50%"),
        paste(round(as.numeric(head(td, 1)[1,]),digits = 2), "vs ", round(as.numeric(head(td, 2)[2,]), digits = 2), sep = "\n"),
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
        
      )
    p    
  })
  
  output$myPlot4Tab2 <- renderValueBox({
    td <- unique(subset(Country2_GDP() %>% arrange(desc(Year)), select=c("Education_Expenditure_GDP"))) %>% filter(Education_Expenditure_GDP != '..')
    t <- as.numeric(head(td, 1)[1,])- as.numeric(head(td, 2)[2,])
    tperc <- round((t/as.numeric(head(td, 1)[1,])*100),digits=1)
    p <- 
      valueBox(
        value = tags$div("Education",tags$br(),"Spend(GDP)", style = "font-size:50%"),
        #round(as.numeric(unique(subset(Country2_GDP() %>% filter(Year ==2014 ), select=c("Education_Expenditure_GDP")))), digits =2),
        paste((round(as.numeric(head(td, 1)[1,]),digits = 2)), "(", tperc, "%)",sep = "\n"),
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
        
      )
    p    
  })
  
  output$myPlot4Tab3 <- renderValueBox({
    td <- unique(subset(Country2_GDP() %>% arrange(desc(Year)), select=c("Umemployment_Rate"))) %>% filter(Umemployment_Rate != '..')
    t <- as.numeric(head(td, 1)[1,])- as.numeric(head(td, 2)[2,])
    p <- 
      valueBox(
        value = tags$div("Unemployment",tags$br(),"Rate", style = "font-size:50%"),
        paste((round(as.numeric(head(td, 1)[1,]),digits = 2)), "vs", round(as.numeric(head(td, 2)[2,]),digits = 2),sep = "\n"),
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
        
      )
    p    
  })
  
  output$myPlot4Tab4 <- renderValueBox({
    td <- unique(subset(Country2_GDP() %>% arrange(desc(Year)), select=c("Education_Exp_Tertiary"))) %>% filter(Education_Exp_Tertiary != '..')
    t <- as.numeric(head(td, 1)[1,])- as.numeric(head(td, 2)[2,])
    tperc <- round((t/as.numeric(head(td, 1)[1,])*100),digits=1)
    p <- 
      valueBox(
        value = tags$div("Education",tags$br(),"Spend(Tertiary)", style = "font-size:50%"),
        paste((round(as.numeric(head(td, 1)[1,]),digits = 2)), "(", tperc, "%)",sep = "\n"),
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
        
      )
    p    
  })
  
  output$myPlot4Tab5 <- renderValueBox({
    td <- unique(subset(Country2_GDP() %>% arrange(desc(Year)), select=c("Import_Growth_Rate"))) %>% filter(Import_Growth_Rate != '..')
    t <- as.numeric(head(td, 1)[1,])- as.numeric(head(td, 2)[2,])
    p <- 
      valueBox(
        value = tags$div("Import",tags$br(),"Growth Rate", style = "font-size:50%"),
        paste((round(as.numeric(head(td, 1)[1,]),digits = 2)), "vs", round(as.numeric(head(td, 2)[2,]),digits = 2),sep = "\n"),
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
        
      )
    p    
  })
  
  output$myPlot4Tab6 <- renderValueBox({
    td <- unique(subset(Country2_GDP() %>% arrange(desc(Year)), select=c("Export_Growth_Rate"))) %>% filter(Export_Growth_Rate != '..')
    t <- as.numeric(head(td, 1)[1,])- as.numeric(head(td, 2)[2,])
    p <- 
      valueBox(
        value = tags$div("Export",tags$br(),"Growth Rate", style = "font-size:50%"),
        paste((round(as.numeric(head(td, 1)[1,]),digits = 2)), "vs", round(as.numeric(head(td, 2)[2,]),digits = 2),sep = "\n"),
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
        
      )
    p    
  })
  
  output$myPlot4Tab7 <- renderValueBox({
    td <- unique(subset(Country2_GDP() %>% arrange(desc(Year)), select=c("Industry_Growth_Rate"))) %>% filter(Industry_Growth_Rate != '..')
    t <- as.numeric(head(td, 1)[1,])- as.numeric(head(td, 2)[2,])
    p <- 
      valueBox(
        value = tags$div("Industry",tags$br(),"Growth Rate", style = "font-size:50%"),
        paste((round(as.numeric(head(td, 1)[1,]),digits = 2)), "vs", round(as.numeric(head(td, 2)[2,]),digits = 2),sep = "\n"),
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
        
      )
    p    
  })
  
  output$myPlot4Tab8 <- renderValueBox({
    td <- unique(subset(Country2_GDP() %>% arrange(desc(Year)), select=c("GNI_Growth_Rate"))) %>% filter(GNI_Growth_Rate != '..')
    t <- as.numeric(head(td, 1)[1,])- as.numeric(head(td, 2)[2,])
    p <- 
      valueBox(
        value = tags$div("GNI",tags$br(),"Growth Rate", style = "font-size:50%"),
        paste((round(as.numeric(head(td, 1)[1,]),digits = 2)), "vs", round(as.numeric(head(td, 2)[2,]),digits = 2),sep = "\n"),
        icon = (if(is.na(t)) {icon("pushpin")} else (if (t > 0) { icon("arrow-up")} else { icon("arrow-down") })),
        color = (if(is.na(t)) { "blue"} else (if (t > 0) { "green" } else { "red" }))
        
      )
    p    
  }) 
  
  #if (input$univ_comp="")
  #observe({
  #  if(!is.null(reactive(input$univ_comp))){}
  #else{
  # observe({
  #  print(input$univ_comp)
  #}) 
  
  # output$ROI <- renderPlotly({
  #     sp <- ggplot(data=Times_Full_USA_2016, aes(x=Total_Score_Calculated, y=ROI_InState, 
  #                                                color=bins, shape=Species)) + 
  #       geom_point(size=6, alpha=0.6)  +
  #       geom_text(aes(label = University_Name), angle = 45, vjust = "outward", hjust = "outward", 
  #                 nudge_y = 1, check_overlap = 10 ,color  = "black") +
  #     geom_hline(yintercept=12) +
  #     geom_hline(yintercept=12, linetype="dashed", color = "red", size =0.5) +
  #     geom_hline(yintercept=12, linetype="dashed", color = "red", size=1) +
  #     geom_vline(xintercept = median(Times_Full_USA_2016$Total_Score_Calculated)) +
  #     geom_vline(xintercept = median(Times_Full_USA_2016$Total_Score_Calculated), linetype="dashed", color = "blue", size=0.5) +
  #     geom_vline(xintercept = median(Times_Full_USA_2016$Total_Score_Calculated), linetype="dashed", color = "blue", size=1) +
  #       
  #       labs (x= p("Total Score"), y = h3("In-state ROI")  ) +
  #       theme(
  #         #panel.border = element_rect(size = 1),
  #             plot.margin = margin(0, 1, 0, 0, "cm"),
  #             #plot.background = element_rect(fill = 'lightblue', colour = 'black'),
  #             axis.title.x = element_text(hjust=0.98),
  #             axis.title.y = element_text(hjust=1),
  #             axis.text = element_text(colour = "black", size = 12),
  #             axis.ticks = element_blank()
  #       ) 
  #     ggplotly(sp)
  # })
  
  Times_Full_USA_2016 <- Times_Full_USA %>% filter(Year  == "2016")
  
  output$InROI <- renderPlotly({
    # Times_Full_USA_2016_84 <- diamonds[sample(nrow(diamonds), 1000), ]
    
    # Make a basic scatter plot :
    p<-plot_ly(Times_Full_USA_2016, x = ~Total_Score_Calculated, y = ~ROI_InState, type="scatter", 
               text = paste("University : ", Times_Full_USA_2016$University_Name),
               mode = "markers", color = ~Total_Score_Calculated, alpha = 1, 
               size = ~Total_Score_Calculated) 
    #%>%
    # add_markers(color = ~Total_Score_Calculated, size = ~ROI_InState)
    p
  })
  
  output$OutROI <- renderPlotly({
    # Times_Full_USA_2016_84 <- diamonds[sample(nrow(diamonds), 1000), ]
    
    # Make a basic scatter plot :
    p<-plot_ly(Times_Full_USA_2016, x = ~Total_Score_Calculated, y = ~ROI_OutState, type="scatter", 
               text = paste("University : ", Times_Full_USA_2016$University_Name),
               mode = "markers", color = ~Total_Score_Calculated, alpha = 1, 
               size = ~Total_Score_Calculated) 
    #%>%
    # add_markers(color = ~Total_Score_Calculated, size = ~ROI_InState)
    p
  })
  
  Times_Full_USA_25_2016 <- Times_Full_USA %>% filter(World_Rank <= 25) %>% filter(Year  == "2016")
  
  output$InROI25 <- renderPlotly({
    p<-plot_ly(Times_Full_USA_25_2016, x = ~Total_Score_Calculated, y = ~ROI_InState, type="scatter", 
               text = paste("University : ", Times_Full_USA_25_2016$University_Name),
               mode = "markers", color = ~Total_Score_Calculated, alpha = 1, 
               size = ~Total_Score_Calculated) 
    p
  })
  
  output$OutROI25 <- renderPlotly({
    p<-plot_ly(Times_Full_USA_25_2016, x = ~Total_Score_Calculated, y = ~ROI_OutState, type="scatter", 
               text = paste("University : ", Times_Full_USA_25_2016$University_Name),
               mode = "markers", color = ~Total_Score_Calculated, alpha = 1, 
               size = ~Total_Score_Calculated) 
    p
  })
  
  
  Times_Full_USA_TX_2016 <- Times_Full_USA %>% filter(STABBR == "TX") %>% filter(Year  == "2016")
  Times_Full_USA_25_TX_2016 <-  rbind(  Times_Full_USA_25_2016, Times_Full_USA_TX_2016) 
  
  output$InROITX <- renderPlotly({
    p<-plot_ly(Times_Full_USA_25_TX_2016, x = ~Total_Score_Calculated, y = ~ROI_InState, type="scatter", 
               text = paste("University : ", Times_Full_USA_25_TX_2016$University_Name),
               mode = "markers", color = ~Total_Score_Calculated, alpha = 1, 
               size = ~Total_Score_Calculated) 
    p
  })
  
  output$OutROITX <- renderPlotly({
    p<-plot_ly(Times_Full_USA_25_TX_2016, x = ~Total_Score_Calculated, y = ~ROI_OutState, type="scatter", 
               text = paste("University : ", Times_Full_USA_25_TX_2016$University_Name),
               mode = "markers", color = ~Total_Score_Calculated, alpha = 1, 
               size = ~Total_Score_Calculated) 
    p
  })
  
  #Unemploy/GDP/Edu vs score
  output$UnempTOut <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    p <- Times_All_High_5 %>%
      plot_ly(
        x = ~(round(as.numeric(Umemployment_Rate), digits =1)),
        y = ~Total_Score_Calculated,
        frame = ~Year,
        type = 'scatter',
        mode = 'markers',color = ~Total_Score_Calculated, alpha = 1, 
        size = ~Total_Score_Calculated,
        showlegend = F
      )  %>%   
      add_markers(
        text = ~paste(University_Name, paste("Total Score:", Total_Score_Calculated), 
                      paste("Total Score:", (round(as.numeric(Umemployment_Rate), digits =1))),sep = "<br />"),
        color = ~Total_Score_Calculated, symbol = I("circle"), size = ~Total_Score_Calculated, hoverinfo = "text"
      ) %>%
      layout(autosize = F, width = 1080, height = 590,margin = m,
             xaxis = list(title = "Umemployment Rate",titlefont = f, zeroline = FALSE, ticksuffix = '%'), 
             yaxis = list(title = "Total Score",titlefont = f, zeroline = FALSE) )
    p
  })
  
  output$GDPTOut <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    p <- Times_All_High_5 %>%
      plot_ly(
        x = ~(round(as.numeric(GDP_Growth_Rate), digits =0)),
        y = ~Total_Score_Calculated,
        frame = ~Year,
        type = 'scatter',
        mode = 'markers',color = ~Total_Score_Calculated, alpha = 1, 
        size = ~Total_Score_Calculated,
        showlegend = F
      )  %>%   
      add_markers(
        text = ~paste(University_Name, paste("Total Score:", Total_Score_Calculated), 
                      paste("GDP Growth Rate (%):", (round(as.numeric(GDP_Growth_Rate), digits =0))),sep = "<br />"),
        color = ~Total_Score_Calculated, symbol = I("circle"), size = ~Total_Score_Calculated, hoverinfo = "text"
      ) %>%
      layout(autosize = F, width = 1080, height = 590,margin = m,
             xaxis = list(title = "GDP Growth Rate",titlefont = f, zeroline = FALSE), 
             yaxis = list(title = "Total Score",titlefont = f, zeroline = FALSE) )
    
    p
  })
  
  output$EDUTTOut <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    
    p <- Times_All_High_5 %>%
      plot_ly(
        x = ~round(as.numeric(Education_Exp_Tertiary), digits =0),
        y = ~Total_Score_Calculated,
        frame = ~Year,
        type = 'scatter',
        mode = 'markers',color = ~Total_Score_Calculated, alpha = 1, 
        size = ~round(as.numeric(GDP_Actual),digits =1),
        showlegend = F
      )  %>%   
      add_markers(
        text = ~paste(University_Name, paste("Total Score:", Total_Score_Calculated), 
                      paste("Tertiary Spend %:", round(as.numeric(Education_Exp_Tertiary), digits =0)),
                      paste("Actual GDP (in B$):", round(as.numeric(GDP_Actual), digits =1)/1000000000),
                      sep = "<br />"),
        color = ~Total_Score_Calculated, symbol = I("circle"), 
        size = ~round(as.numeric(GDP_Actual),digits =1), hoverinfo = "text"
      ) %>%
      layout(autosize = F, width = 1080, height = 590,margin = m,
             xaxis = list(title = "Education Expenditure Tertiary (% of All Educational Spend)",ticksuffix = '%',
                          titlefont = f, zeroline = FALSE), 
             yaxis = list(title = "Total Score",titlefont = f, zeroline = FALSE) )
    
    p
  })
  
  
  output$worldmapout <- renderPlotly({
    g <- list(
      scope = 'world',
      showland = TRUE,
      showlakes = TRUE,
      showframe = FALSE,
      showcoastlines = FALSE,
      lakecolor = toRGB('white'),
      #projection = list(type = 'Mercator'),
      landcolor = toRGB("gray90"),
      subunitcolor = toRGB("gray85"),
      countrycolor = toRGB("gray85"),
      countrywidth = 0.5,
      subunitwidth = 0.5
    )
    
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    
    # add_trace(
    #   z = ~Total_Score_Calculated, text = ~hover, 
    #   locations = ~code,
    #   color = ~Total_Score_Calculated, colors = 'Blues'
    # ) %>%
     Times_Full$hover <- with(Times_Full, 
                              paste(University_Name, '<br>', "Year", Year, '<br>', "State", STABBR, 
                                    '<br>', "Total Score", Total_Score_Calculated, "<br>"))
    p <- plot_geo(Times_Full, lat = ~Latitude, lon = ~Longitude,
                  frame = ~Year, showlegend = F) %>%
      colorbar(title = "Total Score") %>%
      add_markers(
        sizes=c(1,500), 
        #text = ~paste(University_Name, paste("Total Score:", Total_Score_Calculated), sep = "<br />"),
        text = ~hover,
        color = ~-Total_Score_Calculated, symbol = I("circle-dot"), 
        size = ~Total_Score_Calculated
      ) %>%
      layout( autosize = F, width = 1080, height = 590, margin = m,
              title = '<br /> University Scores - USA', geo = g) 
  })
  
  
  output$usamapout <- renderPlotly({
    g <- list(
      scope = 'usa',
      #projection = list(type = 'mercator'),
      projection = list(type = 'albers usa'),
      lonaxis = range(-30, 60) ,
      lataxis = range(30, 70) ,
      showland = TRUE,
      showlakes = TRUE,
      showframe = FALSE,
      showcoastlines = FALSE,
      lakecolor = toRGB('white'),
      landcolor = toRGB("gray90"),
      subunitcolor = toRGB("gray85"),
      subunitwidth = 1
    )
    
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    
    # add_trace(
    #   z = ~Total_Score_Calculated, text = ~hover, 
    #   locations = ~code,
    #   color = ~Total_Score_Calculated, colors = 'Blues'
    # ) %>%
    # Times_Full$hover <- with(Times_Full, 
    #                          paste(University_Name, '<br>', "Year", Year, '<br>', "State", STABBR, 
    #                                '<br>', "Total Score", Total_Score_Calculated, "<br>"))
    p <- plot_geo(Times_Full, lat = ~Latitude, lon = ~Longitude,
                  frame = ~Year, showlegend = F) %>%
      colorbar(title = "Total Score") %>%
      add_markers(
        sizes=c(1,500), 
        text = ~paste(University_Name, paste("Total Score:", Total_Score_Calculated), sep = "<br />"),
        color = ~-Total_Score_Calculated, symbol = I("circle-dot"), 
        size = ~Total_Score_Calculated
      ) %>%
      layout( autosize = F, width = 1080, height = 590, margin = m,
              title = '<br /> University Scores - USA', geo = g) 
  })
  
  output$worldEDUSPEND <- renderPlotly({
    l <- list(color = toRGB("grey"), width = 0.5)
    g <- list(
      showland = TRUE,
      showlakes = TRUE,
      showframe = FALSE,
      showcoastlines = FALSE,
      lakecolor = toRGB('white'),
      landcolor = toRGB("gray90"),
      subunitcolor = toRGB("gray90"),
      projection = list(type = 'Mercator')
    )
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 1
    )
    p <- plot_geo(GDP_All, frame = ~Year) %>%
      add_trace(
        z = ~as.numeric(Education_Expenditure_GDP), color = ~as.numeric(Education_Expenditure_GDP), colors = 'Blues',
        #text = ~Country_Code, 
        text = ~paste(Country_Code, 
                      paste("Educational Spend (% GDP):", round(as.numeric(Education_Expenditure_GDP), digits=1)), 
                      paste("Actual GDP:", round(as.numeric(GDP_Actual), digits=1)/1000000000," B$"),
                      sep = "<br />"),
        locations = ~Country_Code, marker = list(line = l)
      ) %>%
      colorbar(title = 'Educational Spend (% GDP)', ticksuffix = '%') %>%
      layout(autosize = F, width = 1080, height = 590, margin = m,
        title = '<br /> Educational Spend (% of GDP)',
        geo = g
      )
    })
  
  output$worldTop5 <- renderPlotly({
    g <- list(
      scope = 'world',
      showland = TRUE,
      showlakes = TRUE,
      showframe = FALSE,
      showcoastlines = FALSE,
      lakecolor = toRGB('white'),
      projection = list(type = 'Mercator'),
      landcolor = toRGB("gray90"),
      subunitcolor = toRGB("gray85"),
      countrycolor = toRGB("gray85"),
      countrywidth = 0.5,
      subunitwidth = 0.5
    )
    
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    
    # Times_Full$hover <- with(Times_Full, 
    #                          paste(University_Name, '<br>', "Year", Year, '<br>', "State", STABBR, 
    #                                '<br>', "Total Score", Total_Score_Calculated, "<br>"))
    p <- plot_geo(Times_All_High_5, lat = ~Latitude, lon = ~Longitude,
                  frame = ~Year, showlegend = F) %>%
      colorbar(title = "Total Score") %>%
      add_markers(
        sizes=c(1,500), 
        text = ~paste(University_Name, paste("Total Score:", Total_Score_Calculated), 
                      paste("Country Code: ", Country_Code),
                      paste("World Rank: ", World_Rank),sep = "<br />"),
        color = ~-Total_Score_Calculated, symbol = I("circle-dot"), 
        size = ~Total_Score_Calculated
      ) %>%
      layout( autosize = F, width = 1080, height = 590, margin = m,
              title = '<br /> University Scores - USA', geo = g) 
  })
  
  output$usamaproiin <- renderPlotly({
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showland = TRUE,
      showlakes = TRUE,
      lakecolor = toRGB('white'),
      #projection = list(type = 'Mercator'),
      landcolor = toRGB("gray95"),
      subunitcolor = toRGB("gray85"),
      countrycolor = toRGB("gray85"),
      countrywidth = 0.5,
      subunitwidth = 0.5
    )
    
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    
    # add_trace(
    #   z = ~Total_Score_Calculated, text = ~hover, 
    #   locations = ~code,
    #   color = ~Total_Score_Calculated, colors = 'Blues'
    # ) %>%
    Times_Full$hover <- with(Times_Full, 
                             paste(University_Name, '<br>', "Year", Year, '<br>', "State", STABBR, 
                                   '<br>', "In State ROI:", ROI_InState, "<br>"))
    p <- plot_geo(Times_Full, locationmode = 'zipcode', lat = ~Latitude, lon = ~Longitude,
                  frame = ~Year, showlegend = F) %>%
      colorbar(title = "In State ROI") %>%
      add_markers(
        sizes=c(1,500), 
        text = ~paste(University_Name, paste("In State ROI:", ROI_InState), sep = "<br />"),
        color = ~-ROI_InState, symbol = I("circle-dot"), 
        size = ~ROI_InState
      ) %>%
      layout( autosize = F, width = 1080, height = 590, margin = m,
              title = '<br /> University ROI (In_State) - USA', geo = g) 
  })
  
  output$usamaproiout <- renderPlotly({
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showland = TRUE,
      showlakes = TRUE,
      lakecolor = toRGB('white'),
      #projection = list(type = 'Mercator'),
      landcolor = toRGB("gray95"),
      subunitcolor = toRGB("gray85"),
      countrycolor = toRGB("gray85"),
      countrywidth = 0.5,
      subunitwidth = 0.5
    )
    
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    
    # add_trace(
    #   z = ~Total_Score_Calculated, text = ~hover, 
    #   locations = ~code,
    #   color = ~Total_Score_Calculated, colors = 'Blues'
    # ) %>%
    Times_Full$hover <- with(Times_Full, 
                             paste(University_Name, '<br>', "Year", Year, '<br>', "State", STABBR, 
                                   '<br>', "Out State ROI:", ROI_OutState, "<br>"))
    p <- plot_geo(Times_Full, locationmode = 'zipcode', lat = ~Latitude, lon = ~Longitude,
                  frame = ~Year, showlegend = F) %>%
      colorbar(title = "Out Of State ROI") %>%
      add_markers(
        sizes=c(1,500), 
        text = ~paste(University_Name, paste("Out of State ROI:", ROI_OutState), sep = "<br />"),
        color = ~-ROI_OutState, symbol = I("circle-dot"), 
        size = ~ROI_OutState
      ) %>%
      layout( autosize = F, width = 1080, height = 590, margin = m,
              title = '<br /> University ROI (Out Of State) - USA', geo = g) 
  })
  
  output$Grad4Out <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    p <- Times_Grad_Full %>%
      plot_ly(
        x = ~(round(as.numeric(Grate_4yrs_tot), digits =1)),
        y = ~Total_Score_Calculated,
        frame = ~Year,
        type = 'scatter',
        mode = 'markers',
        color = ~-Total_Score_Calculated, 
        alpha = 1, 
        symbol = I("circle"),
        size = ~Total_Score_Calculated,
        showlegend = F,
        text = ~paste(University_Name, paste("Total Score:", Total_Score_Calculated),
                      paste("4 Year Rate:", round(as.numeric(Grate_4yrs_tot), digits =1)),sep = "<br />"),
        hoverinfo = "text"
      )  %>%   
      add_trace(x = ~(round(as.numeric(Grate_5yrs_tot), digits =1)),
                ~Total_Score_Calculated,
                frame = ~Year,
                type = 'scatter',
                mode = 'markers',
                color = ~-Total_Score_Calculated, 
                alpha = 1, 
                symbol = I("diamond"),
                size = ~Total_Score_Calculated,
                text = ~paste(University_Name, paste("Total Score:", Total_Score_Calculated), 
                              paste("5 Year Rate:", round(as.numeric(Grate_5yrs_tot), digits =1)),sep = "<br />"),
                hoverinfo = "text",
                showlegend = F) %>%
      add_trace(x = ~(round(as.numeric(Grate_6yrs_tot), digits =1)),
                ~Total_Score_Calculated,
                frame = ~Year,
                type = 'scatter',
                mode = 'markers',
                color = ~-Total_Score_Calculated, 
                alpha = 1, 
                symbol = I("square"),
                size = ~Total_Score_Calculated,
                text = ~paste(University_Name, paste("Total Score:", Total_Score_Calculated), 
                              paste("6 Year Rate:", round(as.numeric(Grate_6yrs_tot), digits =1)), sep = "<br />"),
                hoverinfo = "text",
                showlegend = F) %>%
      colorbar(title = "Total score") %>%
      layout(autosize = F, width = 1080, height = 590, margin = m,
             xaxis = list(title = "Graduation Rates 4 vs 5 vs 6 Years",ticksuffix = '%',titlefont = f, zeroline = FALSE), 
             yaxis = list(title = "Total Score",titlefont = f, zeroline = FALSE) )
    
    p
  })
  
  
  
  output$GradTVsAis <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    total_spend <- 
      p <- Times_Grad_Full %>%
      plot_ly(
        x = ~Avg_amt_aids,
        y = ~(round(as.numeric(Grate_Total), digits =1)),
        frame = ~Year,
        type = 'scatter',
        mode = 'markers',
        color = ~-Total_Score_Calculated,
        alpha = 1, 
        size = ~Total_Score_Calculated,
        showlegend = F
      )  %>%   
      add_markers(
        text = ~paste(University_Name, paste("Total Graduation Rate:", Grate_Total,"Avg Aid Amount:", Avg_amt_aids), sep = "<br />"),
        symbol = I("circle"), size = ~Grate_Total, hoverinfo = "text"
      ) %>%
      layout(autosize = F, width = 1080, height = 590, margin = m,
             xaxis = list(title = "Avg Aid Amount",titlefont = f, zeroline = FALSE) ,
             yaxis = list(title = "Total Graduation Rate",titlefont = f, zeroline = FALSE)
             )
    
    p
  })
  
  
  output$GradTVTuition <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    p <- Times_Grad_Full %>%
      plot_ly(
        x = ~avg_tuition_amt_aid,
        y = ~(round(as.numeric(Grate_Total), digits =1)),
        frame = ~Year,
        type = 'scatter',
        mode = 'markers',
        color = ~-Total_Score_Calculated,
        alpha = 1, 
        size = ~Total_Score_Calculated,
        showlegend = F
      )  %>%   
      add_markers(
        text = ~paste(University_Name, paste("Total Graduation Rate:", Grate_Total,
                                             "Weighted Tutition Per Student:", avg_tuition_amt_aid), sep = "<br />"),
        symbol = I("circle"), size = ~Grate_Total, hoverinfo = "text"
      ) %>%
      layout(autosize = F, width = 1080, height = 590, margin = m,
             xaxis = list(title = "Weighted Tutition Per Student",titlefont = f, zeroline = FALSE), 
             yaxis = list(title = "Total Graduation Rate",titlefont = f, zeroline = FALSE) )
    
    p
  })
  
  output$Admission_Graduation_Rate <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    p <- Times_Grad_Full %>%
      plot_ly(
        x = ~adm_rate_tot,
        y = ~(round(as.numeric(Grate_Total), digits =1)),
        frame = ~Year,
        type = 'scatter',
        mode = 'markers',
        color = ~-Total_Score_Calculated,
        alpha = 1, 
        size = ~-Total_Score_Calculated,
        showlegend = F
      )  %>%   
      add_markers(
        text = ~paste(University_Name, paste("Total Graduation Rate:", Grate_Total,
                                             "Admission Rate:", adm_rate_tot), sep = "<br />"),
        symbol = I("circle"), size = ~Grate_Total, hoverinfo = "text"
      ) %>%
      layout(autosize = F, width = 1080, height = 590, margin = m,
             xaxis = list(title = "Admission Rate",titlefont = f, zeroline = FALSE), 
             yaxis = list(title = "Total Graduation Rate",titlefont = f, zeroline = FALSE) )
    
    p
  })
  
  output$Male_Female_Graduation_Rate <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    p <- Times_Grad_Full %>%
      plot_ly(
        x = ~adm_rate_women,
        y = ~(round(as.numeric(Grate_Wmn), digits =1)),
        frame = ~Year,
        type = 'scatter',
        mode = 'markers',
        color = 'pink',
        alpha = 1, 
        size = ~-Total_Score_Calculated,
        text = ~paste(University_Name, paste("Total Graduation Rate:", Grate_Total), 
                      paste("Female Graduation Rate:", round(as.numeric(Grate_Wmn), digits =1)),
                      paste("Female Admission Rate:", round(as.numeric(adm_rate_women), digits =1)),
                      sep = "<br />"),
        showlegend = F
      )  %>%   
      # add_markers(
      #   text = ~paste(University_Name, paste("Total Graduation Rate:", Grate_Total,
      #                                        "Female Admission Rate:", adm_rate_women), sep = "<br />"),
      #   symbol = I("circle"), size = ~Grate_Total, hoverinfo = "text"
      # ) %>%
      add_trace( x = ~adm_rate_men,
                 y = ~(round(as.numeric(Grate_Men), digits =1)),
                 frame = ~Year,
                 type = 'scatter',
                 mode = 'markers',
                 color = 'blue', 
                 alpha = 1, 
                 symbol = I("diamond"),
                 size = ~-Total_Score_Calculated,
                 text = ~paste(University_Name, paste("Total Graduation Rate:", Grate_Total), 
                               paste("Male Graduation Rate:", round(as.numeric(Grate_Men), digits =1)),
                               paste("Male Admission Rate:", round(as.numeric(adm_rate_men), digits =1)),
                               sep = "<br />"),
                 hoverinfo = "text",
                 showlegend = F) %>%
      layout(autosize = F, width = 1080, height = 590, margin = m,
             xaxis = list(title = "Male/Female Admission Rate",titlefont = f, zeroline = FALSE,ticksuffix = '%'), 
             yaxis = list(title = "Total Graduation Rate",titlefont = f, zeroline = FALSE) )
    
    p
  })
  
  output$Inst_exp_Grad_Rate <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    p <- Times_Grad_Full %>%
      plot_ly(
        x = ~(round(as.numeric(exp_inst*exp_tot*0.01), digits =1)),
        y = ~(round(as.numeric(Grate_4yrs_tot), digits =1)),
        frame = ~Year,
        type = 'scatter',
        mode = 'markers',
        color = ~-(round(as.numeric(Grate_4yrs_tot), digits =1)), 
        alpha = 1, 
        symbol = I("circle"),
        size = ~(round(as.numeric(Grate_4yrs_tot), digits =1)),
        showlegend = F,
        text = ~paste(University_Name, paste("Total Score:", Total_Score_Calculated),
                      paste("4 Year Rate:", round(as.numeric(Grate_4yrs_tot), digits =1)),
                      paste("Instituional Expenses:", round(as.numeric(exp_inst*exp_tot*0.01), digits =1)),
                      sep = "<br />"),
        hoverinfo = "text"
      )  %>%   
      add_trace( x = ~(round(as.numeric(exp_inst*exp_tot*0.01), digits =1)),
                y = ~(round(as.numeric(Grate_5yrs_tot), digits =1)),
                frame = ~Year,
                type = 'scatter',
                mode = 'markers',
                color = ~-(round(as.numeric(Grate_5yrs_tot), digits =1)), 
                alpha = 1, 
                symbol = I("diamond"),
                size = ~-(round(as.numeric(Grate_5yrs_tot), digits =1)),
                text = ~paste(University_Name, paste("Total Score:", Total_Score_Calculated), 
                              paste("5 Year Rate:", round(as.numeric(Grate_5yrs_tot), digits =1)),
                              paste("Instituional Expenses:", round(as.numeric(exp_inst*exp_tot*0.01), digits =1)),
                              sep = "<br />"),
                hoverinfo = "text",
                showlegend = F) %>%
      add_trace(x = ~(round(as.numeric(exp_inst*exp_tot*0.01), digits =1)),
                y = ~(round(as.numeric(Grate_6yrs_tot), digits =1)),
                frame = ~Year,
                type = 'scatter',
                mode = 'markers',
                color = ~-(round(as.numeric(Grate_6yrs_tot), digits =1)),
                alpha = 1, 
                symbol = I("square"),
                size = ~-(round(as.numeric(Grate_6yrs_tot), digits =1)),
                text = ~paste(University_Name, paste("Total Score:", Total_Score_Calculated), 
                              paste("6 Year Rate:", round(as.numeric(Grate_6yrs_tot), digits =1)),
                              paste("Instituional Expenses:", round(as.numeric(exp_inst*exp_tot*0.01), digits =1)),
                              sep = "<br />"),
                hoverinfo = "text",
                showlegend = F) %>%
      colorbar(title = "Graduation Rates") %>%
      layout( autosize = F, width = 1080, height = 590, margin = m,
             xaxis = list(title = "Instituional Expenses",titlefont = f, zeroline = FALSE),
             yaxis = list(title = "Graduation Rates 4/5/6 Years",titlefont = f, zeroline = FALSE))
    p
  })
  
  
  output$Res_exp_Grad_Rate <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    p <- Times_Grad_Full %>%
      plot_ly(
        x = ~(round(as.numeric(exp_res*exp_tot*0.01), digits =1)),
        y = ~(round(as.numeric(Grate_Total), digits =1)),
        frame = ~Year,
        type = 'scatter',
        mode = 'markers',
        color = ~-(round(as.numeric(Grate_Total), digits =1)), 
        alpha = 1, 
        symbol = I("circle"),
        size = ~(round(as.numeric(Grate_Total), digits =1)),
        showlegend = F,
        text = ~paste(University_Name, paste("Total Score:", Total_Score_Calculated),
                      paste("Total Graduation Rate:", round(as.numeric(Grate_Total), digits =1)),
                      paste("Instituional Expenses:", round(as.numeric(exp_res*exp_tot*0.01), digits =1)),
                      sep = "<br />"),
        hoverinfo = "text"
      )  %>%   
      colorbar(title = "Graduation Rates") %>%
      layout( autosize = F, width = 1080, height = 590, margin = m,
              xaxis = list(title = "Research Expenses",titlefont = f, zeroline = FALSE),
              yaxis = list(title = "Total Graduation Rate",titlefont = f, zeroline = FALSE))
    p
  })
  
  output$Academic_exp_Grad_Rate <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    p <- Times_Grad_Full %>%
      plot_ly(
        x = ~(round(as.numeric(exp_academic*exp_tot*0.01), digits =1)),
        y = ~(round(as.numeric(Grate_Total), digits =1)),
        frame = ~Year,
        type = 'scatter',
        mode = 'markers',
        color = ~-(round(as.numeric(Grate_Total), digits =1)), 
        alpha = 1, 
        symbol = I("circle"),
        size = ~(round(as.numeric(Grate_Total), digits =1)),
        showlegend = F,
        text = ~paste(University_Name, paste("Total Score:", Total_Score_Calculated),
                      paste("Total Graduation Rate:", round(as.numeric(Grate_Total), digits =1)),
                      paste("Academic Expenses:", round(as.numeric(exp_academic*exp_tot*0.01), digits =1)),
                      sep = "<br />"),
        hoverinfo = "text"
      )  %>%   
      colorbar(title = "Graduation Rates") %>%
      layout( autosize = F, width = 1080, height = 590, margin = m,
              xaxis = list(title = "Academic Expenses",titlefont = f, zeroline = FALSE),
              yaxis = list(title = "Total Graduation Rate",titlefont = f, zeroline = FALSE))
    p
  })
  
  

  output$UTSA_exp_Grad_Rate <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    p <- Times_Grad_Full_UTSA %>%
      plot_ly(
        x = ~(round(as.numeric(exp_tot*exp_student*0.01), digits =1)),
        y = ~Year,
        #frame = ~Year,
        type = 'scatter',
        mode = 'markers',
        #width = 3, 
        #dash = 'dash',
        color = ~University_Name, 
        alpha = 1, 
       # symbol = I("circle"),
        #size = ~(round(as.numeric(Grate_Total), digits =1)),
        showlegend = F,
        text = ~paste(University_Name, paste("Total Score:", Total_Score_Calculated),
                      paste("Graduation Rate:", round(as.numeric(Grate_Total), digits =1)),
                      paste("Student Expenses:", round(as.numeric(exp_tot*exp_student*0.01), digits =1)),
                      sep = "<br />"),
        hoverinfo = "text"
      )  %>%   
    add_trace( z = ~(round(as.numeric(Grate_Total), digits =1)),
               #y = ~Year,
               #frame = ~Year,
               type = 'scatter',
               mode = 'markers',
               #width = 1, dash = 'dash',
               color = ~University_Name, 
               alpha = 1, 
               #symbol = I("diamond"),
               #size = ~-(round(as.numeric(Grate_Total), digits =1)),
               text = ~paste(University_Name, paste("Total Score:", Total_Score_Calculated), 
                             paste("Graduation Rate:", round(as.numeric(Grate_Total), digits =1)),
                             paste("Instituional Expenses:", round(as.numeric(exp_inst*exp_tot*0.01), digits =1)),
                             sep = "<br />"),
               hoverinfo = "text",
               showlegend = F) %>%
      add_trace( z = ~(round(as.numeric(exp_inst*exp_tot*0.01), digits =1)),
                 y = ~Year,
                 #frame = ~Year,
                 type = 'scatter',
                 mode = 'lines',
                 #width = 1, dash = 'dash',
                 color = ~University_Name, 
                 alpha = 1, 
                 #symbol = I("diamond"),
                 #size = ~-(round(as.numeric(Grate_Total), digits =1)),
                 text = ~paste(University_Name, paste("Total Score:", Total_Score_Calculated), 
                               paste("Graduation Rate:", round(as.numeric(Grate_Total), digits =1)),
                               paste("Instituional Expenses:", round(as.numeric(exp_inst*exp_tot*0.01), digits =1)),
                               sep = "<br />"),
                 hoverinfo = "text",
                 showlegend = F) %>%
      add_trace( x = ~(round(as.numeric(exp_res*exp_tot*0.01), digits =1)),
                 y = ~Year,
                 #frame = ~Year,
                 type = 'scatter',
                 mode = 'lines',
                 width = 1, 
                 #dash = 'dash',
                 color = ~University_Name, 
                 alpha = 1, 
                 #symbol = I("diamond"),
                 #size = ~-(round(as.numeric(Grate_Total), digits =1)),
                 text = ~paste(University_Name, paste("Total Score:", Total_Score_Calculated), 
                               paste("Graduation Rate:", round(as.numeric(Grate_Total), digits =1)),
                               paste("Research Expenses:", round(as.numeric(exp_res*exp_tot*0.01), digits =1)),
                               sep = "<br />"),
                 hoverinfo = "text",
                 showlegend = F) %>%
      add_trace(z = ~(round(as.numeric(exp_academic*exp_tot*0.01), digits =1)),
                y = ~Year,
                #frame = ~Year,
                type = 'scatter',
                mode = 'lines',
                #width = 1, dash = 'dash',
                color = ~University_Name,
                alpha = 1, 
               # symbol = I("square"),
               # size = ~-(round(as.numeric(Grate_Total), digits =1)),
                text = ~paste(University_Name, paste("Total Score:", Total_Score_Calculated), 
                              paste("Graduation Rate:", round(as.numeric(Grate_Total), digits =1)),
                              paste("Academic Expenses:", round(as.numeric(exp_academic*exp_tot*0.01), digits =1)),
                              sep = "<br />"),
                hoverinfo = "text",
                showlegend = F) %>%
      colorbar(title = "Total Graduation Rate") %>%
      layout( autosize = F, width = 1080, height = 590, margin = m,
        xaxis = list(title = "University Expenses",titlefont = f, zeroline = FALSE),
        yaxis = list(title = "Graduation Rate",titlefont = f, zeroline = FALSE))
    p
  })
  
  
  
  
  #Stop Shiny App after session Ends
  #session$onSessionEnded(stopApp)
}
shinyApp(ui, server)
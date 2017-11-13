library(RODBC)
library(Matrix)
library(arules)
library(arulesViz)
library(ibmdbR)
library(dplyr)
library(plotly)
library(highcharter)
library(lubridate)
library(ggthemes)
library(tidyr)
library(viridis)
library(shiny)
library(ggplot2)

# @hidden_cell
# This connection object is used to access your data and contains your credentials.
# You might want to remove those credentials before you share your notebook.

# con.e <- idaConnect("DATABASE=BLUDB;HOSTNAME=dashdb-entry-yp-dal09-07.services.dal.bluemix.net;PORT=50000;PROTOCOL=TCPIP;UID=dash8075;PWD=BOEDMd6bgPOP;", uid = "dash8075", pwd = "BOEDMd6bgPOP", conType = "rodbc")
# idaInit(con.e)

dsn_driver <- "BLUDB"  
dsn_database <- "BLUDB"  
dsn_hostname <- "dashdb-entry-yp-dal09-07.services.dal.bluemix.net"  
dsn_port <- "50000"  
dsn_uid <- "dash8075"  
dsn_pwd <- "BOEDMd6bgPOP"

conn_path <- paste(dsn_driver,  
                   ";DATABASE=",dsn_database,
                   ";HOSTNAME=",dsn_hostname,
                   ";PORT=",dsn_port,
                   ";UID=",dsn_uid,
                   ";PWD=",dsn_pwd,sep="")

ch <- idaConnect(conn_path)  
idaInit(ch)  


Vehicle <- as.data.frame(ida.data.frame('DASH8075.DATABASE'))
head(Vehicle)
str(Vehicle)

# You can close the connection with the following code:
# idaClose(con.e)

## Create the date and time variable 
Vehicle$DATE_TIME = paste(Vehicle$DATE,Vehicle$TIME)
Vehicle$day = wday(Vehicle$DATE_TIME,label = T)
Vehicle$month = month(Vehicle$DATE_TIME,label = T)
Vehicle$hour = hour(Vehicle$DATE_TIME)


##########   Number of Accidents
#####  Number of Accidents Per Person
## Calculate number of accidents by area
temp_1 = Vehicle %>% group_by(BOROUGH) %>% summarise(n=n())
## Sign the popolation of each Borough 
temp_1$pop = rep(0,dim(temp_1)[1])
temp_1$pop = ifelse(temp_1$BOROUGH=="MANHATTAN",1644158,temp_1$pop)
temp_1$pop = ifelse(temp_1$BOROUGH=="BRONX",1455444,temp_1$pop)
temp_1$pop = ifelse(temp_1$BOROUGH=="QUEENS",2339150,temp_1$pop)
temp_1$pop = ifelse(temp_1$BOROUGH=="STATEN ISLAND",474558,temp_1$pop)
temp_1$pop = ifelse(temp_1$BOROUGH=="BROOKLYN",2636735,temp_1$pop)
temp_1$per_cap = temp_1$n/temp_1$pop
temp_1 %>% ggplot(aes(x=BOROUGH,y=per_cap))+
  geom_bar(stat="identity")+
  ggtitle("Number of Accidents Per Person in each Borough")+
  ylab("Number of Accidents Per Person")    


Vehicle %>% filter(BOROUGH!="") %>%  group_by(date,BOROUGH) %>% summarise(n=mean(n())) %>% na.omit() %>%
  ggplot(aes(x=date, y=n, colour=BOROUGH, group=BOROUGH)) + geom_line()+geom_point(size=2,shape=1)+theme_hc(bgcolor = "darkunica") + scale_x_date('month') +
  scale_fill_hc("darkunica") +ggtitle("Borough Accidents(Mean) by Time")+geom_text(aes(label=ifelse(n>150,n,"")), size=3,hjust=1.8)

ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") +
  xlab("")



##### Mean Number of Collisions per Month
Vehicle %>% filter(BOROUGH!="") %>% group_by(month,BOROUGH) %>% summarise(n=mean(n())) %>% na.omit() %>%
  ggplot(aes(x=month, y=n, fill=BOROUGH)) +
  geom_bar(position="dodge",stat = "identity")+geom_text(aes(label=n), vjust=1.5, colour="black",
                                                         position=position_dodge(.9), size=3)+ggtitle("Mean Number of Collisions Per Month")+
  ylab('Number of Collisions')


### Which Day had the highest mean number of Accidents?
## - Do weekends have higher accidents than week days?
Vehicle %>% group_by(BOROUGH,day)%>% summarise(n=mean(n())) %>% filter(BOROUGH!="") %>%
  ggplot(aes(x=day, y=n, fill=BOROUGH)) +
  geom_bar(position="dodge",stat = "identity")+geom_text(aes(label=n), vjust=1.5, colour="black",
                                                         position=position_dodge(.9), size=3)+ggtitle("Mean Number of Collisions Per Day")+
  ylab('Number of Collisions')


### Reasons for accidents to take place
Table_1 = Vehicle %>% select(PERSONS_KILLED,PERSONS_INJURED,VEHICLE_1_FACTOR,VEHICLE_2_FACTOR,VEHICLE_3_FACTOR,VEHICLE_4_FACTOR,VEHICLE_5_FACTOR) %>% gather(type,value,1:2) %>% gather(vehicle_type,cause,1:5) %>% filter(value!=0,cause!="",cause!="Unspecified")
Table_2 = Table_1 %>% select(-vehicle_type) %>% group_by(type,cause) %>% summarise(total=sum(value,na.rm=T))
ggplot(data = Table_2, aes(x = cause, y = log(total), fill = type)) +
  geom_bar(data = subset(Table_2, type=="PERSONS_INJURED"),
           stat = "identity") +
  geom_bar(data = subset(Table_2, type=="PERSONS_KILLED"),
           stat = "identity",
           position = "identity",
           mapping = aes(y = -log(total))) +
  scale_y_continuous(labels = abs) +
  coord_flip()+ggtitle('Causes of Accidents') +
  xlab('Cause')+
  ylab('Log Total')



#### Location Wise Plot(Injuries)
# install.packages('ggmap')
library(ggmap)
df = Vehicle %>% select(LATITUDE,LONGITUDE,PEDESTRIANS_INJURED,CYCLISTS_INJURED,MOTORISTS_INJURED) %>% gather(type,value,3:5) %>% na.omit() %>% group_by(LATITUDE,LONGITUDE,type) %>% summarise(total=sum(value,na.rm=T)) %>% filter(total!=0)
nyc = get_map("new york")
ggmap(nyc)

Vehicle$LATITUDE_2 = as.numeric(Vehicle$LATITUDE)
Vehicle$LONGITUDE_2 = as.numeric(Vehicle$LONGITUDE)
df_2 = Vehicle %>% select(LATITUDE_2,LONGITUDE_2,PEDESTRIANS_INJURED,CYCLISTS_INJURED,MOTORISTS_INJURED) %>% gather(type,value,3:5) %>% na.omit() %>% group_by(LATITUDE_2,LONGITUDE_2,type) %>% summarise(total=sum(value,na.rm=T)) %>% filter(total!=0)

g1 <- ggmap(nyc)+geom_point(data=subset(df_2,type=="PEDESTRIANS_INJURED"), 
                            aes(x=LONGITUDE_2, y=LATITUDE_2, colour=total),size=1,alpha=0.2) +
  ggtitle("Pedestrians Injured")+scale_color_continuous(low = "red",  high = "black")

g2 <- ggmap(nyc)+geom_point(data=subset(df_2,type=="CYCLISTS_INJURED"), 
                            aes(x=LONGITUDE_2, y=LATITUDE_2, colour=total),size=1,alpha=0.2) +
  ggtitle("Cyclists Injured")+scale_color_continuous(low = "red",  high = "black")

g3 <- ggmap(nyc)+geom_point(data=subset(df_2,type=="MOTORISTS_INJURED"), 
                            aes(x=LONGITUDE_2, y=LATITUDE_2, colour=total),size=1,alpha=0.2) +
  ggtitle("Motorists Injured")+scale_color_continuous(low = "red",  high = "black")
g1
g2
g3


########## Shinny web interface ###### 
library(leaflet)
library(shinydashboard)
library(leaflet)
library(osrm)
# Define UI for application that draws a accident map
vars = c(
  "Motorists" = "MOTORISTS_INJURED",
  "Pedestrians" = "PEDESTRIANS_INJURED",
  "Cyclists" = "CYCLISTS_INJURED"
)
# Vehicle$month = as.character(Vehicle$month)
var_mon = c(
  "January" = "Jan",
  "Febuary" = "Feb",
  "March" = "Mar",
  "April" = "Apr",
  "May" = "May",
  "June" = "Jun",
  "July" = "Jul",
  "August" = "Aug",
  "September" = "Sep",
  "October" = "Oct",
  "November" = "Nov",
  "December" = "Dec"
)
ui = fluidPage(
  titlePanel("Vehicle Collision"),
  navbarPage("Number of Collisions", id="nav",
             tabPanel("Interactive map",
                      div(class="outer"),
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 110, left = "auto", right = 20, bottom = "auto",
                                    width = 650, height = "auto",
                                    
                                    h2("Collision explorer"),
                                    
                                    selectInput("months", "Month", var_mon),
                                    selectInput("type", "Persona", vars),
                                    #conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                     # Only prompt for threshold when coloring or sizing by superzip
                                                     #numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                    #) ,
                                    
                                    plotOutput('plot2', height = 500)
                                    #plotOutput("scatterCollegeIncome", height = 250)
                      ),
                      mainPanel(
                        plotOutput('plot1',width = "100%", height = "800px")
                      )
             ),
             tabPanel("Data explorer",
                      fluidRow(
                        column(4,
                               selectInput("BOROUGH", "Borough", c("All Boroughs", unique(Vehicle$BOROUGH)))
                        ),
                        column(4,
                               selectInput("Months","Month", c("All",as.character(sort(unique(Vehicle$month))))
                               )
                        ),
                        column(4,
                               selectInput("hour", "Hour in Day", c("All Day", sort(unique(Vehicle$hour))))
                        )
                      ),
                      # Create a new row for the table.
                      fluidRow(
                        DT::dataTableOutput("table")
                      )
             )
             
  )
)

# Define server logic required to draw a histogram
server = function(input, output) {
  # Filter data based on selections
  months = 'Dec'
  df_1 = Vehicle[Vehicle$month == months,]
  df_2 <- df_1 %>% select(LATITUDE_2,LONGITUDE_2,PEDESTRIANS_INJURED,CYCLISTS_INJURED,MOTORISTS_INJURED) %>% gather(type,value,3:5) %>% na.omit() %>% group_by(LATITUDE_2,LONGITUDE_2,type) %>% summarise(total=sum(value,na.rm=T)) %>% filter(total!=0)
  
  output$plot1 <- renderPlot({
  g1 <- ggmap(nyc)+geom_point(data=subset(df_2,type == input$type), 
                              aes(x=LONGITUDE_2, y=LATITUDE_2, colour=total),size=1,alpha=0.2) +
    ggtitle("Number of People Injured")+scale_color_continuous(low = "red",  high = "black")
  print(g1)
  })
  
  Table_1 <- df_1 %>% select(PERSONS_KILLED,PERSONS_INJURED,VEHICLE_1_FACTOR,VEHICLE_2_FACTOR,VEHICLE_3_FACTOR,VEHICLE_4_FACTOR,VEHICLE_5_FACTOR) %>% gather(type,value,1:2) %>% gather(vehicle_type,cause,1:5) %>% filter(value!=0,cause!="",cause!="Unspecified")
  Table_2 <- Table_1 %>% select(-vehicle_type) %>% group_by(type,cause) %>% summarise(total=sum(value,na.rm=T))
  output$plot2 = renderPlot({
    g2 = ggplot(data = Table_2, aes(x = cause, y = log(total), fill = type)) + 
      geom_bar(data = subset(Table_2, type=="PERSONS_INJURED"),
               stat = "identity") +
      geom_bar(data = subset(Table_2, type=="PERSONS_KILLED"),
               stat = "identity",
               position = "identity",
               mapping = aes(y = -log(total))) +
      scale_y_continuous(labels = abs) +
      coord_flip()+ggtitle('Causes of Accidents') +
      xlab('Cause')+
      ylab('Log Total')
    print(g2)
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- Vehicle
    datavars <- names(data) %in% c("LATITUDE", "LONGITUDE", "LOCATION","VEHICLE_2_TYPE","VEHICLE_3_TYPE",
                                     "VEHICLE_4_TYPE","VEHICLE_5_TYPE","VEHICLE_3_FACTOR","VEHICLE_4_FACTOR","VEHICLE_5_FACTOR",
                                     "DATE_TIME","LATITUDE_2","LONGITUDE_2","OFF_STREET_NAME")
    newdata <- data[!datavars]
    if (input$BOROUGH != "All Boroughs") {
      newdata <- newdata[newdata$BOROUGH == input$BOROUGH,]
    }
    if (input$Months != "All") {
      newdata <- newdata[newdata$month == input$Months,]
    }
    if (input$hour != "All Day") {
      newdata <- newdata[newdata$hour == input$hour,]
    }
    datavars2 <- names(newdata) %in% c("month","hour")
    newdata2 = newdata[!datavars2]
  }))
  
}

# Run the application 
shinyApp(ui = ui, server = server)







########## time series attempt ###########  
trunc = 1:27000
pre_data = as.data.frame(cbind(summary$date, summary$per_mi_cap,Intercept = 1,summary[,2:3]))
train = pre_data[trunc,]
test = pre_data[-trunc,]

## Train model - Generalized Linear Autoregressive Moving Average Models with Various Distributions
library(glarma)
y = train[,1]
X = as.matrix(train[,2:4])
glarmamod = glarma(y, X, thetaLags = c(5,10,20))

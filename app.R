
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)

#############################################


header <- dashboardHeader(title = "KMPG TASK 3")

##############################################
sidebar <- dashboardSidebar( 
  sidebarMenu(
  menuItem("Dashboard",
           tabName = "dashboard", 
           icon = icon("dashboard")),
  menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
           href=""),
  
  selectInput(
    "month",
    "Month:",
    list(
      "All Year" = 99,
      "January" = 1,
      "February" = 2,
      "March" = 3,
      "April" = 4,
      "May" = 5,
      "June" = 6,
      "July" = 7,
      "August" = 8,
      "September" = 9,
      "October" = 10,
      "November" = 11,
      "December" = 12
    ),
    selected = "All Year",
    selectize = FALSE
  )
  
  
  )
)


#############################################
frow1 <- fluidRow(
  valueBoxOutput("numberofcustomers"),
  valueBoxOutput("approvalBox")
  )

frow2 <- fluidRow( 
  box(
    title = "Total profit based on industry"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("profit_based_on_industry", height = "300px")
  ),
  box(
    title = "Total profit based on wealth segment"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("profit_based_on_wealth_segment", height = "300px")
  )
)

frow3<-fluidRow(
box(
    title = "Total profit based on states"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("profit_based_on_states", height = "300px")
  ),
box(
  title = "Number of bicycles purchased per month"
  ,status = "primary"
  ,solidHeader = TRUE 
  ,collapsible = TRUE 
  ,plotOutput("profit_based_on_gender", height = "300px")
  )
)

frow4<-fluidRow(
  box(
    title = "Total profit based on age groups"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("profit_based_on_agegroups", height = "300px")
  ),
box(
  title = "Most purchased brands among customers"
  ,status = "primary"
  ,solidHeader = TRUE 
  ,collapsible = TRUE 
  ,plotOutput("most_purchased_brands", height = "300px")
  )
)

frow5<-fluidRow(
  box(
    title = "Most purchased products among customers"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("most_purchased_products", height = "300px")
    )
  )
 

body <- dashboardBody(frow1,frow2,frow3,frow4,frow5)

################################################

ui <- dashboardPage(title = 'KMPG TASK 3',header, sidebar, body, skin='blue')



server <- function(input, output) {
  datum<- read.csv("merged_data2.csv")
  #merged_data2<-read.csv("merged_data2.csv")
  prac_data<- read.csv("prac_data.csv")
  job_industry_category.profit<- read.csv("job_industry_category.profit.csv")
  wealth_segment.profit<-read.csv("wealth_segment.profit.csv")
  state.profit<-read.csv("state.profit.csv")
  d<-read.csv("d.csv")
  d<- read.csv("d.csv")
  monthly_data_2<- read.csv("monthly_data_2.csv")
  age_group.profit<-read.csv("age_group.profit.csv")
 # product_line.profit<-read.csv("product_line.profit")
  brand.profit<-read.csv("brand.profit.csv")
  
  
  
#################################################
  output$approvalBox <- renderValueBox({
    total.revenue <- sum(datum$profit)
    valueBox(
      formatC(total.revenue, format="d", big.mark=',')
      ,'Total Profit'
      ,icon = icon("usd",lib='glyphicon')
      ,color = "blue") 
  }) 
################################################
  output$numberofcustomers<-renderValueBox({
    number_of_customers<-length(datum$customer_id)
    valueBox(
      formatC(number_of_customers, format="d", big.mark=',')
      ,'Total number of customers'
      ,icon = icon("bike",lib='glyphicon')
      ,color = "blue") 
  })  
#############################################
output$profit_based_on_industry<- renderPlot({
  profit_based_on_industry_plot1=ggplot(job_industry_category.profit, aes(Total_profit, job_industry_category,  fill=Total_profit)) + geom_col(alpha=1)+
   labs(title="Total profit based on industry")+xlab('Total profit') +
    ylab('Job industry category')
  profit_based_on_industry_plot1+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),
                             text=element_text(size=11),element_line(size=0))+
    theme(axis.line.x = element_line(color="blue", size = 0.5),
          axis.line.y = element_line(color="blue", size = 0.5))
 })
#############################################
  output$profit_based_on_wealth_segment<- renderPlot({
    wealth_segment.profit_plot2=ggplot(wealth_segment.profit, aes(x="",y=Total_profit, fill=wealth_segment)) +
      geom_bar(stat="identity") +
      coord_polar("y", start=0) +labs(title = "Total profit based on wealth segment")
    wealth_segment.profit_plot2+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
    panel.background=element_blank(),text=element_text(size=10),element_line(size=0))
  }) 
############################################ 
  output$profit_based_on_states<- renderPlot({
    state.profit_plot1=ggplot(state.profit, aes(x="", y=Total_profit, fill=state)) +
      geom_bar(width=1,stat="identity") +
      coord_polar("y", start=0) +
      labs(title = "Total profit based on states")
    state.profit_plot1+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
    panel.background=element_blank(),text=element_text(size=10),element_line(size=0))
  })
############################################  
  output$profit_based_on_gender<- renderPlot({
    monthly_data_2_plot1=ggplot() + 
      geom_line(data =monthly_data_2, aes(x=month_of_transaction, y=male_avg,group=0),
                color = "blue") +
      geom_line(data =monthly_data_2, aes(x=month_of_transaction, y=female_avg,group=2), color = "red") +
      labs(title=" Number of bicycles purchased per month ")+
      xlab('Number of bicycles') +
      ylab('Dates')
    monthly_data_2_plot1+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),
                               text=element_text(size=11),element_line(size=0))+
      theme(axis.line.x = element_line(color="blue", size = 0.5),
            axis.line.y = element_line(color="blue", size = 0.5))
  })   
############################################ 
  output$profit_based_on_agegroups<- renderPlot({
    age_group.profit_plot1=ggplot(age_group.profit, aes(x =age_group, y =Total_profit, fill=age_group)) + geom_col(alpha=1)+
      labs(title="Total profit based on age")+xlab('age group') +
      ylab(' Total profit')
    age_group.profit_plot1+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),
                                 text=element_text(size=11),element_line(size=0))+
      theme(axis.line.x = element_line(color="blue", size = 0.5),
            axis.line.y = element_line(color="blue", size = 0.5))
  }) 
  ######################2###################### 
  output$most_purchased_brands<- renderPlot({
    brand.profit_plot1=ggplot(data=datum,aes(y=forcats::fct_infreq(brand),fill=brand))+geom_bar()+labs(title="Most purchased brand")+xlab('Number of bicycles') +
      ylab('Brand')
  brand.profit_plot1+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),
                                                text=element_text(size=11),element_line(size=0))+
      theme(axis.line.x = element_line(color="blue", size = 0.5),
            axis.line.y = element_line(color="blue", size = 0.5))
  }) 
  ################3########################### 
  output$most_purchased_products<- renderPlot({
    product_line.profit_plot1=ggplot(data=datum)+geom_bar(mapping=aes(x=forcats::fct_infreq(product_line),fill=product_line))+labs(title="Most purchased product")+xlab('Product line')+ylab('Number of bicycles')
  product_line.profit_plot1+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),
  text=element_text(size=11),element_line(size=0))+
      theme(axis.line.x = element_line(color="blue", size = 0.5),
            axis.line.y = element_line(color="blue", size = 0.5))
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)





library(shiny)
shinyUI(
  pageWithSidebar(
    headerPanel("Lan_Wei_Social Network Analysis"),
    sidebarPanel(fileInput('file','Choose File:',
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 
                 fileInput('file_labels','Choose File (Labels):',
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 numericInput("number", "Number of connection:", 10),
                 numericInput("neighnumber", "Number of neighbours:", 6)
    
                 ),
    
    
    mainPanel(
      #tableOutput('mytable1'),
      #h4('TOP 10 senders'),
      #tableOutput('sender'),
      #h4('TOP 10 receivers'),
      #tableOutput('receiver')
      tabsetPanel(
        tabPanel('Display File',
                 tableOutput('mytable1')),
        tabPanel('Top 10 Sender',
                 tableOutput("mytable2"), forceNetworkOutput("myplot2")),
        tabPanel('Top 10 Receiver',
                 tableOutput("mytable3"), forceNetworkOutput("myplot3")),
        tabPanel('Degree Centrality',
                 tableOutput("mytable4"), forceNetworkOutput("myplot4")),
        tabPanel('Betweenness Centrality',
                 tableOutput("mytable5"), forceNetworkOutput("myplot5")),
        tabPanel('Indegree Centrality',
                 tableOutput("mytable6"), forceNetworkOutput("myplot6")),
        tabPanel('Department',
                 tableOutput("mytable7"), forceNetworkOutput("myplot7"))
      )
      
      )
  )
)
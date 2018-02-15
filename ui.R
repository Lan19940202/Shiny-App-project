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
                 tableOutput('mytable1'),h4("The up-to-two-hop-neighbor of 10 people sent most of emails has 970 nodes which almost contain the total nodes in the network(which is 1005);"),
                 h4('From the plots, we can find that the nodes of each plots usually contact with people from the same department with them;'),
                 h4('Several nodes like 160, 121, 86 appear in all five top datasets. It looks like they are the information hinge of the institution.'),
                 h4('The plot of betweenness centrality looks more loose then the other. Since they are the connection different nodes groups who connect strongly.')),
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
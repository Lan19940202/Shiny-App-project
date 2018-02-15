library(shiny)
library(networkD3)
library(sqldf)
library(igraph)
library(plyr)
library(splitstackshape) #cSplit
library(data.table) #as.data.table
library(dplyr) 



shinyServer(function(input,output)
{
  
  dataInput <- reactive({
    inFile<-input$file
    if (is.null(inFile))
      return(NULL)
    return(inFile)
  })
  
  labelInput <- reactive({
    infile_labels<-input$file_labels
    if (is.null(infile_labels))
      return(NULL)
    return(infile_labels)
  })
  
  
  
  PlotSender <- reactive({
    inFile<-dataInput()
    infile_labels<-labelInput()
    
    #read table
    email<-read.csv(inFile$datapath, header = FALSE, sep=" ",col.names = c("sender","receiver"))
    department<-read.csv(infile_labels$datapath,header = FALSE, sep = " ",col.names = c("person","department"))
    
    #creating the graph
    graph=graph_from_data_frame(email,directed = TRUE,department)
    
    #4. the number of emails sent by each person
    NoSent<-sqldf("select sender, count(sender) as sent_n from email group by sender order by sent_n desc")
    
    #5. the number of emails received by each person
    NoReceived<-sqldf("select receiver, count(receiver) as received_n from email group by receiver order by received_n desc")
    
    #6. Top_10
    #6.1 visualize Top10_NoSent
    Top10_NoSent<-head(NoSent,10)
    Top10_Sent<-Top10_NoSent$sender
    
    # the 1-hop neighbor
    hop_1_Sent<-sqldf('select Top10_NoSent.sender, email.receiver from Top10_NoSent, email where Top10_NoSent.sender=email.sender')
    hop_1_Sent <- hop_1_Sent %>% group_by(sender)
    hop_1_sample_Sent<-sample_n(hop_1_Sent, input$neighnumber, replace = TRUE)
    hop_1_sample_Sent_receiver <- as.vector(hop_1_sample_Sent$receiver)
    
    Top10_Sent<-Top10_Sent+1
    Top10_Sent_2hop<-make_ego_graph(graph,order=2,nodes = Top10_Sent,mode = 'all')
    Top10_Sent_graph<-igraph::union(Top10_Sent_2hop[[1]],Top10_Sent_2hop[[2]],Top10_Sent_2hop[[3]],
                                    Top10_Sent_2hop[[4]],Top10_Sent_2hop[[5]],Top10_Sent_2hop[[6]],
                                    Top10_Sent_2hop[[7]],Top10_Sent_2hop[[8]],Top10_Sent_2hop[[9]],Top10_Sent_2hop[[10]])
    
    node2_Sent <- as.character(hop_1_sample_Sent_receiver)
    Top10_Sent_graph <- induced.subgraph(Top10_Sent_graph, node2_Sent)
    
    Top10_Sent_d3<-igraph_to_networkD3(Top10_Sent_graph)
    Top10_Sent_node<-data.frame(Top10_Sent_d3$nodes)
    Top10_Sent_department<-sqldf('select department.person, department.department 
                                 from department, Top10_Sent_node 
                                 where department.person=Top10_Sent_node.name')
    Top10_Sent_d3_2<-igraph_to_networkD3(Top10_Sent_graph,Top10_Sent_department)
    
    ###############
   
    ################
    
    Top10_Sent_vi<-forceNetwork(Links = Top10_Sent_d3_2$links, Nodes = Top10_Sent_d3_2$nodes,
                                Source = 'source', Target = 'target', NodeID = 'name',
                                Group = 'department', zoom = TRUE)
    return(Top10_Sent_vi) 
  })
  
  PlotReceiver <- reactive({
    inFile<-dataInput()
    infile_labels<-labelInput()
    
    #read table
    email<-read.csv(inFile$datapath, header = FALSE, sep=" ",col.names = c("sender","receiver"))
    department<-read.csv(infile_labels$datapath,header = FALSE, sep = " ",col.names = c("person","department"))
    
    #creating the graph
    graph=graph_from_data_frame(email,directed = TRUE,department)
    
    #4. the number of emails sent by each person
    NoSent<-sqldf("select sender, count(sender) as sent_n from email group by sender order by sent_n desc")
    
    #5. the number of emails received by each person
    NoReceived<-sqldf("select receiver, count(receiver) as received_n from email group by receiver order by received_n desc")
    
    
    #6.2 visualize Top10_NoReceiver
    Top10_NoReceiver<-head(NoReceived,10)
    Top10_Receiver<-Top10_NoReceiver$receiver
    
    #################
    hop_1_Receiver<-sqldf('select email.sender, Top10_NoReceiver.receiver 
                        from Top10_NoReceiver, email 
                        where Top10_NoReceiver.receiver=email.receiver')
    hop_1_Receiver <- hop_1_Receiver %>% group_by(receiver)
    hop_1_sample_Receiver<-sample_n(hop_1_Receiver, input$neighnumber, replace = TRUE)
    hop_1_sample_receiver <- as.vector(hop_1_sample_Receiver$sender)
    ###################
    
    Top10_Receiver<-Top10_Receiver+1
    Top10_Receiver_2hop<-make_ego_graph(graph,order=2,nodes = Top10_Receiver,mode = 'all')
    
    Top10_Receiver_graph<-igraph::union(Top10_Receiver_2hop[[1]],Top10_Receiver_2hop[[2]],Top10_Receiver_2hop[[3]],
                                        Top10_Receiver_2hop[[4]],Top10_Receiver_2hop[[5]],Top10_Receiver_2hop[[6]],
                                        Top10_Receiver_2hop[[7]],Top10_Receiver_2hop[[8]],Top10_Receiver_2hop[[9]],Top10_Receiver_2hop[[10]])
    ################
    node2_Receiver <- as.character(hop_1_sample_receiver)
    Top10_Receiver_graph <- induced.subgraph(Top10_Receiver_graph, node2_Receiver)
    ################
    
    
    Top10_Receiver_d3<-igraph_to_networkD3(Top10_Receiver_graph)
    Top10_Receiver_node<-data.frame(Top10_Receiver_d3$nodes)
    Top10_Receiver_department<-sqldf('select department.person, department.department 
                                   from department, Top10_Receiver_node 
                                   where department.person=Top10_Receiver_node.name')
    Top10_Receiver_d3_2<-igraph_to_networkD3(Top10_Receiver_graph,Top10_Receiver_department)
    
    Top10_Receiver_vi<-forceNetwork(Links = Top10_Receiver_d3_2$links, Nodes = Top10_Receiver_d3_2$nodes,
                                    Source = 'source', Target = 'target', NodeID = 'name',
                                    Group = 'department',fontSize = 20,
                                    zoom = TRUE)
    return(Top10_Receiver_vi) 
  })
  
  
  PlotDegree <- reactive({
    inFile<-dataInput()
    infile_labels<-labelInput()
    
    #read table
    email<-read.csv(inFile$datapath, header = FALSE, sep=" ",col.names = c("sender","receiver"))
    department<-read.csv(infile_labels$datapath,header = FALSE, sep = " ",col.names = c("person","department"))
    
    #creating the graph
    graph=graph_from_data_frame(email,directed = TRUE,department)
    
    
    # 7. compute the degree centrality of each person
    # 7.1 compute: 
    degree=centr_degree(graph)
    
    #degree2=centr_degree(graph, mode = 'all')
    #mode: Character string, 
    # "out" for out-degree, "in" for in-degree 
    # "total" for the sum of the two. 
    # For undirected graphs this argument is ignored. "all" is a synonym of "total".
    
    name_degree=data.frame(department$person,degree$res)
    colnames(name_degree)<-c('name','degree')
    #nrow(department)=nrow(name_degree)=1005
    range_name_degree<-arrange(name_degree,desc(degree))
    Top10_name_degree<-head(range_name_degree,10)
    
    # 7.2 visualize
    Top10_degree<-Top10_name_degree$name
    
    #################
    hop_1_degree<-sqldf('select email.sender, email.receiver 
                      from Top10_name_degree, email 
                      where Top10_name_degree.name=email.sender')
    Top10_name_degree <- hop_1_degree %>% group_by(sender)
    hop_1_sample_degree<-sample_n(Top10_name_degree, input$neighnumber, replace = TRUE)
    hop_1_sample_degree <- as.vector(hop_1_sample_degree$receiver)
    ###################
    
    Top10_degree<-Top10_degree+1
    Top10_degree_2hop<-make_ego_graph(graph,order=2,nodes = Top10_degree,mode = 'all')
    #Top10_degree_2hop<-make_ego_graph(graph,order=2,nodes = Top10_degree,mode = 'out')
    
    #Top10_degree_2hop<-simplify(Top10_degree_2hop)
    
    Top10_degree_graph<-igraph::union(Top10_degree_2hop[[1]],Top10_degree_2hop[[2]],Top10_degree_2hop[[3]],
                                      Top10_degree_2hop[[4]],Top10_degree_2hop[[5]],Top10_degree_2hop[[6]],
                                      Top10_degree_2hop[[7]],Top10_degree_2hop[[8]],Top10_degree_2hop[[9]],Top10_degree_2hop[[10]])
    
    
    ################
    node2_degree <- as.character(hop_1_sample_degree)
    Top10_degree_graph <- induced.subgraph(Top10_degree_graph, node2_degree)
    ################
    
    Top10_degree_d3<-igraph_to_networkD3(Top10_degree_graph)
    Top10_degree_node<-data.frame(Top10_degree_d3$nodes)
    Top10_degree_department<-sqldf('select department.person, department.department 
                                 from department, Top10_degree_node 
                                 where department.person=Top10_degree_node.name')
    Top10_degree_d3_2<-igraph_to_networkD3(Top10_degree_graph,Top10_degree_department)
    Top10_degree_vi<-forceNetwork(Links = Top10_degree_d3_2$links, Nodes = Top10_degree_d3_2$nodes,
                                  Source = 'source', Target = 'target', NodeID = 'name',
                                  Group = 'department',fontSize = 20,
                                  zoom = TRUE)
    return(Top10_degree_vi) 
  })
  
  
  
  PlotBT <- reactive({
    
    inFile<-dataInput()
    infile_labels<-labelInput()
    
    #read table
    email<-read.csv(inFile$datapath, header = FALSE, sep=" ",col.names = c("sender","receiver"))
    department<-read.csv(infile_labels$datapath,header = FALSE, sep = " ",col.names = c("person","department"))
    
    #creating the graph
    graph=graph_from_data_frame(email,directed = TRUE,department)
    
    #8. compute the betweennee centrality of each person
    # 8.1 compute
    betweenness<-centr_betw(graph,directed = TRUE)
    name_betweenness<-data.frame(department$person,betweenness$res)
    colnames(name_betweenness)<-c('name','betweenness')
    range_name_betweenness<-arrange(name_betweenness,desc(betweenness))
    Top10_name_betweenness<-head(range_name_betweenness,10)
    # 8.2 visualize
    
    #################
    hop_1_betweenness<-sqldf('select email.sender, email.receiver 
                           from Top10_name_betweenness, email 
                           where Top10_name_betweenness.name=email.sender')
    Top10_betweenness <- hop_1_betweenness %>% group_by(sender)
    hop_1_sample_betweenness<-sample_n(Top10_betweenness, input$neighnumber, replace = TRUE)
    hop_1_sample_betweenness <- as.vector(hop_1_sample_betweenness$receiver)
    ###################
    
    Top10_betweenness<-Top10_name_betweenness$name
    Top10_betweenness<-Top10_betweenness+1
    Top10_betweenness_2hop<-make_ego_graph(graph,order=2,nodes = Top10_betweenness,mode = 'all')
    
    Top10_betweenness_graph<-igraph::union(Top10_betweenness_2hop[[1]],Top10_betweenness_2hop[[2]],Top10_betweenness_2hop[[3]],
                                           Top10_betweenness_2hop[[4]],Top10_betweenness_2hop[[5]],Top10_betweenness_2hop[[6]],
                                           Top10_betweenness_2hop[[7]],Top10_betweenness_2hop[[8]],Top10_betweenness_2hop[[9]],Top10_betweenness_2hop[[10]])
    
    ################
    node2_betweenness <- as.character(hop_1_sample_betweenness)
    Top10_betweenness_graph <- induced.subgraph(Top10_betweenness_graph, node2_betweenness)
    ################
    
    
    
    Top10_betweenness_d3<-igraph_to_networkD3(Top10_betweenness_graph)
    Top10_betweenness_node<-data.frame(Top10_betweenness_d3$nodes)
    Top10_betweenness_department<-sqldf('select department.person, department.department from department, Top10_betweenness_node where department.person=Top10_betweenness_node.name')
    Top10_betweenness_d3_2<-igraph_to_networkD3(Top10_betweenness_graph,Top10_betweenness_department)
    
    Top10_betweenness_vi<-forceNetwork(Links = Top10_betweenness_d3_2$links, Nodes = Top10_betweenness_d3_2$nodes,
                                       Source = 'source', Target = 'target', NodeID = 'name',
                                       Group = 'department',fontSize = 20,
                                       zoom = TRUE)
    
    return(Top10_betweenness_vi) 
  })
  
  
  
  PlotIndegree <- reactive({
    inFile<-dataInput()
    infile_labels<-labelInput()
    
    #read table
    email<-read.csv(inFile$datapath, header = FALSE, sep=" ",col.names = c("sender","receiver"))
    department<-read.csv(infile_labels$datapath,header = FALSE, sep = " ",col.names = c("person","department"))
    
    #creating the graph
    graph=graph_from_data_frame(email,directed = TRUE,department)
    
    
    #9. indegree centrality
    # 9.1 calculate
    indegree<-centr_degree(graph, mode = 'in')
    name_indegree<-data.frame(department$person,indegree$res)
    colnames(name_indegree)<-c('name','indegree')
    range_name_indegree<-arrange(name_indegree,desc(indegree))
    Top10_name_indegree<-head(range_name_indegree,10)
    
    # 9.2 visualize
    
    #################
    hop_1_indegree<-sqldf('select email.sender, email.receiver 
                        from Top10_name_indegree, email 
                        where Top10_name_indegree.name=email.sender')
    Top10_indegree <- hop_1_indegree %>% group_by(sender)
    hop_1_sample_indegree<-sample_n(Top10_indegree, input$neighnumber, replace = TRUE)
    hop_1_sample_indegree <- as.vector(hop_1_sample_indegree$receiver)
    ###################
    
    
    Top10_indegree<-Top10_name_indegree$name
    Top10_indegree<-Top10_indegree+1
    Top10_indegree_2hop<-make_ego_graph(graph,order=2,nodes = Top10_indegree,mode = 'all')
    
    Top10_indegree_graph<-igraph::union(Top10_indegree_2hop[[1]],Top10_indegree_2hop[[2]],Top10_indegree_2hop[[3]],
                                        Top10_indegree_2hop[[4]],Top10_indegree_2hop[[5]],Top10_indegree_2hop[[6]],
                                        Top10_indegree_2hop[[7]],Top10_indegree_2hop[[8]],Top10_indegree_2hop[[9]],Top10_indegree_2hop[[10]])
    
    ################
    node2_indegree <- as.character(hop_1_sample_indegree)
    Top10_indegree_graph <- induced.subgraph(Top10_indegree_graph, node2_indegree)
    ################
    
    Top10_indegree_d3<-igraph_to_networkD3(Top10_indegree_graph)
    Top10_indegree_node<-data.frame(Top10_indegree_d3$nodes)
    Top10_indegree_department<-sqldf('select department.person, department.department from department, Top10_indegree_node where department.person=Top10_indegree_node.name')
    Top10_indegree_d3_2<-igraph_to_networkD3(Top10_indegree_graph,Top10_indegree_department)
    
    Top10_indegree_vi<-forceNetwork(Links = Top10_indegree_d3_2$links, Nodes = Top10_indegree_d3_2$nodes,
                                    Source = 'source', Target = 'target', NodeID = 'name',
                                    Group = 'department',fontSize = 20,
                                    zoom = TRUE)
    
    return(Top10_indegree_vi) 
  })
  
  
  PlotDept <- reactive({
    
    inFile<-dataInput()
    infile_labels<-labelInput()
    
    #read table
    email<-read.csv(inFile$datapath, header = FALSE, sep=" ",col.names = c("sender","receiver"))
    department<-read.csv(infile_labels$datapath,header = FALSE, sep = " ",col.names = c("person","department"))
    
    #10. 
    #10.1 aggregate the emails sent per person, to the department level
    sender_department<-sqldf('select email.sender as sender, department.department as sender_department from email, department where email.sender=department.person')
    receiver_department<-sqldf('select email.receiver as receiver, department.department as receiver_department from email, department where email.receiver=department.person')
    email_department<-data.frame(sender_department$sender_department, receiver_department$receiver_department)
    #10.2 generate the number of emails sent and received between each and every department
    graph_email=graph_from_data_frame(email_department,directed = TRUE)
    email_conncetion<-as_ids(E(graph_email))
    
    email_conncetion<-as.data.table(email_conncetion)
    
    email_conncetion_split=as.data.table(cSplit(email_conncetion,'email_conncetion','|'))
    email_conncetion_split$dummy<-1
    email_conncetion_split$key<-paste(email_conncetion_split$email_conncetion_1,email_conncetion_split$email_conncetion_2,sep='_')
    email_conncetion_count=unique(email_conncetion_split[,.(email_conncetion_1,email_conncetion_2,sum(dummy)),by=key])[,2:4]
    email_conncetion_count<-as.data.frame(email_conncetion_count)
    graph_email_simp<-simplify(graph_email, remove.multiple = TRUE, remove.loops = FALSE)
    graph_email_d3<-igraph_to_networkD3(graph_email_simp)
    deaprtment_plot<-forceNetwork(Links = graph_email_d3$links,
                                  Nodes = graph_email_d3$nodes,
                                  Source = 'source',
                                  Target = 'target',
                                  NodeID = 'name', 
                                  Group = 'name',
                                  opacity = 0.85,
                                  zoom = TRUE, 
                                  opacityNoHover = 0.1)
    
    return(deaprtment_plot) 
  })
  
  
  output$mytable1<-renderTable(
    {head(
      {
        inFile = dataInput()
        
        read.csv(inFile$datapath,sep = input$sep,col.names = c("sender","receiver"))
      },
      input$number
      
    )}
  )
  
  output$mytable2 = renderTable({
    # inFile<-input$file
    # if (is.null(inFile))
    #   return(NULL)
    inFile = dataInput()
    
    t<-read.csv(inFile$datapath,sep = input$sep,col.names = c("sender","receiver"))
    
    sender<-sqldf("select sender, count(sender) as No_sent from t group by sender order by No_sent desc")
    
    head(sender,10)
    
  })
  
  output$myplot2 <- renderForceNetwork(PlotSender())
  output$myplot3 <- renderForceNetwork(PlotReceiver())
  output$myplot4 <- renderForceNetwork(PlotDegree())
  output$myplot5 <- renderForceNetwork(PlotBT())
  output$myplot6 <- renderForceNetwork(PlotIndegree())
  output$myplot7 <- renderForceNetwork(PlotDept())
  
  output$mytable3 = renderTable({
    inFile<-input$file
    if (is.null(inFile))
      return(NULL)
    
    t<-read.csv(inFile$datapath,sep = input$sep,col.names = c("sender","receiver"))
    
    receiver<-sqldf("select receiver, count(receiver) as No_receiver from t group by receiver order by No_receiver desc")
    
    head(receiver,10)
  })
  
  #compute the number of emails sent by each person
  output$mytable4 <-renderTable(
    {
      inFile<-dataInput()
      infile_labels<-labelInput()
      
      #read table
      email<-read.csv(inFile$datapath, header = FALSE, sep=" ",col.names = c("sender","receiver"))
      department<-read.csv(infile_labels$datapath,header = FALSE, sep = " ",col.names = c("person","department"))
      
      #creating the graph
      graph=graph_from_data_frame(email,directed = TRUE,department)
      
      
      # 7. compute the degree centrality of each person
      # 7.1 compute: 
      degree=centr_degree(graph)
      
      #degree2=centr_degree(graph, mode = 'all')
      #mode: Character string, 
      # "out" for out-degree, "in" for in-degree 
      # "total" for the sum of the two. 
      # For undirected graphs this argument is ignored. "all" is a synonym of "total".
      
      name_degree=data.frame(department$person,degree$res)
      colnames(name_degree)<-c('name','degree')
      #nrow(department)=nrow(name_degree)=1005
      range_name_degree<-arrange(name_degree,desc(degree))
      head(range_name_degree,10)
    }
  )
  
  #compute the number of emails received by each person
  output$ mytable5 <-renderTable(
    {
      inFile<-dataInput()
      infile_labels<-labelInput()
      
      #read table
      email<-read.csv(inFile$datapath, header = FALSE, sep=" ",col.names = c("sender","receiver"))
      department<-read.csv(infile_labels$datapath,header = FALSE, sep = " ",col.names = c("person","department"))
      
      #creating the graph
      graph=graph_from_data_frame(email,directed = TRUE,department)
      
      #8. compute the betweennee centrality of each person
      # 8.1 compute
      betweenness<-centr_betw(graph,directed = TRUE)
      name_betweenness<-data.frame(department$person,betweenness$res)
      colnames(name_betweenness)<-c('name','betweenness')
      range_name_betweenness<-arrange(name_betweenness,desc(betweenness))
      head(range_name_betweenness,10)
    }
  )
  
  output$mytable6 <-renderTable(
    {
      inFile<-dataInput()
      infile_labels<-labelInput()
      
      #read table
      email<-read.csv(inFile$datapath, header = FALSE, sep=" ",col.names = c("sender","receiver"))
      department<-read.csv(infile_labels$datapath,header = FALSE, sep = " ",col.names = c("person","department"))
      
      #creating the graph
      graph=graph_from_data_frame(email,directed = TRUE,department)
      
      
      #9. indegree centrality
      # 9.1 calculate
      indegree<-centr_degree(graph, mode = 'in')
      name_indegree<-data.frame(department$person,indegree$res)
      colnames(name_indegree)<-c('name','indegree')
      range_name_indegree<-arrange(name_indegree,desc(indegree))
      head(range_name_indegree,10)
    }
  )
  
  output$mytable7 <-renderTable(
    {
      inFile<-dataInput()
      infile_labels<-labelInput()
      
      #read table
      email<-read.csv(inFile$datapath, header = FALSE, sep=" ",col.names = c("sender","receiver"))
      department<-read.csv(infile_labels$datapath,header = FALSE, sep = " ",col.names = c("person","department"))
      
      #10. 
      #10.1 aggregate the emails sent per person, to the department level
      sender_department<-sqldf('select email.sender as sender, department.department as sender_department from email, department where email.sender=department.person')
      receiver_department<-sqldf('select email.receiver as receiver, department.department as receiver_department from email, department where email.receiver=department.person')
      email_department<-data.frame(sender_department$sender_department, receiver_department$receiver_department)
      #10.2 generate the number of emails sent and received between each and every department
      graph_email=graph_from_data_frame(email_department,directed = TRUE)
      email_conncetion<-as_ids(E(graph_email))
      
      email_conncetion<-as.data.table(email_conncetion)
      
      email_conncetion_split=as.data.table(cSplit(email_conncetion,'email_conncetion','|'))
      email_conncetion_split$dummy<-1
      email_conncetion_split$key<-paste(email_conncetion_split$email_conncetion_1,email_conncetion_split$email_conncetion_2,sep='_')
      email_conncetion_count=unique(email_conncetion_split[,.(email_conncetion_1,email_conncetion_2,sum(dummy)),by=key])[,2:4]
      as.data.frame(email_conncetion_count)
    }
  )
  
})

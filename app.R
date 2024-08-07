library(shiny)
library(ggplot2)
library("ggrepel")
library(tidyverse)
library(ggpubr)
library(plotly)
library(processx)
library(DT)
library(shinyBS)
library(data.table)
library(viridis)
library(dplyr)

ui <- fluidPage(
  titlePanel("Crosslink Maps"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a csv file", accept = c("csv"),multiple = F),
      #helpText("upload the data file and sample key together in the specified format (windows:ctrl+mouse click, mac:command+mouse click)"),
      uiOutput("selectfile"),
      br(),
      uiOutput("com"),
      br(),
      uiOutput("Species"),
      br(),
      uiOutput("vx")
    ),
    mainPanel(
      tableOutput("contents"),
      uiOutput("tb")
    )
  )
)

server <- function(input, output) {
  output$fileob <- renderTable({
    file <- input$file
    ext1 <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext1 == c("csv"), "Please upload a csv seperated file"))
    nfiles = nrow(input$file) 
    file
  })
  output$com <- renderUI({
    radioButtons("common","Select the common sites",choices = c("Yes" = "Yes","No" = "No"),selected = "No")
  })
  output$Species <- renderUI({
    if(is.null(input$file)) {
      return() 
    }else{
      #if(input$common == "No"){
      #return() 
      #}else{
      radioButtons("species","Select the species",choices = c("Human" = "Human","Mouse" = "Mouse"),selected = "Human")
      #}
    }
  })
  Min <- reactive({
    if(is.null(input$file))
      eMin <- 0
    else 
    {
      nfiles = nrow(input$file) 
      inFile = list()
      for (i in 1 : nfiles)
      {
        inFile[[1]] <- read.csv(input$file$datapath[1],header = TRUE) 
        df <- inFile[[1]]
      }
      eMin <- min(df$E)
      print(eMin)
    }
  })
  Max <- reactive({
    if(is.null(input$file))
      return()
    else 
    {
      nfiles = nrow(input$file) 
      inFile = list()
      for (i in 1 : nfiles)
      {
        inFile[[1]] <- read.csv(input$file$datapath[1],header = TRUE) 
        df <- inFile[[1]]
      }
      eMax <- max(df$E)
      print(eMax)
    }
  })
  #output$vx <- renderUI({
  #sliderInput("eval", ("E-value"),min = Min(), max = 1,value=1,step = 0.000001)
  #})
  output$vx <- renderUI({
    textInput("eval", "E-value",value = 0.0001)
  })
  
  All <- reactive({
    if(is.null(input$file))
      return()
    else 
    {
      nfiles = nrow(input$file) 
      inFile = list()
      for (i in 1 : nfiles)
      {
        inFile[[1]] <- read.csv(input$file$datapath[1],header = TRUE) 
        df <- inFile[[1]]
        arg1 <- input$file$datapath[1]
      }
      arg2 <- "cross-linked_spectra_out.txt"
      arg3 <- "cross-linked_spectra_mat.txt"
      arg4 <- input$eval
      cmd <- paste("perl", "extract_plink.pl", arg1,arg2,arg3,arg4)
      system(cmd)
      if(input$species == "Mouse"){
        arg5 <- "common_mouse_ksites.txt"
        arg6 <- "common_mouse_qsites.txt"
      }else if(input$species == "Human"){
        arg5 <- "common_human_ksites.txt"
        arg6 <- "common_human_qsites.txt"
      }
      arg7 <- "cross-linked_spectra_comAll_out.txt"
      cmd2 <- paste("perl", "find_common1.pl", arg2,arg5,arg6,arg7)
      system(cmd2)
      df <- read.table(arg7,sep = "\t",header = T,row.names = 1)
      df$pair <- paste(df$Ksites,df$Qsites,sep = "-")
      freqtab <- as.data.frame(table(df$pair))
      colnames(freqtab) <- c("Crosslink_sites","frequency")
      write.table(freqtab,"Frequency_table.txt",sep = "\t",quote = FALSE,row.names = FALSE)
      arg8 <- "Frequency_table.txt"
      arg9 <- "cross-linked_spectra_outAll_freq.txt"
      cmd <- paste("perl", "Map_frequency.pl", arg7,arg8,arg9)
      system(cmd)
      df <- read.table(arg9,sep = "\t",header = T)
      print(df)
    }
  })
  
  ComS_tab <- reactive({
    if(is.null(input$file))
      print("Select the 'common sites' option")
    else 
    {
      nfiles = nrow(input$file) 
      inFile = list()
      for (i in 1 : nfiles)
      {
        inFile[[1]] <- read.csv(input$file$datapath[1],header = TRUE) 
        df <- inFile[[1]]
        arg1 <- input$file$datapath[1]
      }
      arg2 <- "cross-linked_spectra_out.txt"
      arg3 <- "cross-linked_spectra_mat.txt"
      arg4 <- input$eval
      cmd1 <- paste("perl", "extract_plink.pl", arg1,arg2,arg3,arg4)
      system(cmd1)
      #df <- read.table(arg2,sep = "\t",header = T)
      #print(df)
      if(input$common == "Yes"){
        if(input$species == "Mouse"){
          arg5 <- "common_mouse_ksites.txt"
          arg6 <- "common_mouse_qsites.txt"
        }else if(input$species == "Human"){
          arg5 <- "common_human_ksites.txt"
          arg6 <- "common_human_qsites.txt"
        }
        arg7 <- "cross-linked_spectra_com_out.txt"
        cmd2 <- paste("perl", "find_common.pl", arg2,arg5,arg6,arg7)
        system(cmd2)
        df <- read.table(arg7,sep = "\t",header = T)
        df$pair <- paste(df$Ksites,df$Qsites,sep = "-")
        freqtab <- as.data.frame(table(df$pair))
        colnames(freqtab) <- c("Crosslink_sites","frequency")
        write.table(freqtab,"Frequency_table_c.txt",sep = "\t",quote = FALSE,row.names = FALSE)
        arg8 <- "Frequency_table_c.txt"
        arg9 <- "cross-linked_spectra_com_out_freq.txt"
        cmd3 <- paste("perl", "Map_frequency.pl", arg7,arg8,arg9)
        system(cmd3)
        df <- read.table(arg9,sep = "\t",header = T)
        print(df)
        #arg8 <- "cross-linked_spectra_com_mat.txt"
        #cmd3 <- paste("perl", "extract_plink_selected.pl", arg7,arg8)
      }else{
        print("Select the 'common sites' option")
      }
    }
  })
  Freq <- reactive({
    if(is.null(input$file))
      return()
    else 
    {
      nfiles = nrow(input$file) 
      inFile = list()
      for (i in 1 : nfiles)
      {
        inFile[[1]] <- read.csv(input$file$datapath[1],header = TRUE) 
        df <- inFile[[1]]
        arg1 <- input$file$datapath[1]
      }
      arg2 <- "cross-linked_spectra_out.txt"
      arg3 <- "cross-linked_spectra_mat.txt"
      arg4 <- input$eval
      cmd <- paste("perl", "extract_plink.pl", arg1,arg2,arg3,arg4)
      system(cmd)
      df <- read.table(arg2,sep = "\t",header = T)
      df$pair <- paste(df$Ksites,df$Qsites,sep = "-")
      freqtab <- as.data.frame(table(df$pair))
      colnames(freqtab) <- c("Crosslink_sites","frequency")
      print(freqtab)
    }
  })
  output$tab <- DT::renderDataTable({
    DT::datatable(All(),options = list(lengthMenu = c(20, 60, 100), pageLength = 20),selection = "multiple",width = 500)
  })
  output$freqtab <- DT::renderDataTable({
    DT::datatable(Freq(),options = list(lengthMenu = c(20, 60, 100), pageLength = 20),selection = "multiple",width = 400)
  })
  output$Ctab <- DT::renderDataTable({
    DT::datatable(ComS_tab(),options = list(lengthMenu = c(20, 60, 100), pageLength = 20),selection = "multiple",width = 500)
  })
  
  selected <- reactive({
    req(input$tab_rows_selected)
    selRow <- All()[input$tab_rows_selected,]
    #write.table(selRow,"selected_crosslink_sites.txt",sep = "\t",quote = FALSE,row.names = F)
    print(selRow)
  })
  selected_freq <- reactive({
    req(input$freqtab_rows_selected)
    selRow <- Freq()[input$freqtab_rows_selected,]
    #write.table(selRow,"selected_frequency_sites.txt",sep = "\t",quote = FALSE,row.names = F)
    print(selRow)
  })
  selected_com <- reactive({
    req(input$Ctab_rows_selected)
    selRow <- ComS_tab()[input$Ctab_rows_selected,]
    #write.table(selRow,"selected_frequency_sites.txt",sep = "\t",quote = FALSE,row.names = F)
    print(selRow)
  })
  
  output$Stab <- DT::renderDataTable({
    DT::datatable(selected(),options = list(lengthMenu = c(10, 50, 100), pageLength = 10),width = 400)
  })
  output$Sfreq <- DT::renderDataTable({
    DT::datatable(selected_freq(),options = list(lengthMenu = c(10, 50, 100), pageLength = 10),width = 400)
  })
  output$Scom <- DT::renderDataTable({
    DT::datatable(selected_com(),options = list(lengthMenu = c(10, 50, 100), pageLength = 10),width = 400)
  })
  All_map <- reactive({
    if(is.null(input$file))
      return()
    else 
    {
      nfiles = nrow(input$file) 
      inFile = list()
      for (i in 1 : nfiles)
      {
        inFile[[1]] <- read.csv(input$file$datapath[1],header = TRUE) 
        df <- inFile[[1]]
        arg1 <- input$file$datapath[1]
      }
      arg2 <- "cross-linked_spectra_out.txt"
      arg3 <- "cross-linked_spectra_mat.txt"
      arg4 <- input$eval
      cmd <- paste("perl", "extract_plink.pl", arg1, arg2,arg3,arg4)
      system(cmd)
      arg2 <- "cross-linked_spectra_out.txt"
      arg3 <- "cross-linked_spectra_mat.txt"
      arg4 <- input$eval
      cmd <- paste("perl", "extract_plink.pl", arg1,arg2,arg3,arg4)
      system(cmd)
      if(input$species == "Mouse"){
        arg5 <- "common_mouse_ksites.txt"
        arg6 <- "common_mouse_qsites.txt"
      }else if(input$species == "Human"){
        arg5 <- "common_human_ksites.txt"
        arg6 <- "common_human_qsites.txt"
      }
      arg7 <- "cross-linked_spectra_comAll_out.txt"
      cmd2 <- paste("perl", "find_common1.pl", arg2,arg5,arg6,arg7)
      system(cmd2)
      df <- read.table(arg7,sep = "\t",header = T,row.names = 1)
      df$pair <- paste(df$Ksites,df$Qsites,sep = "-")
      freqtab <- as.data.frame(table(df$pair))
      colnames(freqtab) <- c("Crosslink_sites","frequency")
      write.table(freqtab,"Frequency_table.txt",sep = "\t",quote = FALSE,row.names = FALSE)
      arg8 <- "Frequency_table.txt"
      arg9 <- "cross-linked_spectra_outAll_freq.txt"
      cmd <- paste("perl", "Map_frequency.pl", arg7,arg8,arg9)
      system(cmd)
      df <- read.table(arg9,sep = "\t",header = T,row.names = 1)
      map <- df %>%
        ggballoonplot(x = "Ksites",y = "Qsites",fill = "Conf_score",size="Freq",size.range = c(3,6))+
        ggtitle(label = "All Crosslinks- K vs Q sites")+
        theme(axis.text.x = element_text(size = 8,face = "bold"),
              axis.text.y = element_text(size = 8,face = "bold"),
              plot.title.position = "plot",
              plot.title = element_text(face = "bold",hjust = 0.5),
              axis.ticks = element_blank(),panel.spacing.x = unit(2, "lines"),panel.spacing.y = unit(2, "lines")) + xlab("K sites") + ylab("Q sites")+
        scale_fill_viridis(guide="colourbar" , option="A")
      
      print(map)
    }
  })
  output$Map1 <- renderPlot({
    dat <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Making plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    plot(dat$x, dat$y)
    All_map()
  })
  FreqMap <- reactive({
    if(is.null(input$file))
      return()
    else 
    {
      nfiles = nrow(input$file) 
      inFile = list()
      for (i in 1 : nfiles)
      {
        inFile[[1]] <- read.csv(input$file$datapath[1],header = TRUE) 
        df <- inFile[[1]]
        arg1 <- input$file$datapath[1]
      }
      arg2 <- "cross-linked_spectra_out.txt"
      arg3 <- "cross-linked_spectra_mat.txt"
      arg4 <- input$eval
      cmd <- paste("perl", "extract_plink.pl", arg1,arg2,arg3,arg4)
      system(cmd)
      df <- read.table(arg2,sep = "\t",header = T)
      df$pair <- paste(df$Ksites,df$Qsites,sep = "-")
      freqtab <- as.data.frame(table(df$pair))
      colnames(freqtab) <- c("Crosslink_sites","frequency")
      write.table(freqtab,"Frequency_table.txt",sep = "\t",quote = FALSE,row.names = FALSE)
      arg5 <- "Frequency_table.txt"
      arg6 <- "Frequency_mat.txt"
      cmd <- paste("perl", "Make_matrix_K_Q_sites.pl", arg5,arg6)
      system(cmd)
      df <- read.table(arg6,sep = "\t",header = T,row.names = 1)
      map <- df %>%
        ggballoonplot(fill = "value",size.range = c(-0.5,7), rotate.x.text = TRUE,
                      ggtheme = theme_minimal(base_size = 9))+
        ggtitle(label = "Frequency Crosslinks- K vs Q sites") +
        theme(panel.grid.major = element_blank(),
              axis.text.x = element_text(size = 8,face = "bold"),
              axis.text.y = element_text(size = 8,face = "bold"),
              plot.title.position = "plot",
              plot.title = element_text(face = "bold",hjust = 0.5),
              axis.ticks = element_blank()) + xlab("K sites") + ylab("Q sites")+
        geom_tile(color = "black", fill = "transparent")+scale_fill_viridis(guide="colourbar" , option="A")
      print(map)
    }
  })
  
  output$FMap <- renderPlot({
    dat <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Making plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    plot(dat$x, dat$y)
    FreqMap()
  })
  ComS_map <- reactive({
    if(is.null(input$file))
      print("Select the 'common sites' option")
    else 
    {
      nfiles = nrow(input$file) 
      inFile = list()
      for (i in 1 : nfiles)
      {
        inFile[[1]] <- read.csv(input$file$datapath[1],header = TRUE) 
        df <- inFile[[1]]
        arg1 <- input$file$datapath[1]
      }
      arg2 <- "cross-linked_spectra_out.txt"
      arg3 <- "cross-linked_spectra_mat.txt"
      arg4 <- input$eval
      cmd1 <- paste("perl", "extract_plink.pl", arg1,arg2,arg3,arg4)
      system(cmd1)
      #df <- read.table(arg2,sep = "\t",header = T)
      #print(df)
      if(input$common == "Yes"){
        if(input$species == "Mouse"){
          arg5 <- "common_mouse_ksites.txt"
          arg6 <- "common_mouse_qsites.txt"
        }else if(input$species == "Human"){
          arg5 <- "common_human_ksites.txt"
          arg6 <- "common_human_qsites.txt"
        }
        arg7 <- "cross-linked_spectra_com_out.txt"
        cmd2 <- paste("perl", "find_common.pl", arg2,arg5,arg6,arg7)
        system(cmd2)
        arg8 <- "cross-linked_spectra_com_mat.txt"
        cmd3 <- paste("perl", "extract_plink_selected.pl", arg7,arg8)
        system(cmd3)
        df <- read.table(arg7,sep = "\t",header = T)
        df$pair <- paste(df$Ksites,df$Qsites,sep = "-")
        freqtab <- as.data.frame(table(df$pair))
        colnames(freqtab) <- c("Crosslink_sites","frequency")
        write.table(freqtab,"Frequency_table_c.txt",sep = "\t",quote = FALSE,row.names = FALSE)
        arg9 <- "Frequency_table_c.txt"
        arg10 <- "cross-linked_spectra_com_out_freq.txt"
        cmd4 <- paste("perl", "Map_frequency.pl", arg7,arg9,arg10)
        system(cmd4)
        df <- read.table(arg10,sep = "\t",header = T,row.names = 1)
        map <- df %>%
          ggballoonplot(x = "Ksites",y = "Qsites",fill = "Conf_score",size="Freq",size.range = c(3,6))+
          ggtitle(label = "Common Crosslinks- K vs Q sites") +
          theme(axis.text.x = element_text(size = 8,face = "bold"),
                axis.text.y = element_text(size = 8,face = "bold"),
                plot.title.position = "plot",
                plot.title = element_text(face = "bold",hjust = 0.5),
                axis.ticks = element_blank()) + xlab("K sites") + ylab("Q sites")+
          scale_fill_viridis(guide="colourbar" , option="A")
        print(map)
      }
    }
  })
  output$Map4 <- renderPlot({
    dat <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Making plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    plot(dat$x, dat$y)
    ComS_map()
  })
  ComAll_map <- reactive({
    if(is.null(input$file))
      print("Select the 'common sites' option")
    else 
    {
      nfiles = nrow(input$file) 
      inFile = list()
      for (i in 1 : nfiles)
      {
        inFile[[1]] <- read.csv(input$file$datapath[1],header = TRUE) 
        df <- inFile[[1]]
        arg1 <- input$file$datapath[1]
      }
      arg2 <- "cross-linked_spectra_out.txt"
      arg3 <- "cross-linked_spectra_mat.txt"
      arg4 <- input$eval
      cmd <- paste("perl", "extract_plink.pl", arg1, arg2,arg3,arg4)
      system(cmd)
      arg2 <- "cross-linked_spectra_out.txt"
      arg3 <- "cross-linked_spectra_mat.txt"
      arg4 <- input$eval
      cmd <- paste("perl", "extract_plink.pl", arg1,arg2,arg3,arg4)
      system(cmd)
      if(input$species == "Mouse"){
        arg5 <- "common_mouse_ksites.txt"
        arg6 <- "common_mouse_qsites.txt"
      }else if(input$species == "Human"){
        arg5 <- "common_human_ksites.txt"
        arg6 <- "common_human_qsites.txt"
      }
      arg7 <- "cross-linked_spectra_comAll_out.txt"
      cmd2 <- paste("perl", "find_common1.pl", arg2,arg5,arg6,arg7)
      system(cmd2)
      df <- read.table(arg7,sep = "\t",header = T,row.names = 1)
      df$pair <- paste(df$Ksites,df$Qsites,sep = "-")
      freqtab <- as.data.frame(table(df$pair))
      colnames(freqtab) <- c("Crosslink_sites","frequency")
      write.table(freqtab,"Frequency_table.txt",sep = "\t",quote = FALSE,row.names = FALSE)
      arg8 <- "Frequency_table.txt"
      arg9 <- "cross-linked_spectra_outAll_freq.txt"
      cmd3 <- paste("perl", "Map_frequency.pl", arg7,arg8,arg9)
      system(cmd3)
      arg10 <- "cross-linked_spectra_All_common.txt"
      cmd4 <- paste("perl", "Map_All_Common.pl", arg5,arg6,arg9,arg10)
      system(cmd4)
      df1 <- read.table(arg10,sep = "\t",header = T)
      print(df1)
      map <- df1 %>%
        ggballoonplot(x = "Ksites",y = "Qsites",fill = "Conf_score",size="Freq",size.range = c(0,6))+
        ggtitle(label = "All Crosslinks common- K vs Q sites")+
        theme(axis.text.x = element_text(size = 8,face = "bold"),
              axis.text.y = element_text(size = 8,face = "bold"),
              plot.title.position = "plot",
              plot.title = element_text(face = "bold",hjust = 0.5),
              axis.ticks = element_blank(),panel.spacing.x = unit(2, "lines"),panel.spacing.y = unit(2, "lines")) + xlab("K sites") + ylab("Q sites")+
        scale_fill_viridis(guide="colourbar" , option="A")
      print(map)
    }
  })
  output$Map6 <- renderPlot({
    dat <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Making plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    plot(dat$x, dat$y)
    ComAll_map()
  })
  
  Sel_map <- reactive({
    if(is.null(input$file))
      return()
    else 
    {
      req(input$tab_rows_selected)
      selRow <- All()[input$tab_rows_selected,]
      write.table(selRow,"selected_crosslink_sites.txt",sep = "\t",quote = FALSE,row.names = F)
      arg1 <- "selected_crosslink_sites.txt"
      arg2 <- "Scross-linked_spectra_mat.txt"
      cmd <- paste("perl", "extract_plink_selected.pl ", arg1, arg2)
      system(cmd)
      df <- read.table(arg1,sep = "\t",header = T,row.names = 1)
      map <- df %>%
        ggballoonplot(x = "Ksites",y = "Qsites",fill = "Conf_score",size="Freq",size.range = c(3,6))+
        ggtitle(label = "Selected Crosslinks- K vs Q sites") +
        theme(axis.text.x = element_text(size = 8,face = "bold"),
              axis.text.y = element_text(size = 8,face = "bold"),
              plot.title.position = "plot",
              plot.title = element_text(face = "bold",hjust = 0.5),
              axis.ticks = element_blank()) + xlab("K sites") + ylab("Q sites")+
        scale_fill_viridis(guide="colourbar" , option="A")
      print(map)
    }
  })
  output$Map2 <- renderPlot({
    dat <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Making plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    plot(dat$x, dat$y)
    Sel_map()
  })
  
  Sel_freq <- reactive({
    if(is.null(input$file))
      return()
    else 
    {
      req(input$freqtab_rows_selected)
      selRow <- Freq()[input$freqtab_rows_selected,]
      write.table(selRow,"selected_frequency_sites.txt",sep = "\t",quote = FALSE,row.names = F)
      arg1 <- "selected_frequency_sites.txt"
      arg2 <- "selected_frequency_mat.txt"
      cmd <- paste("perl", "Make_matrix_K_Q_sites.pl", arg1,arg2)
      system(cmd)
      df <- read.table(arg2,sep = "\t",header = T,row.names = 1)
      map <- df %>%
        ggballoonplot(fill = "value",size.range = c(-0.5,12), rotate.x.text = TRUE,
                      ggtheme = theme_minimal(base_size = 9))+
        ggtitle(label = "Selected Frequency Crosslinks- K vs Q sites") +
        theme(panel.grid.major = element_blank(),
              axis.text.x = element_text(size = 8,face = "bold"),
              axis.text.y = element_text(size = 8,face = "bold"),
              plot.title.position = "plot",
              plot.title = element_text(face = "bold",hjust = 0.5),
              axis.ticks = element_blank()) + xlab("K sites") + ylab("Q sites")+
        geom_tile(color = "black", fill = "transparent")+scale_fill_viridis(guide="colourbar" , option="A")
      print(map)
    }
  })
  
  output$Map3 <- renderPlot({
    dat <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Making plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    plot(dat$x, dat$y)
    Sel_freq()
  })
  
  Sel_Com <- reactive({
    if(is.null(input$file))
      return()
    else 
    {
      req(input$Ctab_rows_selected)
      selRow <- ComS_tab()[input$Ctab_rows_selected,]
      write.table(selRow,"selected_common_sites.txt",sep = "\t",quote = FALSE,row.names = F)
      arg1 <- "selected_common_sites.txt"
      arg2 <- "selected_common_mat.txt"
      cmd <- paste("perl", "extract_plink_selected.pl", arg1,arg2)
      system(cmd)
      df <- read.table(arg1,sep = "\t",header = T,row.names = 1)
      map <- df %>%
        ggballoonplot(x = "Ksites",y = "Qsites",fill = "Conf_score",size="Freq",size.range = c(3,6))+
        ggtitle(label = "Selected common Crosslinks- K vs Q sites") +
        theme(axis.text.x = element_text(size = 8,face = "bold"),
              axis.text.y = element_text(size = 8,face = "bold"),
              plot.title.position = "plot",
              plot.title = element_text(face = "bold",hjust = 0.5),
              axis.ticks = element_blank()) + xlab("K sites") + ylab("Q sites")+
        scale_fill_viridis(guide="colourbar" , option="A")
      print(map)
    }
  })
  output$Map5 <- renderPlot({
    dat <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Making plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    plot(dat$x, dat$y)
    Sel_Com()
  })
  output$download1 <- downloadHandler(
    
    filename = function() {
      paste("All_Map", Sys.Date(),".png",sep = "")
    },
    
    content = function(file) {
      # Write to a file specified by the 'file' argument
      png(file=file,height = 5000,width = 8500,res = 1000)
      print(All_map())
      dev.off()
    }
  )
  output$download2 <- downloadHandler(
    
    filename = function() {
      paste("Frequency_Map", Sys.Date(),".png",sep = "")
    },
    
    content = function(file) {
      # Write to a file specified by the 'file' argument
      png(file=file,height = 5000,width = 8500,res = 1000)
      print(FreqMap())
      dev.off()
    }
  )
  output$download3 <- downloadHandler(
    
    filename = function() {
      paste("Selected_Map", Sys.Date(),".png",sep = "")
    },
    
    content = function(file) {
      # Write to a file specified by the 'file' argument
      png(file=file,height = 5000,width = 8500,res = 1000)
      print(Sel_map())
      dev.off()
    }
  )
  output$download4 <- downloadHandler(
    
    filename = function() {
      paste("Selected_FreqMap", Sys.Date(),".png",sep = "")
    },
    
    content = function(file) {
      # Write to a file specified by the 'file' argument
      png(file=file,height = 5000,width = 8500,res = 1000)
      print(Sel_freq())
      dev.off()
    }
  )
  output$download5 <- downloadHandler(
    
    filename = function() {
      paste("CommonSites", Sys.Date(),".png",sep = "")
    },
    
    content = function(file) {
      # Write to a file specified by the 'file' argument
      png(file=file,height = 5000,width = 8500,res = 1000)
      print(ComS_map())
      dev.off()
    }
  )
  output$download6 <- downloadHandler(
    
    filename = function() {
      paste("Selected_CommonSites", Sys.Date(),".png",sep = "")
    },
    
    content = function(file) {
      # Write to a file specified by the 'file' argument
      png(file=file,height = 4000,width = 7500,res = 1000)
      print(Sel_Com())
      dev.off()
    }
  )
  output$download7 <- downloadHandler(
    
    filename = function() {
      paste("All_CommonSites", Sys.Date(),".png",sep = "")
    },
    
    content = function(file) {
      # Write to a file specified by the 'file' argument
      png(file=file,height = 4000,width = 7500,res = 1000)
      print(ComAll_map())
      dev.off()
    }
  )
  output$tb <- renderUI({
    if(is.null(input$file)) {
      h3(tags$p("This app takes cross-linking mass spectrometry data and generates cross-link maps"),
         tags$img(src="Crosslinker.jpg",width = "500px", height = "500px"))
    }
    else
      tabsetPanel(
        tabPanel("File Uploaded", tableOutput("fileob")),
        #tabPanel("Frequency", bsCollapse(open="Crosslink sites",multiple = TRUE,bsCollapsePanel("Crosslink sites",DT::dataTableOutput("freqtab"),style = "success"),bsCollapsePanel("Frequency Map",downloadButton("download2", "Download Map" ),plotOutput("FMap"),style = "success"))),
        #tabPanel("Selected Frequency", bsCollapse(open="Selected Crosslink sites",multiple = TRUE,bsCollapsePanel("Selected Crosslink sites",DT::dataTableOutput("Sfreq"),style = "success"),bsCollapsePanel("Selected Frequency Map",downloadButton("download4", "Download Map" ),plotOutput("Map3"),style = "success"))),
        tabPanel("All", bsCollapse(open="All Data",multiple = TRUE,bsCollapsePanel("All Data",DT::dataTableOutput("tab"),style = "success"),bsCollapsePanel("All map",downloadButton("download1", "Download Map" ),plotOutput("Map1"),style = "success"),bsCollapsePanel("All Common sites map",downloadButton("download7", "Download Map" ),plotOutput("Map6"),style = "success"),bsCollapsePanel("Common sites",DT::dataTableOutput("Ctab"),style = "success"),bsCollapsePanel("Common site map",downloadButton("download5", "Download Map" ),plotOutput("Map4"),style = "success"))),
        tabPanel(("Selected"),bsCollapse(open="Selected_Data",multiple = TRUE,bsCollapsePanel("Selected Data",DT::dataTableOutput("Stab"),style = "success"),bsCollapsePanel("Selected map",downloadButton("download3", "Download Map" ),plotOutput("Map2"),style = "success"),bsCollapsePanel("Selected Common sites",DT::dataTableOutput("Scom"),style = "success"),bsCollapsePanel("Selected Common site map",downloadButton("download6", "Download Map" ),plotOutput("Map5"),style = "success"))),
        #tabPanel("Output data",downloadButton("download1", "Download Data" ), DT::dataTableOutput("newdata1")),
        #tabPanel("Differential Analysis",downloadButton("download2", "Download Plot" ),plotOutput("Plot1",height = "700px",width = "1200px"),downloadButton("download1", "Download Data" ), DT::dataTableOutput("newdata1")),
        #tabPanel("Volcano Plot",bsCollapse(open="Volcano 1",multiple = TRUE,bsCollapsePanel("Volcano 1",downloadButton("download2", "Download Plot" ),plotOutput("Plot1",height = "500px",width = "900px"),style = "success"),bsCollapsePanel("Volcano 2",downloadButton("download4", "Download Plot" ),plotOutput("Plot5",height = "500px",width = "900px"),style = "success"))),
        #tabPanel("Box Plot",downloadButton("download3", "Download Plot"),plotOutput("Plot2"))
        #,column(11,offset = 11,(tableOutput("PieData1"))),column(11,offset = 11,(tableOutput("PieData2")))
      )
  })
  
}
shinyApp(ui, server)
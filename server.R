library(DT)
library(revtools)
library(magrittr)
library(stringr)
library(dplyr)
library(tidyr)
library(shinyjs)
options(stringsAsFactors = FALSE)

# code helper module ------------------------------------------------------

mod_code_helper <- function(input){
  
  # note: SEP has to be \t
  data <- read.table(as.character(input),stringsAsFactors=FALSE,sep = "\t",header = TRUE,encoding = "UTF-16")
  #  coding data
  datalist <- split(data,data[,1])
  
  # initiate data frame
  table_asymmetric <- data.frame()
  
  table_symmetric <- data.frame()
  # loop through each data file
  for (k in 1:length(datalist)){
    data_need_to_replace <- datalist[[k]]
    # initiate warning
    warn=0
    # initiate vectors
    titles <- c()
    compares <- c()
    data_r <- c()
    # loop through each variables
    for (i in 1:(length(data_need_to_replace[,2])-1)){
      titles <- c(titles,rep(data_need_to_replace[i,3],length(data_need_to_replace[,3])-i))
      compares <- c(compares,na.omit(data_need_to_replace[i+1:length(data_need_to_replace),3]) %>%  as.character())
      if (data_need_to_replace[i,i+5]!=""&is.na(data_need_to_replace[i,i+5])==FALSE){
        warning("Your correlation matrix is not symmetric,please check")
        warn=1
      }else{
        data_r <- c(data_r,data_need_to_replace[,i+5] %>% str_extract_all(.,".+") %>% unlist() %>% na.omit())
      }
    }
    study_id <- rep(unique(data_need_to_replace[,1]),length(titles))
    if (warn==1){
      table_asymmetric <- rbind(table_asymmetric,data.frame(study_id=study_id,var1=titles,var2=compares))
    }else{
      table_symmetric <- rbind(table_symmetric,data.frame(study_id=study_id,var1=titles,var2=compares,data_r=data_r))
    }
  }
  
  df <- list(table_asymmetric,table_symmetric)
  
  return(df)
}

server <- function(input, output,session) {
  status <- reactiveVal(1)
  edit <- 0
  table1 <- eventReactive(input$Load,{
    req(input$files)
    if (input$sidebar=="apa"&&input$files$type!="text/plain"){
      df <- read_bibliography(input$files$datapath) %>% select(.,title,author,year,doi) %>%   
        separate(.,doi,into = c("dois","others"),sep = "Early") %>% select(.,title,author,year,dois) %>% 
        mutate(.,dois = ifelse(is.na(dois)!=TRUE,paste0("https://doi.org/",dois),NA))
      
    }else if(input$sidebar=="Coding"&&input$files$type=="text/plain"){
      
      df <- mod_code_helper(input$files$datapath)
    }else{
      shinyalert("Error!", "You input a wrong data type, please upload a correct data file", type = "error")
      df <- ""
    }
    return(df)
  })

  onevent(event = "click",id="press",{
     if (status()==2){
       shinyalert("Error!", "You have to reset the table before using the other function", type = "error")
       }
  })
  # 

  
  

  observeEvent(input$reset,{
    status(1)
    removeUI("#table")
  })
  
  observeEvent(input$Load, {
    insertUI("#placeholder", "afterEnd", ui = DT::dataTableOutput('table'))
    status(2)
    if(length(table1()[[1]])>1&input$sidebar=="Coding"){
      shinyalert("Warning!", "Your correlation table is perhaps not symmetric, please check", type = "warning")
    }
  })
  
  output[["table"]]<- DT::renderDataTable({
    if (input$sidebar=="apa"){
      df <- table1()
    }else if(input$sidebar=="Coding"){
      if (input$symmetric=="sym"){
        df <- table1()[[2]]
      }else{
        df <- table1()[[1]]
      }
      return(df)
    }
  },editable = 'cell', selection = 'none',server = FALSE)
  

# prepare for download data -----------------------------------------------
  
  df1 <- reactiveVal()
  
  
  proxy = dataTableProxy('table')

  observeEvent(input[["table_cell_edit"]], {
    edit <<- 1
    cell <- input[["table_cell_edit"]]
 if(input$sidebar=="Coding"){
      if (input$symmetric=="sym"){
        newdf <- table1()[[2]]
      }else{
        newdf <- table1()[[1]]
      }
   newdf[cell$row, cell$col] <- cell$value
   replaceData(proxy,newdf,resetPaging = FALSE)
   df1(newdf)
 }else{
   shinyalert("Warning!", "Don't edit the content for DOI extractor", type = "warning")
 }
  })


  
  output$downloadData <- downloadHandler(
    filename = function() {
      if(input$sidebar=="apa"){
        if(input$download_w=="doi"){
          paste("doi", ".txt", sep = "")
        }else{
          paste("title", ".txt", sep = "")  
        }
      }else if(input$sidebar=="Coding"){
        if(input$symmetric=="sym"){
          paste("symmetric", ".csv", sep = "")
        }else{
          paste("asymmetric", ".csv", sep = "")
        }
      }
      
    },
    content = function(file) {
      if(input$sidebar=="apa"){
        if(input$download_w=="doi"){
          write.table(table1()[,4], file, row.names = FALSE,quote = FALSE,col.names = FALSE,eol="\r\n")
        }else(
          write.table(table1()[,1], file, row.names = FALSE,quote = FALSE,col.names = FALSE,eol="\r\n")  
        )
      }else if(input$sidebar=="Coding"&edit==1){
        if(input$symmetric=="sym"){
          if(input$delete_star=="Y"){
            output <- df1() %>% separate(.,data_r,into = c("r","stars"),sep = "\\*",remove = FALSE) %>% select(.,study_id,var1,var2,r,data_r)
          }else if(input$delete_star=="N"){
            output <- df1()
          }
          write.table(output,file,sep=input$output_f,quote = FALSE,row.names = FALSE)
        }else if(input$symmetric=="asy"){
          write.table(df1(),file,sep=input$output_f,quote = FALSE,row.names = FALSE)  
        }
      }else if(input$sidebar=="Coding"&edit==0)  {
        if(input$symmetric=="sym"){
          if(input$delete_star=="Y"){
            output <- table1()[[2]] %>% separate(.,data_r,into = c("r","stars"),sep = "\\*",remove = FALSE) %>% select(.,study_id,var1,var2,r,data_r)
          }else if(input$delete_star=="N"){
            output <- table1()[[2]]
          }
          write.table(output,file,sep=input$output_f,quote = FALSE,row.names = FALSE)
        }else if(input$symmetric=="asy"){
          write.table(table1()[[1]],file,sep=input$output_f,quote = FALSE,row.names = FALSE)  
        }
      }
      
    }
  )
  
}

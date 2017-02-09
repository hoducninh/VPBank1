 #' @title Shiny App for General Frequent Patterns
#' @description Showing the Frequent Patterns with Shiny App. 
#' @author Anh Hoang Duc (anhhd3@vpbank.com.vn)
#' @param Data set (transaction data)
#' @keywords Shiny, Association Rules
#' @return Shiny App
#' @import shiny arulesViz arules DT igraph dplyr foreach doParallel stringr
#' @export
#' @examples
#' data("Groceries")
#' customer.behavior(Groceries)
customer.behavior <- function(Transaction){
  force(Transaction)
  require(shiny)
  require(visNetwork)
  require(DT)
  require(dplyr)
shinyApp(
  ui <- fluidPage(
    #Tên ứng dụng
    #Tên ứng dụng
    h1("Basket Analysis for VPBank's Customers ", align = "center"),
    h4("Business Intelligence Competency Center - VPBank", align = "center"),
    h5("Developed by Anh Hoang Duc - BICC", align = "center"),
    h5("Email: anhhd3@vpbank.com.vn", align = "center"),
    sidebarLayout(
      sidebarPanel(
        h4("Parameters Selection"),
        numericInput("sup",
                     "Support",
                     value = 0.008,
                     min = 0.00001,
                     max = 0.01, 
                     step = 0.00001),
        sliderInput("conf",
                    "Confidence",
                    min = 0,
                    max = 1,
                    value = 0.5),
      actionButton("explore", "Explore", icon("paper-plane"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), 
      br(),
      br(),
      actionButton("export", "Export result to HTML", icon("download"),
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
      mainPanel(tabsetPanel(
        tabPanel("Overview", 
                 plotOutput("item.frequency"),
                 verbatimTextOutput("item.summary")),
        tabPanel("Plot",visNetworkOutput("network")),
        tabPanel("Rules in detail", DT::dataTableOutput("table.rules")),
                tabPanel("Summary Rules", verbatimTextOutput("summary.rules")),
        tabPanel("Recommendations", 
                 numericInput("id",
                              "Please provide customer's ID",
                              value = 1),
                 DT::dataTableOutput("recom.item"),
                 actionButton("download", "Download recommendations for all customers", icon("download"),
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
      ))),
  server <- shinyServer(function(input, output){
    a <- eventReactive(input$explore,{input$sup})
    b <- eventReactive(input$explore,{input$conf})
    #c <- eventReactive(input$explore,{input$max.leng})
    dir <- getwd()
    observeEvent(input$export,{
      #Function
      result <- function(Transaction, sup, conf){
        rmarkdown::render(system.file("rmd/flex.customer.behavior.Rmd", package="VPBank"), 
                          output_dir = dir,
                          output_file = paste(Sys.Date(),"_Dashboard.html"))
      }
      result(Transaction, sup = a(), conf = b())
    })
    
    observeEvent(input$download,{
      #Function
      get_recom <- function(Transaction, rules){
        library(foreach);
        library(doParallel);
        registerDoParallel(cores = (detectCores() - 1))
        no <- Transaction@data@Dim[2];
        #Function to get ID & recommended item as data frame
        recom_item <- function(rules, data, iter) {
          #rec_iter is to get rules matching 
          rec_iter <- function(rules, data, iter) {
            basket <- data[iter]
            rulesMatchLHS <- is.subset(rules@lhs, basket)
            suitableRules <-  rulesMatchLHS & !(is.subset(rules@rhs, basket))
            rules <- sort(rules[suitableRules], decreasing = TRUE, by = "lift")
            as(rules, "data.frame")
          }
          rec_iter(rules, data, iter) -> rules.df
          rec_items <- str_split_fixed(rules.df$rules, " => ", n = 2) %>%
            as.data.frame %>% distinct(V2) 
          rec_items <- paste0(rec_items$V2, sep = "")
          ID <- data[iter]@itemsetInfo$transactionID
          rec_items_df <- data.frame(ID=ID, Recommendation = rec_items)
          return(rec_items_df)
        }
        foreach(iter = 1: no,
                .combine = rbind, 
                .packages = c("arules", "dplyr", "stringr")) %dopar% 
          recom_item(rules, Transaction, iter) -> df
        my_result <- df %>% filter(Recommendation != "")
        return(my_result)
      }
      rules <-  apriori(Transaction, 
                        parameter = list(supp = a(),conf=b(), maxlen = 10, minlen = 2))
      subset.matrix <- is.subset(rules, rules)
      subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
      redundant <- colSums(subset.matrix, na.rm=T) >= 1
      rules.pruned <- rules[!redundant]
      subrules2 <- sort(rules.pruned, by="lift")
      my_result <- get_recom(Transaction, subrules2)
      write.table(my_result, file = paste(Sys.Date(),"_recommendations.txt"),
                  sep = ",", row.names = FALSE)
    })
    
    output$item.frequency <- renderPlot(
      itemFrequencyPlot(Transaction, topN = 40, type = "absolute")
    )
    
    output$item.summary <- renderPrint(
      Transaction %>% summary
    )
    
    output$network <- renderVisNetwork(
      {
        library(arules)
        library(arulesViz)
        library(dplyr)
        library(visNetwork)
        library(igraph)
        library(dplyr)
        library(DT)
        rules <-  apriori(Transaction, 
                          parameter = list(supp = a(),conf=b(), maxlen = 10, minlen = 2))
        subset.matrix <- is.subset(rules, rules)
        subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
        redundant <- colSums(subset.matrix, na.rm=T) >= 1
        rules.pruned <- rules[!redundant]
        subrules2 <- sort(rules.pruned, by="lift")
        ig <- plot( subrules2, method="graph", control=list(type="items"))
        ig_df <- get.data.frame(ig, what = "both")
        ##################
        no.of.rules <- dim(subrules2@quality)[1]
        no.of.obs <- length(ig_df$vertices$confidence)
        edges = ig_df$edges
        nodes = data.frame(
          id = ig_df$vertices$name
          ,label = ifelse(ig_df$vertices$label == "","", ig_df$vertices$label))
        value.confidence <- c(NA,ig_df$vertices$confidence[-no.of.obs])
        value.support <- c(NA,ig_df$vertices$support[-no.of.obs])
        value.lift <- c(NA,ig_df$vertices$lift[-no.of.obs])
        nodes$color[(no.of.obs - no.of.rules +1):no.of.obs] <- "red"
        nodes$value <- value.confidence
        nodes$title <- paste0(
          "Support:", round(value.support * 100,3), "%", "\n",                        
          "Confidence:", round(value.confidence * 100, 3),"%","\n",                   
          "Lift:", round(value.lift, 3))
        
        visNetwork(nodes, edges, height = "100%", width = "100%",
                   main = "Frequent patterns in general") %>% 
          visEdges(arrows = "to") %>% 
          visOptions(highlightNearest = TRUE, 
                     nodesIdSelection = TRUE) %>% 
          visExport()
      })
    
    output$table.rules <- DT::renderDataTable({
      rules <-  apriori(Transaction, 
                        parameter = list(supp = a(),conf=b(), maxlen = 10, minlen = 2))
      subset.matrix <- is.subset(rules, rules)
      subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
      redundant <- colSums(subset.matrix, na.rm=T) >= 1
      rules.pruned <- rules[!redundant]
      subrules2 <- sort(rules.pruned, by="lift")
      ruledf <- as(subrules2, "data.frame")
      datatable(ruledf, rownames = F,
                caption = "Association Rules in Detail",
                extensions = "AutoFill", options = list(autoFill = TRUE)) %>% 
        formatPercentage(c(2,3),3) %>% 
        formatRound(c(4),3)})
    output$recom.item <- DT::renderDataTable({
      rules <-  apriori(Transaction, 
                        parameter = list(supp = a(),conf=b(), maxlen = 10, minlen = 2))
      subset.matrix <- is.subset(rules, rules)
      subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
      redundant <- colSums(subset.matrix, na.rm=T) >= 1
      rules.pruned <- rules[!redundant]
      subrules2 <- sort(rules.pruned, by="lift")
      ###
      rec <- function(rules, data, ID=data[1]@itemsetInfo$transactionID %>% as.numeric) {
        basket <- data[data@itemsetInfo$transactionID==ID]
        rulesMatchLHS <- is.subset(rules@lhs, basket)
        suitableRules <-  rulesMatchLHS & !(is.subset(rules@rhs, basket))
        rules <- sort(rules[suitableRules], decreasing = TRUE, by = "lift")
        as(rules, "data.frame") -> df;
        df1 <- data.frame(item = "No item to recommend")
        df2 <- if(dim(df)[[1]]==0) df1 else df 
        return(df2)
      }
      datatable(rec(subrules2, Transaction, input$id),
                caption = "Recommended items")
    })
    
    output$summary.rules <- renderPrint({
      rules <- apriori(Transaction, 
                        parameter = list(supp = a(),conf=b(), maxlen = 10, minlen = 2))
      subset.matrix <- is.subset(rules, rules)
      subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
      redundant <- colSums(subset.matrix, na.rm=T) >= 1
      rules.pruned <- rules[!redundant]
      subrules2 <- sort(rules.pruned, by="lift")
      subrules2 %>% summary %>% print
    })
  })
)
}
library(shiny)
library(DT)
library(shinythemes)

source("modules/module1_view.R")
source("modules/module2_access.R")
source("modules/module3_add.R")
source("modules/module4_modify.R")
source("modules/module5_count.R")

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(h2("📚 BookNest - Personal Book Collection", style="font-weight:bold")),
  
  tabsetPanel(
    tabPanel("📖 View Collection",
             br(),
             DTOutput("view_table"),
             br(),
             actionButton("view_btn", "Refresh", icon=icon("sync"), class="btn-primary")
    ),
    
    tabPanel("🔍 Access Book",
             br(),
             fluidRow(
               column(4, numericInput("access_num", "Enter Book Number:", value=1, min=1)),
               column(2, br(), actionButton("access_btn", "Get Details", class="btn-info"))
             ),
             br(),
             uiOutput("access_card")
    ),
    
    tabPanel("➕ Add Book",
             br(),
             fluidRow(
               column(6,
                      textInput("add_title", "Title:"),
                      textInput("add_author", "Author:"),
                      textInput("add_genre", "Genre:"),
                      numericInput("add_rating", "Rating (0-5):", value=4.5, min=0, max=5, step=0.1),
                      actionButton("add_btn", "Add Book", icon=icon("plus"), class="btn-success")
               ),
               column(6,
                      br(), br(),
                      uiOutput("add_msg")
               )
             )
    ),
    
    tabPanel("✏️ Modify Book",
             br(),
             fluidRow(
               column(4, numericInput("mod_num", "Book Number:", value=1, min=1)),
               column(4, selectInput("mod_field", "Field to Modify:", choices=c("Title"=1,"Author"=2,"Genre"=3,"Rating"=4))),
               column(4, textInput("mod_value", "New Value:"))
             ),
             actionButton("mod_btn", "Update Book", icon=icon("edit"), class="btn-warning"),
             br(), br(),
             uiOutput("mod_msg")
    ),
    
    tabPanel("🔢 Count",
             br(),
             fluidRow(
               column(6, wellPanel(h4("Total Books"), h1(textOutput("count_total"), style="color:#2C3E50; font-weight:bold"))),
               column(6, wellPanel(h4("Details Per Book"), h1(textOutput("count_details"), style="color:#18BC9C; font-weight:bold")))
             ),
             br(),
             actionButton("save_btn", "💾 Save to CSV (Permanent)", class="btn-primary"),
             verbatimTextOutput("save_msg")
    )
  )
)

server <- function(input, output) {
  books <- reactiveVal(load_books("data/books.csv"))
  
  # Helper to convert list to dataframe for table
  books_df <- reactive({
    lst <- books()
    if(length(lst)==0) return(data.frame())
    data.frame(
      No = 1:length(lst),
      Book_ID = sapply(lst, function(x) x$Book_ID),
      Title = sapply(lst, function(x) x$Title),
      Author = sapply(lst, function(x) x$Author),
      Genre = sapply(lst, function(x) x$Genre),
      Rating = sapply(lst, function(x) x$Rating)
    )
  })
  
  output$view_table <- renderDT({
    input$view_btn
    datatable(books_df(), options=list(pageLength=10), rownames=FALSE, filter='top')
  })
  
  observeEvent(input$access_btn, {
    num <- input$access_num
    lst <- books()
    if(is.na(num) || num <1 || num > length(lst)){
      output$access_card <- renderUI({ div(style="color:red; font-weight:bold;", "❌ Invalid book number!") })
    } else {
      b <- lst[[num]]
      output$access_card <- renderUI({
        wellPanel(
          h4(paste0("Book #", num, " - ", b$Book_ID)),
          p(strong("Title: "), b$Title),
          p(strong("Author: "), b$Author),
          p(strong("Genre: "), b$Genre),
          p(strong("Rating: "), paste0(b$Rating, " ⭐"))
        )
      })
    }
  })
  
  observeEvent(input$add_btn, {
    title <- trimws(input$add_title)
    author <- trimws(input$add_author)
    genre <- trimws(input$add_genre)
    rating <- input$add_rating
    if(title=="" || author=="" || genre==""){
      output$add_msg <- renderUI({ div(style="color:red;", "❌ Value cannot be empty!") })
      return()
    }
    if(is.na(rating) || rating <0 || rating >5){
      output$add_msg <- renderUI({ div(style="color:red;", "❌ Invalid rating!") })
      return()
    }
    current <- books()
    new_id <- paste0("B", sprintf("%03d", length(current)+1))
    current[[length(current)+1]] <- list(Book_ID=new_id, Title=title, Author=author, Genre=genre, Rating=rating)
    books(current)
    output$add_msg <- renderUI({ div(style="color:green; font-weight:bold;", paste0("✅ Added as ", new_id, "! Total: ", length(books()))) })
  })
  
  observeEvent(input$mod_btn, {
    num <- input$mod_num; field <- input$mod_field; value <- trimws(input$mod_value)
    current <- books()
    if(is.na(num) || num <1 || num > length(current)){
      output$mod_msg <- renderUI({ div(style="color:red;", "❌ Invalid book number!") }); return()
    }
    if(field %in% c("1","2","3") && value==""){
      output$mod_msg <- renderUI({ div(style="color:red;", "❌ Value cannot be empty!") }); return()
    }
    if(field=="1") current[[num]]$Title <- value
    if(field=="2") current[[num]]$Author <- value
    if(field=="3") current[[num]]$Genre <- value
    if(field=="4"){
      r <- suppressWarnings(as.numeric(value))
      if(is.na(r) || r<0 || r>5){ output$mod_msg <- renderUI({ div(style="color:red;", "❌ Invalid rating! 0-5 only") }); return() }
      current[[num]]$Rating <- r
    }
    books(current)
    output$mod_msg <- renderUI({ div(style="color:green; font-weight:bold;", "✅ Updated!"); renderPrint({ current[[num]] }) })
  })
  
  output$count_total <- renderText({ length(books()) })
  output$count_details <- renderText({ if(length(books())>0) length(books()[[1]]) else 0 })
  
  observeEvent(input$save_btn, {
    df <- books_df()
    if(nrow(df)>0){
      # Save without No column
      write.csv(df[, -1], "data/books.csv", row.names=FALSE)
      output$save_msg <- renderPrint({ cat("✅ Saved to data/books.csv permanently!") })
    }
  })
}

shinyApp(ui, server)
# ==========================================================
# BookNest - Personal Book Collection & Reading Management System
# Experiment 4: Lists in R
# Run this file to start the application.
# ==========================================================

# Step 1: Load all the modules (each file defines one or more functions)
source("modules/helpers.R")
source("modules/module1_view.R")
source("modules/module2_access.R")
source("modules/module3_add.R")
source("modules/module4_modify.R")
source("modules/module5_count.R")
# Step 2: Make sure the data file exists before we start
if (!file.exists("data/books.csv")) {
  stop("books.csv not found. Open the BookNest project and check the data folder.")
}

# Step 3: Read the CSV once and store the books as a list
book_list <- load_books("data/books.csv")

cat("\nWelcome to BookNest!\n")

# Step 4: Menu loop - keeps running until the user chooses Exit
repeat {
  
  cat("\n===== BOOKNEST =====\n")
  cat("1. View Book Collection\n")
  cat("2. Access Book Details\n")
  cat("3. Add New Book\n")
  cat("4. Modify Book Details\n")
  cat("5. Count Books\n")
  cat("6. Exit\n")
  
  choice <- trimws(readline("Enter your choice (1-6): "))
   
  if (choice == "1") {
    view_books(book_list)
    
  } else if (choice == "2") {
    access_book(book_list)
    
  } else if (choice == "3") {
    book_list <- add_book(book_list)       # save the updated list
    
  } else if (choice == "4") {
    book_list <- modify_book(book_list)    # save the updated list
    
  } else if (choice == "5") {
    count_books(book_list)
    
  } else if (choice == "6") {
    cat("\nThank you for using BookNest. Goodbye!\n")
    break                                  # leave the loop
    
  } else {
    cat("\nInvalid choice! Please enter a number from 1 to 6.\n")
  }
}
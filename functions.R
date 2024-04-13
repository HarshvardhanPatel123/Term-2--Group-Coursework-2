#load or save the working environment
save_environment <- function(filename) {
  save(list = ls(all.names = TRUE), file = filename)
}
load_environment <- function(filename) {
  load(filename)
}

#parse the input file and update the inventory
parse_and_update_inventory <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
  content <- paste(lines, collapse = " ")
  content <- iconv(content, "latin1", "UTF-8", sub = "byte")
  
  pattern <- "([A-Za-z]+)\\D+(\\d+)\\D+£([0-9]+\\.[0-9]{2})"
  matches <- gregexpr(pattern, content, perl=TRUE)
  data <- regmatches(content, matches)
  
  inventory <- list()  #initialize the inventory afresh each time
  
  for (match_group in data) {
    for (match in match_group) {
      cat("Match found:", match, "\n")
      
      color <- gsub("[^A-Za-z].*$", "", match)  #extract color name
      numbers <- regmatches(match, gregexpr("\\d+\\.?\\d*", match))
      
      if (length(numbers[[1]]) >= 2) {
        number_delivered <- as.numeric(numbers[[1]][1])
        price <- as.numeric(numbers[[1]][length(numbers[[1]])])
        
        inventory[[tolower(color)]] <- list(
          delivered = number_delivered,
          price = price,
          sold = 0
        )
        cat(sprintf("Updated %s: Delivered %d, Price %.2f\n", color, number_delivered, price))
      }
    }
  }
  
  assign("inventory", inventory, envir = .GlobalEnv)
  print(inventory)
}

#update sold quantities for a day
update_sold_quantities <- function() {
  if (!exists("inventory") || length(inventory) == 0) {
    stop("Inventory has not been initialized or is empty.")
  }
  
  #iterating over inventory to update sold quantities
  for (color in names(inventory)) {
    if (nzchar(color)) {  # Check if color name is not empty
      repeat {
        cat(sprintf("Enter the number of %s tins sold today:\n", color))
        sold <- as.integer(readline())
        
        if (!is.na(sold) && sold >= 0) {
          inventory[[color]]$sold <- sold
          break
        } else {
          cat("Invalid input. Please enter a non-negative integer.\n")
        }
      }
    } else {
      cat("Warning: A color name is missing in the inventory.\n")
    }
  }
  
  assign("inventory", inventory, envir = .GlobalEnv)  # Save updates to global environment
}

#add or remove a color
manage_color <- function(color, action) {
  if (action == "add" && !exists("inventory")) {
    inventory[[color]] <- list(delivered = 0, price = 0, sold = 0)
  } else if (action == "remove" && exists("inventory")) {
    inventory[[color]] <- NULL
  }
  
  assign("inventory", inventory, envir = .GlobalEnv)
}

#generate and save the sales report
generate_sales_report <- function(date) {
}
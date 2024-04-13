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

#function to get or initialize inventory
get_or_initialize_inventory <- function() {
  inventory_path <- "inventory.rds"
  if (file.exists(inventory_path)) {
    return(readRDS(inventory_path))
  } else {
    cat("No inventory found. Initializing a new inventory.\n")
    #if no inventory theb create a new empty environment
    inventory <- new.env(hash = TRUE)
    saveRDS(inventory, inventory_path)
    return(inventory)
  }
}

#add or remove a color
manage_color <- function(color, action) {
  inventory <- get_or_initialize_inventory()
  
  if (action == "add") {
    if (!exists(color, envir = inventory)) {
      inventory[[color]] <- list(delivered = 0, sold = 0, revenue = 0, prices = list())
      cat(sprintf("Added new color: %s to inventory.\n", color))
    } else {
      cat(sprintf("Color %s already exists in inventory.\n", color))
    }
  } else if (action == "remove") {
    if (exists(color, envir = inventory)) {
      rm(list = color, envir = inventory)
      cat(sprintf("Removed color: %s from inventory.\n", color))
    } else {
      cat(sprintf("Color %s does not exist in inventory.\n", color))
    }
  }
  
  #save the updated inventory back
  saveRDS(inventory, "inventory.rds")
}

#generate and save the sales report
generate_sales_report <- function(date, filename) {
  if (!exists("inventory") || length(inventory) == 0) {
    stop("Inventory has not been initialized or is empty.")
  }
  
  #create a data frame to store the report data
  report_data <- data.frame(
    Color = character(),
    Delivered = integer(),
    Sold = integer(),
    Remaining = integer(),
    Price = numeric(),
    Revenue = numeric(),
    stringsAsFactors = FALSE
  )
  
  #fill the data frame with inventory data
  for (color in names(inventory)) {
    if (nzchar(color)) {
      delivered = inventory[[color]]$delivered
      sold = inventory[[color]]$sold
      remaining = delivered - sold
      price = inventory[[color]]$price
      revenue = sold * price
      
      #append to data frame
      report_data <- rbind(report_data, data.frame(
        Color = color,
        Delivered = delivered,
        Sold = sold,
        Remaining = remaining,
        Price = price,
        Revenue = revenue
      ))
    }
  }
  
  #specify the filename based on the provided date if not specified
  if (missing(filename)) {
    filename <- paste0("sales_report_", date, ".csv")
  }
  
  #write the report to a CSV file
  write.csv(report_data, filename, row.names = FALSE)
  
  cat(sprintf("Sales report for %s saved as %s\n", date, filename))
}

print.inventory_item <- function(x) {
  cat("Color:", x$name, "\nDelivered:", x$delivered, "\nSold:", x$sold, "\nRemaining:", x$remaining, "\nPrice:", x$price, "\nRevenue:", x$revenue, "\n\n")
}
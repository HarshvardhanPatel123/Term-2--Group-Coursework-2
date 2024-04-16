#load or save the working environment
save_environment <- function(filename) {
  save(list = ls(all.names = TRUE), file = filename)
}
load_environment <- function(filename) {
  load(filename)
}


#parse the input file and update the inventory
parse_and_update_inventory <- function(filepath, output_dir = getwd()) {
  #check if file exists and is readable
  if (!file.exists(filepath)) {
    cat("Error: File does not exist.\n")
    return()
  }
  
  lines <- readLines(filepath, warn = FALSE)
  if (length(lines) == 0) {
    cat("Error: File is empty or unreadable.\n")
    return()
  }
  
  content <- paste(lines, collapse = " ")
  content <- iconv(content, "latin1", "UTF-8", sub = "byte")
  
  pattern <- "([A-Za-z]+)\\D+(\\d+)\\D+£([0-9]+\\.[0-9]{2})"
  matches <- gregexpr(pattern, content, perl=TRUE)
  data <- regmatches(content, matches)
  
  if (length(data[[1]]) == 0) {
    cat("No valid data found in file.\n")
    return()
  }
  
  inventory <- new.env(hash = TRUE)  #initialize or clear previous inventory
  
  for (match_group in data) {
    for (match in match_group) {
      cat("Match found:", match, "\n")
      
      color <- gsub("[^A-Za-z].*$", "", match)
      numbers <- regmatches(match, gregexpr("\\d+\\.?\\d*", match))
      
      if (length(numbers[[1]]) >= 2) {
        number_delivered <- as.numeric(numbers[[1]][1])
        price <- as.numeric(numbers[[1]][2])
        
        if (is.na(number_delivered) || is.na(price)) {
          cat(sprintf("Warning: Non-numeric data encountered for %s, skipping.\n", color))
          next
        }
        
        inventory[[tolower(color)]] <- list(
          delivered = number_delivered,
          price = price,
          sold = 0
        )
        cat(sprintf("Updated %s: Delivered %d, Price %.2f\n", color, number_delivered, price))
      }
    }
  }
  
  save_path <- file.path(output_dir, "inventory.rds")
  saveRDS(inventory, save_path)
  cat(sprintf("Inventory saved to %s\n", save_path))
}


#update sold quantities for a day
update_sold_quantities <- function() {
  inventory <- get_or_initialize_inventory()  #ensure inventory is loaded
  
  if (length(inventory) == 0) {
    stop("Inventory has not been initialized or is empty.")
  }
  
  #temporary data storage to review before final save
  temp_data <- list()
  
  #iterating over inventory to update sold quantities
  for (color in names(inventory)) {
    repeat {
      cat(sprintf("Enter the number of %s tins sold today:\n", color))
      sold_str <- readline()
      
      if (nzchar(sold_str) && grepl("^[0-9]+$", sold_str)) {
        sold <- as.integer(sold_str)
        temp_data[[color]] <- sold
        break
      } else {
        cat("Invalid input. Please enter a non-negative integer.\n")
      }
    }
  }
  
  #display all collected data for confirmation
  cat("You entered the following sales data:\n")
  for (color in names(temp_data)) {
    cat(sprintf("%s: %d sold\n", color, temp_data[[color]]))
  }
  
  #confirm if data is correct
  cat("Is this data correct? (yes/no):\n")
  response <- tolower(readline())
  
  if (response == "yes") {
    #update inventory with confirmed data
    for (color in names(temp_data)) {
      inventory[[color]]$sold <- temp_data[[color]]
      inventory[[color]]$remaining <- inventory[[color]]$delivered - inventory[[color]]$sold  #calculate remaining stock
      cat(sprintf("%s: Remaining stock %d\n", color, inventory[[color]]$remaining))
    }
    
    saveRDS(inventory, "inventory.rds")  #save updated inventory
    cat("Sales data updated and saved.\n")
  } else {
    cat("Data update canceled. No changes made.\n")
  }
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
add_color <- function(color, delivered = 0, price = 0.00) {
  inventory <- get_or_initialize_inventory()  # Load inventory
  
  color <- tolower(color)  # Normalize color name to lowercase for consistency
  if (!exists(color, envir = inventory)) {
    inventory[[color]] <- list(
      delivered = delivered,
      sold = 0,  # Initialize sold as 0
      price = price
    )
    cat(sprintf("Added new color: %s to inventory with %d delivered and price £%.2f.\n", color, delivered, price))
  } else {
    cat(sprintf("Color %s already exists in inventory.\n", color))
  }
  
  # Save the updated inventory
  saveRDS(inventory, "inventory.rds")
}


remove_color <- function(color) {
  inventory <- get_or_initialize_inventory()  # Load the current inventory
  
  color <- tolower(color)  # Normalize color name to lowercase for consistency
  if (!exists(color, envir = inventory)) {
    cat(sprintf("Color %s does not exist in the inventory.\n", color))
    return()
  }
  
  # Retrieve the color data
  color_data <- inventory[[color]]
  
  # Set delivered and remaining stock to zero but preserve historical data
  color_data$delivered <- 0
  color_data$sold <- 0  # Assuming you want to zero out any unsold stock
  color_data$inactive <- TRUE  # Mark the color as inactive
  
  # Save the updated color data back to the inventory
  inventory[[color]] <- color_data
  
  # Save the updated inventory
  saveRDS(inventory, "inventory.rds")
  cat(sprintf("Color %s has been deactivated and remaining stock removed.\n", color))
}


#generate and save the sales report
generate_sales_report <- function(date, filename = NULL) {
  inventory <- get_or_initialize_inventory()  # Load inventory
  if (length(ls(envir = inventory)) == 0) {
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
  for (color in ls(envir = inventory)) {
    color_data <- inventory[[color]]
    if (!is.numeric(color_data$sold) || !is.numeric(color_data$price)) {
      cat(sprintf("Error with data types for color %s: sold or price are not numeric.\n", color))
      next  #skip this color if data types are incorrect
    }
    
    sold = as.numeric(color_data$sold)
    price = as.numeric(color_data$price)
    delivered = as.numeric(color_data$delivered)
    remaining = delivered - sold
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
  
  #specify the filename based on the provided date if not specified
  if (missing(filename)) {
    filename <- paste0("sales_report_", date, ".csv")
  }
  
  #write the report to a CSV file
  write.csv(report_data, filename, row.names = FALSE)
  cat(sprintf("Sales report for %s saved as %s\n", date, filename))
}


history_color <- function(color) {
  inventory <- get_or_initialize_inventory()
  
  if (!exists(color, envir = inventory)) {
    cat(sprintf("No data found for color: %s\n", color))
  } else {
    color_data <- inventory[[color]]
    cat(sprintf("Sales History for Color: %s\n", color))
    cat(sprintf("Delivered: %d\n", color_data$delivered))
    cat(sprintf("Sold: %d\n", color_data$sold))
    cat(sprintf("Price: £%.2f\n", color_data$price))
    if (color_data$sold > 0) {
      cat(sprintf("Revenue: £%.2f\n", color_data$sold * color_data$price))
    } else {
      cat("No sales have been made yet.\n")
    }
  }
}


history_week <- function(week_date) {
  inventory <- get_or_initialize_inventory()
  cat(sprintf("Sales Data for Week Starting: %s\n", week_date))
  
  if (length(ls(envir = inventory)) == 0) {
    cat("No inventory data available.\n")
    return()
  }
  
  for (color in ls(envir = inventory)) {
    color_data <- inventory[[color]]
    if (is.numeric(color_data$sold) && is.numeric(color_data$price)) {
      sold = as.numeric(color_data$sold)
      price = as.numeric(color_data$price)
      revenue = sold * price
      cat(sprintf("%s: Sold %d, Price £%.2f, Revenue £%.2f\n", color, sold, price, revenue))
    } else {
      cat(sprintf("Check data types for %s: Ensure 'sold' and 'price' are numeric.\n", color))
    }
  }
}


current_stock <- function() {
  inventory <- get_or_initialize_inventory()
  cat("Current Stock Levels:\n")
  
  if (length(ls(envir = inventory)) == 0) {
    cat("No inventory data available.\n")
    return()
  }
  
  for (color in ls(envir = inventory)) {
    color_data <- inventory[[color]]
    remaining_stock = color_data$delivered - color_data$sold
    cat(sprintf("%s: Remaining %d\n", color, remaining_stock))
  }
}
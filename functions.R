#load or save the working environment
save_environment <- function(filename) {
  save(list = ls(all.names = TRUE), file = filename)
}
load_environment <- function(filename) {
  load(filename)
}


#parse the input file and update the inventory
parse_and_update_inventory <- function(filepath, output_dir = getwd()) {
  lines <- readLines(filepath, warn = FALSE)
  content <- paste(lines, collapse = " ")
  content <- iconv(content, "latin1", "UTF-8", sub = "byte")
  
  pattern <- "([A-Za-z]+)\\D+(\\d+)\\D+£([0-9]+\\.[0-9]{2})"
  matches <- gregexpr(pattern, content, perl=TRUE)
  data <- regmatches(content, matches)
  
  # Initialize or clear previous inventory
  inventory <- new.env(hash = TRUE)  # Use an environment to store inventory
  
  # Update inventory with new data
  for (match_group in data) {
    for (match in match_group) {
      cat("Match found:", match, "\n")
      
      color <- gsub("[^A-Za-z].*$", "", match)  # Extract color name
      numbers <- regmatches(match, gregexpr("\\d+\\.?\\d*", match))
      
      if (length(numbers[[1]]) >= 2) {
        number_delivered <- as.numeric(numbers[[1]][1])
        price <- as.numeric(numbers[[1]][2])
        
        inventory[[tolower(color)]] <- list(
          delivered = number_delivered,
          price = price,
          sold = 0
        )
        cat(sprintf("Updated %s: Delivered %d, Price %.2f\n", color, number_delivered, price))
      }
    }
  }
  
  # Save inventory to an RDS file in the specified directory
  save_path <- file.path(output_dir, "inventory.rds")
  saveRDS(inventory, save_path)
  cat(sprintf("Inventory saved to %s\n", save_path))
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
  inventory <- get_or_initialize_inventory() #load inventory
  
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
generate_sales_report <- function(date, filename = NULL) {
  inventory <- get_or_initialize_inventory()  # Load inventory
  if (!exists("inventory") || length(inventory) == 0) {
    stop("Inventory has not been initialized or is empty.")
  }
  
  # Create a data frame to store the report data
  report_data <- data.frame(
    Color = character(),
    Delivered = integer(),
    Sold = integer(),
    Remaining = integer(),
    Price = numeric(),
    Revenue = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Fill the data frame with inventory data
  for (color in ls(envir = inventory)) {
    color_data <- inventory[[color]]
    if (is.numeric(color_data$sold) && is.numeric(color_data$price)) {
      sold = as.numeric(color_data$sold)
      price = as.numeric(color_data$price)
      delivered = as.numeric(color_data$delivered)
      remaining = delivered - sold
      revenue = sold * price
      
      # Append to data frame
      report_data <- rbind(report_data, data.frame(
        Color = color,
        Delivered = delivered,
        Sold = sold,
        Remaining = remaining,
        Price = price,
        Revenue = revenue
      ))
    } else {
      cat(sprintf("Error with data types for color %s: sold or price are not numeric.\n", color))
    }
  }
  
  # Specify the filename based on the provided date if not specified
  if (missing(filename)) {
    filename <- paste0("sales_report_", date, ".csv")
  }
  
  # Write the report to a CSV file
  write.csv(report_data, filename, row.names = FALSE)
  cat(sprintf("Sales report for %s saved as %s\n", date, filename))
}


history_color <- function(color) {
  inventory <- get_or_initialize_inventory()  # Make sure inventory is loaded from the RDS file
  
  if (!exists(color, envir = inventory)) {
    cat(sprintf("No data found for color: %s\n", color))
  } else {
    color_data <- inventory[[color]]
    cat(sprintf("Sales History for Color: %s\n", color))
    cat(sprintf("Delivered: %d\n", color_data$delivered))
    cat(sprintf("Sold: %d\n", color_data$sold))
    cat(sprintf("Price: %.2f\n", color_data$price))
    cat(sprintf("Revenue: %.2f\n", color_data$sold * color_data$price))
  }
}


history_week <- function(week_date) {
  inventory <- get_or_initialize_inventory()  # Load inventory
  cat(sprintf("Sales Data for Week Starting: %s\n", week_date))
  
  # Iterate over each color in the inventory
  for (color in ls(envir = inventory)) {
    color_data <- inventory[[color]]
    # Ensure that both sold and price are numeric before calculation
    if (is.numeric(color_data$sold) && is.numeric(color_data$price)) {
      sold = as.numeric(color_data$sold)
      price = as.numeric(color_data$price)
      revenue = sold * price
      
      cat(sprintf("%s: Sold %d, Price %.2f, Revenue %.2f\n", color, sold, price, revenue))
    } else {
      cat(sprintf("Data type error for color %s: 'sold' or 'price' not numeric.\n", color))
    }
  }
}


current_stock <- function() {
  inventory <- get_or_initialize_inventory()  # Load inventory
  cat("Current Stock Levels:\n")
  
  for (color in ls(envir = inventory)) {
    color_data <- inventory[[color]]
    remaining_stock = color_data$delivered - color_data$sold  # Calculate remaining stock using correct assignment
    cat(sprintf("%s: %d remaining\n", color, remaining_stock))
  }
}
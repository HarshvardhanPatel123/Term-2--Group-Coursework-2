#load or save the working environment
save_environment <- function(filename) {
  save(list = ls(all.names = TRUE), file = filename)
}
load_environment <- function(filename) {
  load(filename)
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


#parse the input file and update the inventory
parse_and_update_inventory <- function(filepath, output_dir = getwd()) {
  #load existing inventory or initialize a new environment if it doesn't exist
  existing_inventory <- if (file.exists(file.path(output_dir, "inventory.rds"))) {
    readRDS(file.path(output_dir, "inventory.rds"))
  } else {
    new.env(hash = TRUE)
  }
  
  lines <- readLines(filepath, warn = FALSE)
  content <- paste(lines, collapse = " ")
  content <- iconv(content, "latin1", "UTF-8", sub = "byte")
  
  pattern <- "([A-Za-z]+)\\D+(\\d+)\\D+£([0-9]+\\.[0-9]{2})"
  matches <- gregexpr(pattern, content, perl=TRUE)
  data <- regmatches(content, matches)
  
  inventory <- new.env(hash = TRUE)  # Initialize new inventory
  
  #copy existing data to new inventory structure
  for (color in ls(envir = existing_inventory)) {
    inventory[[color]] <- existing_inventory[[color]]
  }
  
  #process new data and merge with existing
  for (match_group in data) {
    for (match in match_group) {
      color <- tolower(gsub("[^A-Za-z].*$", "", match))
      numbers <- regmatches(match, gregexpr("\\d+\\.?\\d*", match))
      if (length(numbers[[1]]) >= 2) {
        number_delivered <- as.numeric(numbers[[1]][1])
        price <- as.numeric(numbers[[1]][2])
        
        #ensure inactive is checked properly
        if (!exists(color, envir = inventory) || !isTRUE(inventory[[color]]$inactive)) {
          inventory[[color]] <- list(
            delivered = number_delivered,
            price = price,
            sold = 0,
            inactive = FALSE  #default to FALSE if not set
          )
        }
      }
    }
  }
  
  #save the merged inventory
  saveRDS(inventory, file.path(output_dir, "inventory.rds"))
  cat(sprintf("Inventory updated and saved to %s\n", file.path(output_dir, "inventory.rds")))
}


#update sold quantities for a day
update_sold_quantities <- function(date_str) {
  inventory <- get_or_initialize_inventory()  #load inventory
  
  if (length(ls(envir = inventory)) == 0) {
    stop("Inventory has not been initialized or is empty.")
  }
  
  daily_sales <- list()
  
  for (color in ls(envir = inventory)) {
    color_data <- inventory[[color]]
    
    if (isTRUE(color_data$inactive)) {
      next  #skip inactive colors
    }
    
    #initialize sales for the date if not already present
    if (is.null(color_data$sales[[date_str]])) {
      color_data$sales[[date_str]] <- 0
    }
    
    current_available <- color_data$delivered - sum(unlist(color_data$sales))  #current stock available for sale
    
    repeat {
      cat(sprintf("Enter the number of %s tins sold on %s (Available: %d):\n", color, date_str, current_available))
      sold_str <- readline()
      if (nzchar(sold_str) && grepl("^[0-9]+$", sold_str)) {
        sold <- as.integer(sold_str)
        if (sold > current_available) {
          cat("Error: Sold quantity cannot exceed the quantity available. Please enter a valid number.\n")
        } else {
          color_data$sales[[date_str]] <- color_data$sales[[date_str]] + sold  #accumulate sales for the date
          break  #exit the repeat loop after valid input
        }
      } else {
        cat("Invalid input. Please enter a non-negative integer.\n")
      }
    }
    inventory[[color]] <- color_data  #update inventory with the modified color data
  }
  
  saveRDS(inventory, "inventory.rds")  # Save the updated inventory
  cat("Sales data updated and saved.\n")
}


#add or remove a color
add_color <- function(color, delivered, price) {
  inventory <- get_or_initialize_inventory()  #load inventory
  
  color <- tolower(color)  #normalize color name to lowercase for consistency
  if (!exists(color, envir = inventory)) {
    inventory[[color]] <- list(
      delivered = delivered,
      sold = 0,
      price = price
    )
    cat(sprintf("Added new color: %s to inventory with %d delivered and price £%.2f.\n", color, delivered, price))
  } else {
    cat(sprintf("Color %s already exists in inventory.\n", color))
  }
  
  #save the updated inventory
  saveRDS(inventory, "inventory.rds")
}


#function to refill the stock of a specific color
refill_color <- function(color, additional_amount) {
  inventory <- get_or_initialize_inventory()  #load the current inventory
  
  color <- tolower(color)  # Normalize color name to lowercase for consistency
  if (!exists(color, envir = inventory)) {
    cat(sprintf("Color %s does not exist in the inventory. Cannot refill non-existent color.\n", color))
  } else {
    #if color exists, add the additional amount to the delivered stock
    inventory[[color]]$delivered <- inventory[[color]]$delivered + additional_amount
    cat(sprintf("Refilled %s: Added %d units. New delivered total: %d.\n", color, additional_amount, inventory[[color]]$delivered))
  }
  
  #save the updated inventory
  saveRDS(inventory, "inventory.rds")
}


remove_color <- function(color) {
  inventory <- get_or_initialize_inventory()  #load inventory
  
  color <- tolower(color)  #normalize color name
  if (!exists(color, envir = inventory)) {
    cat(sprintf("Color %s does not exist in the inventory.\n", color))
    return()
  }
  
  #set the color as inactive
  inventory[[color]]$inactive <- TRUE
  
  cat(sprintf("Color %s has been deactivated and will no longer be sold.\n", color))
  
  #save the updated inventory
  saveRDS(inventory, "inventory.rds")
}


#generate and save the sales report
generate_sales_report <- function(date) {
  inventory <- get_or_initialize_inventory()  #load inventory
  if (length(ls(envir = inventory)) == 0) {
    stop("Inventory has not been initialized or is empty.")
  }
  
  report_data <- data.frame(
    Color = character(),
    Delivered = integer(),
    Sold = integer(),
    Remaining = integer(),
    Price = numeric(),
    Revenue = numeric(),
    stringsAsFactors = FALSE
  )
  
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
    
    report_data <- rbind(report_data, data.frame(
      Color = color,
      Delivered = delivered,
      Sold = sold,
      Remaining = remaining,
      Price = price,
      Revenue = revenue
    ))
  }
  
  filename <- paste0(date, ".csv")  #use date directly for filename
  
  write.csv(report_data, filename, row.names = FALSE)
  cat(sprintf("Sales report for %s saved as %s\n", date, filename))
}


history_color <- function(color) {
  inventory <- get_or_initialize_inventory()
  
  color <- tolower(color)
  if (!exists(color, envir = inventory)) {
    cat(sprintf("No data found for color: %s\n", color))
  } else {
    color_data <- inventory[[color]]
    status = ifelse(isTRUE(color_data$inactive), "Inactive", "Active")
    cat(sprintf("Sales History for Color: %s (Status: %s)\n", color, status))
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
  
  for (color in ls(envir = inventory)) {
    color_data <- inventory[[color]]
    #check if there are sales for the specific week and set to 0 if not
    weekly_sold <- if (is.list(color_data$sales) && !is.null(color_data$sales[[week_date]])) {
      color_data$sales[[week_date]]
    } else {
      0  #default to 0 if no sales data for the week
    }
    price = color_data$price
    revenue = weekly_sold * price
    
    cat(sprintf("%s: Sold %d, Price £%.2f, Revenue £%.2f\n", color, weekly_sold, price, revenue))
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
    status = ifelse(isTRUE(color_data$inactive), "Inactive", "Active")
    remaining_stock = color_data$delivered - color_data$sold
    cat(sprintf("%s (%s): Remaining %d\n", color, status, remaining_stock))
  }
}
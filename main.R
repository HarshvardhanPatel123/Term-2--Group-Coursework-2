#load necessary functions
setwd("~/Documents/GitHub/Term-2--Group-Coursework-2/")
source("functions.R")

cat("If you want to start from the beginning enter 'restart', otherwise enter anything else: ")
decision <- tolower(readline())

if (decision == "restart") {
  repeat {
    cat("Enter the date for the inventory update (YYYY-MM-DD): ")
    date_input <- readline()
    formatted_date <- gsub("-", "", date_input)  #remove dashes
    weekly_file <- paste0("wc", formatted_date, ".txt")  #construct the filename
    
    if (file.exists(weekly_file)) {
      #check if the inventory file exists before trying to remove it
      inventory_path <- "inventory.rds"
      if (file.exists(inventory_path)) {
        file.remove(inventory_path)
      } else {
        cat("No existing inventory to clear.\n")
      }
      parse_and_update_inventory(weekly_file)
      break
    } else {
      cat(sprintf("No file found for date %s. Please enter a valid date.\n", date_input))
    }
  }
} else {
  cat("Continuing with existing data...\n")
  cat("Please enter today's date (YYYY-MM-DD) for operations:\n")
  date_input <- readline()
}


#add new color with initial stock and price
#add_color("pink", 50, 9.99)
add_color("white", 200, 0.99)
add_color("orange", 200, 1.99)

#add tins to existing colors
refill_color("blue", 25)

#deactivate the color
remove_color("green")

#update the inventory with the number of tins sold for the day
update_sold_quantities(date_input)

#print current stock levels
current_stock()

#generate and save the sales report
#generate_sales_report("2024-03-04")
generate_sales_report(date_input)  #use the actual date for reporting

#show history for a specific color and week
history_color("red")  #print sales history for the color 'red'
history_color("blue")  #print sales history for the color 'blue'
history_week("2024-03-04")  #print sales data for the week starting 2024-03-04
history_week("2024-05-20")  #print sales data for the week starting 2024-05-20

#save the current R environment to preserve changes
save_environment("my_inventory_env.RData")

#optionally, load the environment if this script is restarted
# load_environment("my_inventory_env.RData")
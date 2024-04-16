#load necessary functions
setwd("~/Documents/GitHub/Term-2--Group-Coursework-2/")
source("functions.R")

#weekly file update - ensure the correct file name is used
weekly_file <- "wc20240311.txt"  #change this each week
parse_and_update_inventory(weekly_file)

#update the inventory with the number of tins sold for the day
update_sold_quantities()

#add or remove colors as needed
add_color("purple")
remove_color("green")

#print current stock levels
current_stock()

#generate and save the sales report
generate_sales_report("2024-03-04")

#show history for a specific color and week
history_color("red")  #print sales history for the color 'red'
history_week("2024-03-04")  #print sales data for the week starting 2024-03-04

#save the current R environment to preserve changes
save_environment("my_inventory_env.RData")

#optionally, load the environment if this script is restarted
# load_environment("my_inventory_env.RData")
#load necessary functions
source("functions.R")

#initialize inventory with the data from the first week
parse_and_update_inventory("wc20240304.txt")

#update the inventory with the number of tins sold for the day
update_sold_quantities()

#to add or remove a color
manage_color("purple", "add")
manage_color("green", "remove")

#print current stock levels
current_stock()

#generate and save the sales report
generate_sales_report("2024-03-04")

#to show history
history_color("red")  #print sales history for the color 'red'
history_week("2024-03-04")  #print sales data for the week starting 2024-03-04

#save the current R environment
save_environment("my_inventory_env.RData")

#load the saved environment into a new session
load_environment("my_inventory_env.RData")
#load or save the working environment
save_environment <- function(filename) {
  save(list = ls(all.names = TRUE), file = filename)
}
load_environment <- function(filename) {
  load(filename)
}

#parse the input file and update the inventory
parse_and_update_inventory <- function(filepath) {
  
}

#update sold quantities for a day
update_sold_quantities <- function() {
  
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
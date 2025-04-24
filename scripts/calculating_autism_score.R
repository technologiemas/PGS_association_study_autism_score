# Load the data
data = readRDS("./data/processed/full_required_dataset.rds")

# a function that sums the individual items into an autism scale
sum_autism = function(data, items) {
  # take the variable name as a string
  name = paste0(as.character(substitute(items)), "_aut_sum")
  
  # create a new column with col name as name that sums the autism scale items
  data[[name]] = rowSums(data %>% select(contains(items)))
  return(data)
}

data = sum_autism(data, m12)
data = sum_autism(data, v12)
data = sum_autism(data, t12)
data = sum_autism(data, ysr14)

# save data as new dataset rds file
saveRDS(data, file = "./data/processed/autism_score_calc.rds")
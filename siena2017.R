source("R/transform.R")
source("R/analysis.R")

# course information
COURSE_ID = "DSA/30240184X/2015_T1"
INSTRUCTORS = c("ruby-cai")

# read data
COLUMNS = c("id", "create_time", "user_id", "user_name", "course_id", "parent_id")
data = read.csv("data/forum_interaction_3.csv", stringsAsFactors = FALSE, allowEscapes = TRUE)
#data = data[1:190955, ]   # TODO: fix dataset
table(data$course_id)
data = data[data$course_id == COURSE_ID, ]

# fix anonymous users
anonymous_count = nrow(data[data$user_id == 0, ])
if (anonymous_count > 0) {
  data[data$user_id == 0, "user_id"] = paste("anonymous_", seq(1:anonymous_count), sep = "")
  warning(paste(anonymous_count, "anonymous users were found"))
}

# users and roles
users = create_users_and_roles(data, INSTRUCTORS)
users_count = nrow(users)

# add fields: user_from and user_to
data$user_from <- data$user_id
data$user_to <- 0
for (row in 1:nrow(data)) {
  parent_id <- data[row, "parent_id"]
  user_from <- data[data$id == parent_id, "user_from"]
  if (length(user_from) == 1) {
    # only unique user_from exists for parent data
    data[row, "user_to"] <- user_from
  }
  else {
    data[row, "user_to"] <- NA
    if(!is.na(parent_id)) {
      # integrity error
      warning(paste("Lack of integrity:", length(user_from), "record(s) for id =", parent_id)) 
    }
  }
}

# keep only links "from -> to"
data <- na.omit(data)

# waves
data$date_num = as.numeric(as.Date(data$create_time, format = "%m/%d/%Y"))
data$course_day = data$date_num - min(data$date_num) + 1
time_points = c(1, 5, 15, 30, 50, 80, 100)
data$wave = calculate_waves(data, time_points, column = "course_day")
table(data$wave)

# analyze data
net_data <- create_network_data(data, users)
net_effects <- includeEffects(getEffects(net_data), recip, transTrip)

print01Report(net_data, net_effects, modelname = "descriptive")

for (model_num in 1:3) {
  model_name <- paste("model", model_num, sep="")
  switch(model_num, 
         model1 = {model_effects <- net_effects}, 
         model2 = {model_effects <- includeEffects(net_effects, outPop)}, 
         model3 = {model_effects <- includeEffects(net_effects, between)})
  # TODO: extract coef from results
  result <- perform_siena(model_name, net_data, model_effects)
}


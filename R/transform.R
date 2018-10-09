read_csv_files = function(filenames, columns, ...) {
  first <- TRUE
  data <- NA
  for (filename in filenames) {
    new_data = read.csv(filename, ...)
    for (column in columns) {
      if (!(column %in% colnames(new_data))) {
        new_data[, column] <- NA
      }
    }
    new_data <- new_data[, columns]
    if (first) {
      first = FALSE
      data = new_data
    }
    else {
      data = rbind(data, new_data)
    }
  }
  data
}


create_users_and_roles <- function(data, instructor_names, user_id = "user_id"){
  user_ids <- unique(data[, user_id])
  users_count <- length(user_ids)
  users <- data.frame(user_id = user_ids, user_seq_num = seq_len(users_count), stringsAsFactors = FALSE)
  users$role_name <- "student"
  instructor_ids <- unique(data[data$user_name %in% instructor_names, user_id])
  users[users$user_id %in% instructor_ids, "role_name"] <- "instructor"
  users$role <- as.numeric(users$role_name == "instructor")
  users
}

calculate_waves <- function(data, time_points, column) {
  waves_count = length(time_points) - 1
  data$wave = 0 
  for (wave_num in 1:waves_count) {
    data[data[column] >= time_points[wave_num + 1], "wave"] = wave_num
  }
  data$wave
}
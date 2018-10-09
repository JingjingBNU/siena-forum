library(Matrix)
library(RSiena)

create_network_data <- function(data, users)  {
  wave_count = max(data$wave)
  users_count = nrow(users)
  
  # create empty dataframe
  edges = data[FALSE, c("user_from", "user_to", "wave")]  

  # create list of edges for increasing network
  for (wave_num in 0:wave_count) {
    wave_edges = data[data$wave == wave_num, c("user_from", "user_to", "wave")]
    wave_edges = rBind(edges, wave_edges)  # for increasing network, add new edges to all previous ones
    wave_edges$wave = wave_num
    edges = rBind(edges, wave_edges)
  }
  edges = unique(edges)

  recode_user <- function(df, column) {
    df["user_id"] <- df[column]
    df <- merge(df, users[c("user_id", "user_seq_num")], by = "user_id")
    df[column] <- df["user_seq_num"]
    df <- df[c("user_from","user_to", "wave")]
    df
  }
  
  edges = recode_user(edges, "user_from")
  edges = recode_user(edges, "user_to")
  edges_list = by(edges, edges[, 3], function(x) {
    spMatrix(users_count, users_count, x[, 1], x[, 2], rep(1, length(x[, 1])))
  })

  net <- sienaDependent(edges_list)
  net_data <- sienaDataCreate(net)
  net_data
}


# SIENA  ---------------------------

perform_siena <- function(model_name, data, effects, ...) {
  
  siena07ToConvergence <- function(alg, dat, eff, ans0 = NULL, ...) {
    numr <- 0
    ans <- siena07(alg, data = dat, effects = eff, prevAns = ans0) # the first run
    repeat {
      numr <- numr+1 # count number of repeated runs
      tm <- max(abs(ans$tconv)) # convergence indicator
      cat(numr, tm,"\n") # report how far we are
      if (tm < 0.2) {break} # success
      if (tm > 8) {break} # divergence without much hope
      # of returning to good parameter values
      if (numr > 10) {break} # now it has lasted too long
      ans <- siena07(alg, data = dat, effects = eff, prevAns = ans)
    }
    if (tm > 0.2)
    {
      cat("Warning: convergence inadequate.\n")
    }
    ans
  }
  
  algorithm <- sienaAlgorithmCreate(projname = paste("output/", model_name, sep=""))
  answer <- siena07ToConvergence(algorithm, data, effects)
  answer
}
findsplit <- function(response, data, data_with_date, ssediff_coef, date_coef, freq_coef, weights) {
  y <- data[[response]]
  d <- data_with_date[[date_var]]
  cos <- data[["cos"]]
  sin <- data[["sin"]]
  a <- ssediff_coef
  b <- date_coef
  c <- freq_coef
  ssediffmax <- 0
  decision_metric_max <- 0
  x_below_median_datediff <- 0
  x_above_median_datediff <- 0
  xselect <- NULL
  ssediff <- NULL
  datediff <- NULL
  frequencydiff <- NULL
  ssediff_adj <- NULL
  datediff_adj <- NULL
  frequencydiff_adj <- NULL
  features <- which(names(data) != response)
  for (i in features) {
    x <- data[[i]]
    xt <- rep(x, weights)
    yt <- rep(y, weights)
    dt <- rep(d, weights)
    cost <- rep(cos, weights)
    sint <- rep(sin, weights)
    ft <- cbind(cost, sint)
    qx <- unique(quantile(xt, prob = seq(from = 0.1, to = 0.9, by = 0.05)))
    ssediff <- c(ssediff, sapply(qx, function(q) {
      sse_all_data <- sum((mean(yt) - yt)^2) 
      x_below_median_sse <- ifelse(nrow(ft[xt <= q,]) == 0, 0, sum((mean(yt[xt <= q]) - yt[xt <= q])^2)) 
      x_above_median_sse <- ifelse(nrow(ft[xt > q,]) == 0, 0, sum((mean(yt[xt > q]) - yt[xt > q])^2))
      sse_all_data - (x_below_median_sse + x_above_median_sse)
    })) 
    datediff <- c(datediff, sapply(qx, function(q) {
      datediff_all_data <- max(dt) - min(dt)
      datediff_below <- ifelse(nrow(ft[xt <= q,]) == 0, 0, max(dt[xt <= q]) - min(dt[xt <= q]))
      datediff_above <- ifelse(nrow(ft[xt > q,]) == 0, 0, max(dt[xt > q]) - min(dt[xt > q]))
      ifelse(datediff_all_data - ( ((length(dt[xt <= q]) / length(dt)) * datediff_below) + ((length(dt[xt > q]) / length(dt)) * datediff_above) ) < 0, 0, datediff_all_data - ( ((length(dt[xt <= q]) / length(dt)) * datediff_below) + ((length(dt[xt > q]) / length(dt)) * datediff_above) ))
    }))
    frequencydiff <- c(frequencydiff, sapply(qx, function(q) {
      frequencydiff_all_data <- sum(sqrt((cost - mean(cost))^2 + (sint - mean(sint))^2))
      frequencydiff_below <- ifelse(nrow(ft[xt <= q,]) == 0, 0, sum(sqrt((cost[xt <= q] - mean(cost[xt <= q]))^2 + (sint[xt <= q] - mean(sint[xt <= q]))^2)))
      frequencydiff_above <- ifelse(nrow(ft[xt > q,]) == 0, 0, sum(sqrt((cost[xt > q] - mean(cost[xt > q]))^2 + (sint[xt > q] - mean(sint[xt > q]))^2)))
      ifelse(frequencydiff_all_data - (frequencydiff_below + frequencydiff_above) < 0, 0, frequencydiff_all_data - (frequencydiff_below + frequencydiff_above))
    }))
  }
  
  for (i in features) {
    x <- data[[i]]
    xt <- rep(x, weights)
    yt <- rep(y, weights)
    dt <- rep(d, weights)
    cost <- rep(cos, weights)
    sint <- rep(sin, weights)
    ft <- cbind(cost, sint)
    qx <- unique(quantile(xt, prob = seq(from = 0.1, to = 0.9, by = 0.05)))
    ssediff_adj <- sapply(qx, function(q) {
      sse_all_data_adj <- sum((mean(yt) - yt)^2)
      x_below_median_sse_adj <- ifelse(nrow(ft[xt <= q,]) == 0, 0, sum((mean(yt[xt <= q]) - yt[xt <= q])^2))
      x_above_median_sse_adj <- ifelse(nrow(ft[xt > q,]) == 0, 0, sum((mean(yt[xt > q]) - yt[xt > q])^2))
      sse_all_data_adj - (x_below_median_sse_adj + x_above_median_sse_adj)
    })
    ssediff_adj <- (as.numeric(ssediff_adj) - min(as.numeric(ssediff))) / (max(as.numeric(ssediff)) - min(as.numeric(ssediff)))
    
    datediff_adj <- sapply(qx, function(q) {
      datediff_all_data_adj <- max(dt) - min(dt)
      datediff_below_adj <- ifelse(nrow(ft[xt <= q,]) == 0, 0, max(dt[xt <= q]) - min(dt[xt <= q])) #998
      datediff_above_adj <- ifelse(nrow(ft[xt > q,]) == 0, 0, max(dt[xt > q]) - min(dt[xt > q])) #917
      ifelse(datediff_all_data_adj - ( ((length(dt[xt <= q]) / length(dt)) * datediff_below_adj) + ((length(dt[xt > q]) / length(dt)) * datediff_above_adj) ) < 0, 0, datediff_all_data_adj - ( ((length(dt[xt <= q]) / length(dt)) * datediff_below_adj) + ((length(dt[xt > q]) / length(dt)) * datediff_above_adj) ))
    })
    datediff_adj <- (as.numeric(datediff_adj) - min(as.numeric(datediff))) / (max(as.numeric(datediff)) - min(as.numeric(datediff)))
    
    frequencydiff_adj <- sapply(qx, function(q) {
      frequencydiff_all_data_adj <- sum(sqrt((cost - mean(cost))^2 + (sint - mean(sint))^2))
      frequencydiff_below_adj <- ifelse(nrow(ft[xt <= q,]) == 0, 0, sum(sqrt((cost[xt <= q] - mean(cost[xt <= q]))^2 + (sint[xt <= q] - mean(sint[xt <= q]))^2)))
      frequencydiff_above_adj <- ifelse(nrow(ft[xt > q,]) == 0, 0, sum(sqrt((cost[xt > q] - mean(cost[xt > q]))^2 + (sint[xt > q] - mean(sint[xt > q]))^2)))
      ifelse(frequencydiff_all_data_adj - (frequencydiff_below_adj + frequencydiff_above_adj) < 0, 0, frequencydiff_all_data_adj - (frequencydiff_below_adj + frequencydiff_above_adj))
    })
    frequencydiff_adj <- (as.numeric(frequencydiff_adj) - min(as.numeric(frequencydiff))) / (max(as.numeric(frequencydiff)) - min(as.numeric(frequencydiff)))
    decision_metric <- a * ssediff_adj + b * datediff_adj + c * frequencydiff_adj
    if ( isTRUE( max(decision_metric) >= decision_metric_max ) == TRUE ) {
      decision_metric_max <- max(decision_metric)
      xselect <- i
      splitpoint <- qx[which.max(decision_metric)]
    }
  }
  if (is.null(xselect)) return(NULL)
  return(partysplit(
    varid = as.integer(xselect),
    breaks = as.numeric(splitpoint),
    info = list(decision_metric_max = decision_metric_max
    )))
}

growtree <- function(id = 1L, response, data, data_with_date, ssediff_coef, date_coef, freq_coef, weights) {
  if (sum(weights) < 20) return(partynode(id = id))
  sp <- findsplit(response, data, data_with_date, ssediff_coef, date_coef, freq_coef, weights)
  if (is.null(sp)) return(partynode(id = id))
  kidids <- kidids_split(sp, data = data)
  kids <- vector(mode = "list", length = max(kidids))
  for (kidid in 1:max(kidids)) {
    w <- weights 
    w[kidids != kidid] <- 0
    if(kidid > 1) { 
      myid <- max(nodeids(kids[[kidid - 1]]))
    } else { 
      myid <- id
    }
    kids[[kidid]] <- growtree(id = as.integer(myid + 1), response, data, data_with_date, ssediff_coef, date_coef, freq_coef, w) }
  return(partynode(id = as.integer(id), split = sp, kids = kids)) }

mytree <- function(formula, date_var, ssediff_coef, date_coef, freq_coef, data, weights = NULL) {
  response <- all.vars(formula)[1]
  date_var = date_var
  data_with_date <- data[complete.cases(data), c(date_var, all.vars(formula)[-1], response)]
  data <- data[complete.cases(data), c(all.vars(formula)[-1], response)]
  stopifnot(all(sapply(data, is.numeric)))
  if (is.null(weights)) weights <- rep(1, nrow(data))
  stopifnot(length(weights) == nrow(data) &
              max(abs(weights - floor(weights))) < .Machine$double.eps)
  nodes <- growtree(id = 1L, response, data, data_with_date, ssediff_coef, date_coef, freq_coef, weights)
  fitted <- fitted_node(nodes, data = data)
  ret <- party(nodes,
               data = data,
               fitted = data.frame(
                 "(fitted)" = fitted,
                 "(response)" = data[[response]],
                 "(weights)" = weights,
                 check.names = FALSE),
               terms = terms(formula))
  as.constparty(ret)
}
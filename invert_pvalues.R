
invert_pbinom_leftsided = function(q, size, conf_level = 0.95, tol = 1e-5) {
  log_alpha <- log(1 - conf_level);
  prob_seq <- seq(1e-6, 1 - 1e-6, length = 1e3)
  curr_lb <- pbinom(q = q, size = size, prob = prob_seq, lower.tail = F, log.p = T)
  curr_min <- which.min(abs(curr_lb - log_alpha))
  while(abs(curr_lb[curr_min] - log_alpha) > 1e-5) {
    
    prob_seq = seq(prob_seq[max(1, curr_min - 2)], prob_seq[min(1e3, curr_min + 2)], length = 1e3)
    curr_lb <- pbinom(q = q, size = size, prob = prob_seq, lower.tail = F, log.p = T)
    curr_min <- which.min(abs(curr_lb - log_alpha))
    
  }
  
  list(conf_int = c(prob_seq[curr_min], 1), 
       tol = abs(curr_lb[curr_min] - log_alpha) )
}


invert_pnbinom_leftsided = function(q, size, conf_level = 0.95, tol = 1e-5) {
  log_alpha <- log(1 - conf_level);
  prob_seq <- seq(1e-6, 1 - 1e-6, length = 1e3)
  # Note the difference from above: we use 1 - prob_seq because
  # pnbinom parameterizes as probability of failure, not success
  curr_lb <- pnbinom(q = q, size = size, prob = 1 - prob_seq, lower.tail = F, log.p = T)
  curr_min <- which.min(abs(curr_lb - log_alpha))
  while(abs(curr_lb[curr_min] - log_alpha) > 1e-5) {
    
    prob_seq = seq(prob_seq[max(1, curr_min - 2)], prob_seq[min(1e3, curr_min + 2)], length = 1e3)
    curr_lb <- pnbinom(q = q, size = size, prob = 1 - prob_seq, lower.tail = F, log.p = T)
    curr_min <- which.min(abs(curr_lb - log_alpha))
    
  }
  
  list(conf_int = c(prob_seq[curr_min], 1), 
       tol = abs(curr_lb[curr_min] - log_alpha) )
}


#####
##### se()
##### ----------------------
##### given a numeric vector calcuate std error :: sqrt(var(v))
#####
se <- function(v) {
  sqrt(var(v))
}

#####
##### ci()
##### ----------------------
##### given a numeric vector calculate confidence intervals of width, `width`
#####
ci <- function(mu, sd, n, width = 0.95) {
  interval <- (1 - width) / 2
  error <- (qnorm(1 - interval) * sd) / sqrt(n)
  return(c("bstrp_mu" = mu, "bstrp_err" = error))
}

#####
##### get_se()
##### ----------------------
##### given a df of bootstrapped estimates return std error
#####
get_se <- function(boot_ests) {
  apply(as.matrix(boot_ests), 1, se)  
}

#####
##### get_ci()
##### ----------------------
##### given a df of bootstrapped estimates return `width` confidence intervals
#####
get_ci <- function(boot_ests, width = 0.95) {
  apply(as.matrix(boot_ests), 1, FUN = function(ests) {
    mu <- mean(ests)
    sd <- sd(ests)
    n <- length(ests)
    return(ci(mu, sd, n, width))
  })
}

#####
##### boot_vec()
##### -------------
##### multinomial bootstrap
##### given vector of counts return n boostrap 
##### estimates of proportions
#####
boot_vec <- function(countsVec, n = 1) {
  t(normalizeBy(rmultinom(n, sum(countsVec), countsVec), rows = FALSE))
}
#####
##### pragmods_priors_boot()
##### ----------------------
##### custom pragmods wrapper to boot_priors()
##### used with mapply() to create new prior estimates
#####
pragmods_priors_boot <- function(c1, c2, c3, n = 1) {
  boot_vec(c("foil_cnts" = c1, "logical_cnts" = c2, "target_cnts" = c3), n)
}

#####
##### add_booted_priors()
##### ----------------------
##### given a df with prior counts for 'foil', 'logical' and 'target'
##### return new df with n = 1 boostrap estimates of priors
##### NOTE :: like pragmods_priors_boot this is not a generic function -
##### assumes naming convnetions established here ('foil_cnts', 'logical_cnts', 'target_cnts')
#####
add_booted_priors <- function(df) {
  res <- convertListToDF(with(df, mapply(pragmods_priors_boot, foil_cnts, logical_cnts, target_cnts, SIMPLIFY = FALSE)))
  names(res) <- c("foil_bt", "logical_bt", "target_bt")
  cbind(df, res)  
}

#####
##### run_boot_sims()
##### ---------------------
##### Run `n_sims` simulations with `alhpa`, `depth` 
##### Expects a df `d_prior_counts` that contains prior counts data
#####
run_boot_sims <- function(d_prior_counts, alpha = 1, depth = 1, n_sims = 100, verbose = TRUE) {
  ## Display run
  if (verbose) {
    cat(paste("Running ", n_sims, " simulations with alpha = ", alpha, ""))
  }
  
  ## Store n boostrapped simulations 
  bstrap_sims <- mclapply(seq(1, n_sims), FUN = function(i) {
    ## Convert to rrrsa df
    d_booted_priors <-
      rrrsa_ready_df(add_booted_priors(d_prior_counts),
                     foil_name = "foil_bt", logical_name = "logical_bt", target_name = "target_bt")
    ## Run rrrsa
    res <- plyr::ddply(d_booted_priors, .variables = c("grouper"), rsa.runDf,
                       quantityVarName = "object", semanticsVarName = "speaker.p",
                       itemVarName = "query", alpha = alpha, depth = depth,
                       priorsVarName = "priorValue", usePriorEveryRecurse = FALSE) %>%
      rowwise %>%
      mutate(tuningData = ifelse(!is.na(count), "keep", "throwout"),
             depth = depth) %>%
      filter(tuningData == "keep")
    ## Return predictions
    res$preds
  }, 
  mc.cores = detectCores())
  
  ## Convert simulations to df
  pred_sims <- convertListToDF(bstrap_sims, rows = FALSE)
  pred_sims
}


#####
##### normalizeBy()
##### -------------
##### normaze matrix by rows or cols, maintain naming
#####
normalizeBy <- function(m, rows = TRUE) {
  colNames <- colnames(m)
  rowNames <- rownames(m)
  normedM <- apply(m, MARGIN = ifelse(rows, 1, 2), rrrsa::rsa.normVec)
  colnames(normedM) <- colNames
  rownames(normedM) <- rowNames
  normedM
}

#####
##### inGlobalEnv()
##### -------------
##### boolean object presence in global env
#####
inGlobalEnv <- function(item) {
  item %in% ls(envir = .GlobalEnv)
}

#####
##### getLiteralSemantics()
##### ---------------------
##### literal semantics mapper between 
##### normalized matrices and dataframe
##### called in mutate() within dplyr chain
#####
getLiteralSemantics <- function(matrixName, object, query) {
  if (!(inGlobalEnv("simpleM") | inGlobalEnv("complexM") | inGlobalEnv("oddmanM") | inGlobalEnv("twinsM"))) {
    stop("Missing normalized matrices, please run data pre-processing block")
  }
  
  if (matrixName == "simple") {
    simpleM[as.character(object), as.character(query)]
  } else if (matrixName == "complex") {
    complexM[as.character(object), as.character(query)]
  } else if (matrixName == "oddman") {
    oddmanM[as.character(object), as.character(query)]
  } else if (matrixName == "twins") {
    twinsM[as.character(object), as.character(query)]
  } else {
    stop("Error in getLiteralSemantics()")
  }
}

#####
##### completeQueries()
##### ---------------------
##### add "query" words for each expt
##### this is necessary for running with rrrsa::runDf
#####
completeQueries <- function(d) {
  ## store data
  newD <- d
  currMatrix <- unique(as.character(d$matrix))
  
  ## meta-data
  allQueries <- c("glasses", "hat", "mustache")
  allObjects <- c("foil", "logical", "target")
  
  ## The current query
  currQuery <- unique(as.character(d$query))
  neededQueries <- setdiff(allQueries, currQuery)
  
  ## Add in new queries
  for (q in neededQueries) {
    df <- data.frame(cond = as.vector(d$cond),
                     expt = as.vector(d$expt),
                     matrix = as.vector(d$matrix),
                     prior = as.vector(d$prior),
                     query = rep(q, 3),
                     object = as.vector(d$object),
                     count = rep(NA, 3),
                     p = rep(NA, 3),
                     n = as.vector(d$n),
                     cil = rep(NA, 3), 
                     cih = rep(NA, 3),
                     priorType = as.vector(d$priorType),
                     priorValue = as.vector(d$priorValue),
                     grouper = as.vector(d$grouper))
    newD <- plyr::rbind.fill(newD, df)
    if (currMatrix == "simple") return(newD)
  }
  newD
}
# undebug(completeQueries)
# debug(completeQueries)
# completeQueries(baserate_63)

#####
##### convertListToDf()
##### -----------------
##### Convert a list where each element represents
##### data for a different grouping variable
##### useful with simulations run below
#####
convertListToDF <- function(li, rows = TRUE) {
  df <- data.frame()
  nItems <- seq(1, length(li))
  if (rows) {
    for (i in nItems) {
      df <- rbind(df, as.data.frame(li[[i]]))
    }
  } else {
    df <- data.frame(matrix(nrow = length(li[[1]]), ncol = 1))
    for (i in nItems) {
      df <- cbind(df, as.data.frame(li[[i]]))
    }
    return(df[, -1])
  }
  df
}

#####
##### rrrsa_ready_d
##### -------------
##### process data for use with rrrsa
##### NOTE :: this is not a generic fn
##### an makes certain assumptions about column naming
##### (eg. colnames -> object, expt, n, matrix)
#####
rrrsa_ready_df <- function(d, foil_name = "foil", logical_name = "logical", target_name = "target") {
  ## Initial pre-processing before adding in new cols and then adding literal semantics
  processedD <- d %>%
    ## Gather priors - we're only interested in prior for current object (need to use NSE to make
    ## compatible with boostrapped priors)
    gather_("priorType", "priorValue", c(foil_name, logical_name, target_name))
  processedD <- processedD %>%
    ## Filter out any mismatches between current object and priorType
    ## Do rowwise with grep so that we can use with boostrap naming conventions
    rowwise %>%
    filter(grepl(object, priorType)) %>%
    rowwise %>%
    ## Grouping variable concatenates experiment type and sample size (expt-n)
    mutate(grouper = paste0(as.character(expt), "-", as.character(n)))
  
  fullData <- plyr::ddply(processedD, .variables = ("grouper"), .fun = completeQueries) %>%
    rowwise %>%
    ## Each row gets a unique literal semantics from getLiteraSemantics() helper
    mutate(speaker.p = getLiteralSemantics(matrix, query = query, object = object)) %>%
    arrange(expt, n)
  
  fullData
}

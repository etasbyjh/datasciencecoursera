
get_path <- function(dir, i){
    idd <- sprintf("%03d", i)
    paste(dir,"/", idd, ".csv", sep="")
}

pollutantmean <- function(dir, pollutant, id = 1:332){
    collected <- data.frame()
    for(i in id){
        data_id <- read.csv(get_path(dir, i))
        collected <- rbind(collected, data_id)
    }
    if(pollutant == "sulfate"){
        mean(collected$sulfate, na.rm = TRUE)
    }
    else{
        mean(collected$nitrate, na.rm = TRUE)
    }
}

complete <- function(dir, id = 1:332){
    collected <- data.frame()
    for(i in id){
        data_id <- read.csv(get_path(dir, i))
        com <- complete.cases(data_id)
        com_true <- com[com == TRUE]
        com_true_len <- length(com_true)
        collected = rbind(collected, data.frame("id" = i, "nobs" = com_true_len))
    }
    collected
}

corr <- function(dir, threshold = 0){
    corr_vec <- numeric(0)
    for(i in 1:332){
        data_id <- read.csv(get_path(dir, i))
        com <- complete.cases(data_id)
        com_true <- com[com == TRUE]
        com_true_len <- length(com_true)
        if(com_true_len > threshold){
            corr_val <- cor(data_id$sulfate[com == TRUE], data_id$nitrate[com == TRUE], use = "pairwise.complete.obs")
            corr_vec <- c(corr_vec, corr_val)
        }
    }
    corr_vec
}
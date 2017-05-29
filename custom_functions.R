
get_character_index <- function(x) match(x, sort(unique(x)))


read_claim_datafile <- function(path, progress = FALSE) {
    lob <- path %>% basename
    lob <- gsub("_pos\\.csv", "", lob)

    data_tbl <- read_csv(path, progress = progress)

    col_names <- data_tbl %>% names %>% tolower
    col_names <- gsub("_..?", "", col_names)

    names(data_tbl) <- col_names

    data_tbl <- data_tbl %>% mutate(lob = lob)

    return(data_tbl)
}


create_stanfit <- function(stan_model, usedata_tbl, model_id
                          ,chain_count = 8, iter_count = 500
                          ,stan_seed = stan_seed) {
    cohort_maxtime <- usedata_tbl %>%
        group_by(acc_year) %>%
        summarise(maxtime = max(dev_lag)) %>%
        arrange(acc_year) %>%
        .[[2]]

    cohort_premium <- usedata_tbl %>%
        group_by(acc_year) %>%
        summarise(premium = unique(premium)) %>%
        .[[2]]

    t_values <- usedata_tbl %>%
        select(dev_lag) %>%
        arrange(dev_lag) %>%
        unique %>%
        .[[1]]

    standata_lst <- list(
        growthmodel_id = model_id   # Use weibull rather than loglogistic
       ,n_data         = usedata_tbl %>% nrow
       ,n_time         = usedata_tbl %>% select(dev_lag)  %>% unique %>% nrow
       ,n_cohort       = usedata_tbl %>% select(acc_year) %>% unique %>% nrow
       ,cohort_id      = get_character_index(usedata_tbl$acc_year)
       ,cohort_maxtime = cohort_maxtime
       ,t_value        = t_values
       ,t_idx          = get_character_index(usedata_tbl$dev_lag)
       ,premium        = cohort_premium
       ,loss           = usedata_tbl$cum_loss
    )

    model_stanfit <- sampling(
        object = model_sislob_stanmodel
       ,data   = standata_lst
       ,iter   = iter_count
       ,chains = chain_count
       ,seed   = stan_seed
    )

    return(list(
        standata_lst = standata_lst
       ,stanfit      = model_stanfit))
}

ID <- "test"
NUM_ITER <- 1400
P_WEIGHT <- c(1 , 1.028, 1.023, 1.018)


LAG_list <- list(
  c( 7, 28),
  c( 3, 7 , 14),
  c( 3, 14, 28)
)


WIN_list <- list(
  c( 7, 28),
  c( 3, 7 , 14),
  c( 3, 14, 28)
)






p2 <- list( objective = "tweedie",
            boosting_type = "gbdt",
            tweedie_variance_power = 1.1,
            metric = "rmse",
            subsample = 0.5,
            subsample_freq = 1, 
            learning_rate = 0.03, 
            num_leaves = 2047,
            min_data_in_leaf = 4098,
            feature_fraction = 0.5,
            max_bin = 100, 
            n_estimators = 1000,
            boost_from_average= F,
            verbose= -1,
            verbose_eval = F
)  

library(data.table)
library(lightgbm)
library(ggplot2)
library(tidyverse)

# 每次都要相同的
# 決定要拿哪些當作train
# predict 哪個區間

# 把train到prdict的過程寫成function
# argument 
# 用哪些 feature, all or top?
# 哪些train, predict


free <- function() invisible(gc()) 
one_hot_trans <- function(board, data){
  res <- data %>% apply(.,1,function(x){as.numeric(board %in% x) }) %>% as_tibble() %>% t() %>% as_tibble()
  colnames(res) <- board
  res
}

log_n_mean <- function(dt, lag, win, cols){
  dt <- dt %>% select('id',"item_id", "dept_id", "cat_id", "store_id" ,"state_id" ,cols)
  dt <- gather(dt, cols, key = "d", value = "sales") #%>% drop_na()
  lag_cols <- paste0("lag_", lag) # lag columns names
  roll_cols <- paste0("rmean_", t(outer(lag, win, paste, sep="_"))) # rolling features columns names
  
  res <- data.table(dt)[, (lag_cols) := shift(.SD, lag), by = id, .SDcols = "sales"] # add lag vectors
  res <- data.table(res)[, (roll_cols) := frollmean(.SD, win, na.rm = TRUE), by = id, .SDcols = lag_cols] # rolling features on lag_cols
  return(res)
}
Merge_data <- function(DAT_TDL, CAL, PRICES, DISASTER){
  DAT_TDL %>% as_tibble() %>% 
    left_join(., CAL, by = c("d" = "d" )) %>%
    left_join(., PRICES, by = c("item_id" = "item_id",
                                "store_id" = "store_id",
                                "wm_yr_wk" = "wm_yr_wk" )) %>% 
    select(-wm_yr_wk) %>% # na.omit() %>% # remove NA in price
    left_join(., DISASTER, by = c( "date" = "date",
                                   "state_id" = "state_id")) %>% 
    replace_na(.,list( n_disaster =0,
                       is_Fire = 0,
                       is_Snow = 0,
                       is_Tsunami = 0,
                       is_Severe_Storm = 0,
                       is_Earthquake = 0,
                       is_Flood = 0,
                       is_FM = 0,
                       is_DR = 0,
                       is_EM = 0,
                       ih_program_declared = 0,
                       ia_program_declared = 0,
                       pa_program_declared = 0,
                       hm_program_declared = 0,
                       min_disaster_day_n = 0,
                       max_disaster_day_n = 0
    ) )%>% mutate( dept_id = as.integer(factor(dept_id)),
                   cat_id = as.integer(factor(cat_id)),
                   store_id = as.integer(factor(store_id)),
                   state_id = as.integer(factor(state_id)),
                   item_id = as.integer(factor(item_id))  )
}
FILL_NA <- function(x){  i <- min(which(x>0))
x[1:i-1] <- NA
x  }

uuu = 1
# setwd('/home/rstudio/M5/')

for ( uuu in 1:3){
  
  
  # load data
  
  raw_data <- fread("/home/rstudio/M5/sales_train_evaluation.csv") %>% as_tibble()
  cal <- fread("/home/rstudio/M5/calendar_with_nba.csv") %>% as_tibble()
  prices <- fread("/home/rstudio/M5/sell_prices.csv") %>% as_tibble()
  disaster <- fread("/home/rstudio/M5/disaster.csv") %>% as_tibble()
  
  # 所有資料到1941
  # 最後要交的是1942-1969
  # train的過程中1900後的資料從來沒進model, 用來比較最後幾個model的間的優劣確定好架構後在把1900-1941的資料加進去重新train一個
  
  # one-hot trans
  even_name_trans_tbl <-
    one_hot_trans( board = c(cal$event_name_1,cal$event_name_2) %>% unique() %>% .[-1],
                   data = cal %>% select(event_name_1,event_name_2) )
  even_type_trans_tbl <-
    one_hot_trans( board = c(cal$event_type_1,cal$event_type_2) %>% unique() %>% .[-1],
                   data = cal %>% select(event_type_1,event_type_2)  )
  free()
  
  cal <-
    bind_cols(
      cal %>% select(-event_type_1, -event_type_2, -event_name_1, -event_name_2),
      even_name_trans_tbl,
      even_type_trans_tbl
    ) %>%
    mutate( wday = wday(date), # time features
            mday = mday(date),
            week = week(date),
            month = month(date),
            year = year(date)
    ) %>% select(-weekday)
  
  free()
  
  prices
  dt <- raw_data
  
  id_cols <- c("id","item_id","dept_id","cat_id","store_id","state_id")
  ############### 實際尚未進去model的資料範圍
  # cols <- paste("d_",1:1941, sep = "")
  cols <- paste("d_",1:1941, sep = "")
  free()
  
  # rm(raw_data)
  
  fill_na_tbl <- dt %>% 
    select(cols) %>%
    apply(.,1, FILL_NA  ) %>% 
    t() %>% as_tibble()
  
  dt[,cols] <- fill_na_tbl
  
  dat_tbl <- log_n_mean( dt = dt,
                         lag =  LAG_list[[uuu]] ,
                         win =  WIN_list[[uuu]] ,
                         cols = cols )
  
  free()
  all <- dat_tbl %>% na.omit() 
  free()
  
  rm(dat_tbl)
  all <- all%>% Merge_data(., CAL = cal, PRICES = prices, DISASTER = disaster)
  free()
  all$store_id %>% unique()
  # colnames(all) <- colnames(all) %>% gsub(.,pattern = " ",replacement = "_")
  y = "sales"
  x = setdiff(colnames(all), c(y, "date", "id", "d"))
  x = x[ ! x %in% c("store_id")]
  # cats = c("item_id", "dept_id", "cat_id", "store_id", "state_id" ,"wday", "mday", "week", "month", "year")
  cats = c("item_id", "dept_id", "cat_id", "state_id" ,"wday", "mday", "week", "month", "year")
  free()
  store_ids <- all$store_id %>% unique()
  for ( storr in 1:length(store_ids) ){
    new_all <-  all %>% filter( store_id == store_ids[storr] )
    free()
    set.seed(1122)
    randomID <- sample(nrow(new_all),nrow(new_all)/5)
    
    # Matrix
    valid = lgb.Dataset(data.matrix(new_all[randomID ,x]), label = new_all[[y]][ randomID ], categorical_feature = cats) 
    free()
    train = lgb.Dataset(data.matrix(new_all[ -randomID , x]), label = new_all[[y]][ -randomID ], categorical_feature = cats)
    free()
    
    fit = lgb.train(
      data = train, 
      valid = list(val = valid), 
      num_iterations = NUM_ITER,
      early_stopping_rounds = 600,
      eval_freq = 200,
      params = p2
    )
    free()
    rm(valid, train)
    
    Make_predict <- function( FIT, Feature, LAG, WIN, p_wei){
      val_cols <- paste("d_",1942:1969, sep = "")
      # val_cols <- paste("d_",1901:1941, sep = "")
      tmp_dt  <- dt %>% filter(id %in% new_all$id)
      pre_tbl <- matrix(0, nrow = nrow(tmp_dt),ncol = length(val_cols))
      free()
      colnames(pre_tbl) <- val_cols
      #raw_data <- fread("/home/rstudio/sales_train_validation.csv") %>% as_tibble()
      pre_tbl <- bind_cols(tmp_dt %>% select(id_cols,paste("d_",1900:1941, sep = "")) , pre_tbl %>% as_tibble)
      # pre_tbl <- bind_cols(dt %>% select(id_cols,paste("d_",1700:1900, sep = "")), pre_tbl %>% as_tibble)
      free()
      final_pre <- pre_tbl %>% select(id, val_cols)
      # 貼上換cols的範圍跑下面3個流程
      cols <- colnames(pre_tbl)
      for(  pp in 1942:1969 ){
        # for(  pp in 1901:1941 ){
        predat_tbl <-
          log_n_mean(dt = pre_tbl,
                     lag =  LAG ,
                     win =  WIN ,
                     cols = paste("d", (pp-30):pp, sep = "_")
          )
        free()
        
        predat_tbl <- predat_tbl %>% filter( d == paste("d_",pp,sep = ""))
        pre_all    <- predat_tbl %>% Merge_data(., CAL = cal, PRICES = prices, DISASTER = disaster)
        #colnames(pre_all) <- colnames(pre_all) %>% gsub(.,pattern = " ",replacement = "_")
        pre_res <- predict(FIT, pre_all %>% select(Feature) %>% data.matrix)
        col_m <- paste("d_",pp,sep = "")
        pre_tbl[,colnames(pre_tbl)==col_m] <- pre_res*p_wei
        final_pre[,colnames(final_pre)==col_m]  <- pre_res
        free()
        #print(pp)
        
      }
      #final_predict <- pre_tbl %>% select(id, val_cols)
      free()
      final_pre
    }

    free()
    pred_res_f_w1 <- Make_predict(FIT = fit, Feature = x, LAG = LAG_list[[uuu]], WIN = WIN_list[[uuu]], p_wei = P_WEIGHT[2])
    pred_res_f_w2 <- Make_predict(FIT = fit, Feature = x, LAG = LAG_list[[uuu]], WIN = WIN_list[[uuu]], p_wei = P_WEIGHT[3])
    # free()
    # pred_res_w1 <- Make_predict_for_nn(FIT = fit, Feature = x, LAG = LAG_list[[uuu]], WIN = WIN_list[[uuu]], p_wei = P_WEIGHT[1])
    # pred_res_w2 <- Make_predict_for_nn(FIT = fit, Feature = x, LAG = LAG_list[[uuu]], WIN = WIN_list[[uuu]], p_wei = P_WEIGHT[2])
    # free()
    
    # write.csv(pred_res_w1, file = paste("sep_store", storr , "roll" , uuu ,"lite_data_w1_p2_rmse_pre_res.csv" , sep = "_"))
    # write.csv(pred_res_w2, file = paste("sep_store", storr , "roll" , uuu , "lite_data_w2_p2_rmse_pre_res.csv" , sep = "_"))
    # free()
    write.csv(pred_res_f_w1, file = paste("sep_store", storr , "roll", uuu , "lite_data_w2_final_rmse_p2_pre_res.csv" , sep = "_"))
    free()
    write.csv(pred_res_f_w2, file = paste("sep_store", storr , "roll", uuu , "lite_data_w3_final_rmse_p2_pre_res.csv" , sep = "_"))
    print( storr, uuu )
  }
  
  
  
  
  
######################################################
  
  
  
  
  cols <- paste("d_",1:1913, sep = "")
  
  free()
  # rm(raw_data)
  
  fill_na_tbl <- dt %>% 
    select(cols) %>%
    apply(.,1, FILL_NA  ) %>% 
    t() %>% as_tibble()
  
  dt[,cols] <- fill_na_tbl
  
  dat_tbl <- log_n_mean( dt = dt,
                         lag =  LAG_list[[uuu]] ,
                         win =  WIN_list[[uuu]] ,
                         cols = cols )
  
  free()
  all <- dat_tbl %>% na.omit() 
  free()
  
  rm(dat_tbl)
  all <- all%>% Merge_data(., CAL = cal, PRICES = prices, DISASTER = disaster)
  free()
  all$store_id %>% unique()
  # colnames(all) <- colnames(all) %>% gsub(.,pattern = " ",replacement = "_")
  y = "sales"
  x = setdiff(colnames(all), c(y, "date", "id", "d"))
  x = x[ ! x %in% c("store_id")]
  # cats = c("item_id", "dept_id", "cat_id", "store_id", "state_id" ,"wday", "mday", "week", "month", "year")
  cats = c("item_id", "dept_id", "cat_id", "state_id" ,"wday", "mday", "week", "month", "year")
  store_ids <- all$store_id %>% unique()
  for ( storr in 1:length(store_ids) ){
    new_all <-  all %>% filter( store_id == store_ids[storr] )
    free()
    set.seed(1122)
    free()
    randomID <- sample(nrow(new_all),nrow(new_all)/5)
    
    # Matrix
    valid = lgb.Dataset(data.matrix(new_all[randomID ,x]), label = new_all[[y]][ randomID ], categorical_feature = cats) 
    free()
    train = lgb.Dataset(data.matrix(new_all[ -randomID , x]), label = new_all[[y]][ -randomID ], categorical_feature = cats)
    free()
    
    fit = lgb.train(
      data = train, 
      valid = list(val = valid), 
      num_iterations = NUM_ITER,
      early_stopping_rounds = 600,
      eval_freq = 200,
      params = p2
    )
    
    rm(valid, train)
    free()
    Make_predict_for_nn <- function( FIT, Feature, LAG, WIN, p_wei){
      val_cols <- paste("d_",1914:1941, sep = "")
      # val_cols <- paste("d_",1901:1941, sep = "")
      tmp_dt  <- dt %>% filter(id %in% new_all$id)
      pre_tbl <- matrix(0, nrow = nrow(tmp_dt),ncol = length(val_cols))
      colnames(pre_tbl) <- val_cols
      #raw_data <- fread("/home/rstudio/sales_train_validation.csv") %>% as_tibble()
      pre_tbl <- bind_cols(tmp_dt %>% select(id_cols,paste("d_",1850:1913, sep = "")) , pre_tbl %>% as_tibble)
      # pre_tbl <- bind_cols(dt %>% select(id_cols,paste("d_",1700:1900, sep = "")), pre_tbl %>% as_tibble)
      free()
      final_pre <- pre_tbl %>% select(id, val_cols)
      # 貼上換cols的範圍跑下面3個流程
      cols <- colnames(pre_tbl)
      for(  pp in 1914:1941 ){
        # for(  pp in 1901:1941 ){
        predat_tbl <-
          log_n_mean(dt = pre_tbl,
                     lag =  LAG ,
                     win =  WIN ,
                     cols = paste("d", (pp-30):pp, sep = "_")
          )
        free()
        
        predat_tbl <- predat_tbl %>% filter( d == paste("d_",pp,sep = ""))
        pre_all    <- predat_tbl %>% Merge_data(., CAL = cal, PRICES = prices, DISASTER = disaster)
        #colnames(pre_all) <- colnames(pre_all) %>% gsub(.,pattern = " ",replacement = "_")
        pre_res <- predict(FIT, pre_all %>% select(Feature) %>% data.matrix)
        col_m <- paste("d_",pp,sep = "")
        pre_tbl[,colnames(pre_tbl)==col_m] <- pre_res*p_wei
        final_pre[,colnames(final_pre)==col_m]  <- pre_res
        free()
        #print(pp)
        
      }
      #final_predict <- pre_tbl %>% select(id, val_cols)
      final_pre
      free()
    }
    
    
    # pred_res_f_w1 <- Make_predict(FIT = fit, Feature = x, LAG = LAG_list[[uuu]], WIN = WIN_list[[uuu]], p_wei = P_WEIGHT[2])
    # pred_res_f_w2 <- Make_predict(FIT = fit, Feature = x, LAG = LAG_list[[uuu]], WIN = WIN_list[[uuu]], p_wei = P_WEIGHT[3])
    # free()
    pred_res_w1 <- Make_predict_for_nn(FIT = fit, Feature = x, LAG = LAG_list[[uuu]], WIN = WIN_list[[uuu]], p_wei = P_WEIGHT[1])
    pred_res_w2 <- Make_predict_for_nn(FIT = fit, Feature = x, LAG = LAG_list[[uuu]], WIN = WIN_list[[uuu]], p_wei = P_WEIGHT[2])
    free()
    
    write.csv(pred_res_w1, file = paste("sep_store", storr , "roll" , uuu ,"lite_data_w2_p2_rmse_pre_res.csv" , sep = "_"))
    write.csv(pred_res_w2, file = paste("sep_store", storr , "roll" , uuu , "lite_data_w3_p2_rmse_pre_res.csv" , sep = "_"))
    free()
    # write.csv(pred_res_f_w1, file = paste("sep_store", storr , "roll", uuu , "lite_data_w1_final_rmse_p2_pre_res.csv" , sep = "_"))
    # write.csv(pred_res_f_w2, file = paste("sep_store", storr , "roll", uuu , "lite_data_w2_final_rmse_p2_pre_res.csv" , sep = "_"))
    print( storr, uuu )
  }
  
  
  
  
  
  
  
  

  free()
  
}

#測試沒偷看答案的train結果如何

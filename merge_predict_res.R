
ans  <- fread("/home/rstudio/M5/sales_train_evaluation.csv")
id_g <- fread("/home/rstudio/M5/M5_id_group.csv")
cols <- paste("d_", 1901:1941, sep = "")
g1 <- id_g$id[id_g$g==1]
g1 %>% length()

ans <- ans %>% filter( id %in% g1) %>% select(id, cols)
emp_an <-  matrix(0, nrow = length(g1), ncol = length(1942:1969))
colnames(emp_an) <- paste("d_", 1942:1969, sep = "")
emp_an <- emp_an %>%  as_tibble() %>% mutate(id = ans$id)

trans <- function(X){ fread(X) %>% as_tibble() %>% mutate(V1 = ans$id) %>% gather( ., d ,value, -V1 ) }
long_ans <- gather( ans %>% filter( id %in% g1) %>% select(id, cols), d ,value, -id )
long_emp_an <- gather( emp_an %>% filter( id %in% g1) %>% select(id,  paste("d_", 1942:1969, sep = "")), d ,value, -id )
cal <- fread("/home/rstudio/M5/calendar_with_nba.csv") %>% as_tibble()
long_ans <- left_join(long_ans, cal %>% select(d,wday))

f1 <- "/home/rstudio/M5/G1_res/1_2.53103325070378_g1_pre_res.csv"
f2 <- "/home/rstudio/M5/G1_res/2_2.53677941892748_g1_pre_res.csv"
f3 <- "/home/rstudio/M5/G1_res/3_2.59897481748168_g1_lite_pred_res.csv"
f4 <- "/home/rstudio/M5/G1_res/4_2.54843479724535_g1_lite_pred_res.csv"
f5 <- "/home/rstudio/M5/G1_res/5_2.59700444084134_g1_lite_pred_res.csv"
f6 <- "/home/rstudio/M5/G1_res/6_2.59700444084134_g1_lite_pred_res.csv"
f7 <- "/home/rstudio/M5/G1_res/7_2.59479082140884_g1_pre_res.csv"
f8 <- "/home/rstudio/M5/G1_res/8_2.52846337676867_g1_pre_res.csv"


# f1 <- "/home/rstudio/M5/G1_res/1_g1_final_pre_res.csv"
# f2 <- "/home/rstudio/M5/G1_res/2_g1_final_pre_res.csv"
# f3 <- "/home/rstudio/M5/G1_res/3_g1_final_lite_pred_res.csv"
# f4 <- "/home/rstudio/M5/G1_res/4_g1_final_lite_pred_res.csv"
# f5 <- "/home/rstudio/M5/G1_res/5_g1_final_lite_pred_res.csv"
# f6 <- "/home/rstudio/M5/G1_res/6_g1_final_lite_pred_res.csv"
# f7 <- "/home/rstudio/M5/G1_res/7_g1_final_pre_res.csv"
# f8 <- "/home/rstudio/M5/G1_res/8_g1_final_pre_res.csv"



merge_data <- 
full_join(long_ans,  trans(X = f1) ,   by = c('id'= "V1", "d" = "d")) %>% 
  full_join(., trans(X = f2),   by = c('id'= "V1", "d" = "d")) %>% 
  full_join(., trans(X = f3),   by = c('id'= "V1", "d" = "d")) %>% 
  full_join(., trans(X = f4),   by = c('id'= "V1", "d" = "d")) %>% 
  full_join(., trans(X = f5),   by = c('id'= "V1", "d" = "d")) %>% 
  full_join(., trans(X = f6),   by = c('id'= "V1", "d" = "d")) %>% 
  full_join(., trans(X = f7),   by = c('id'= "V1", "d" = "d")) %>% 
  full_join(., trans(X = f8),   by = c('id'= "V1", "d" = "d"))



merge_data %>% head()
colnames(merge_data)[3:12] <- c('ans', "wday", 'n1','n2','n3','n4','n5','n6','n7','n8')
merge_data %>% head()


# tmp <- merge_data %>% mutate(d = as.integer(factor(d))) %>% mutate(id = as.integer(factor(id)))
# tmp$id[tmp$id == max(tmp$id)] = 0 

tmp <- merge_data %>% mutate(d = as.integer(factor(d)))

id_tbl <- tibble(
  id = tmp$id %>% unique(),
  tr_id = c(0:(length(tmp$id %>% unique())-1)) 
)

f_tmp <- full_join(tmp, id_tbl)
colnames(f_tmp)[c(3,5:12)] <- c("ans", 'n1','n2','n3','n4','n5','n6','n7','n8')
f_tmp %>% filter(tr_id == 15)
f_tmp %>% filter(wday == 7 )
f_tmp$wday[f_tmp$wday==7] <- 0
f_tmp$wday %>% unique()
f_tmp  %>% filter( d %in% c(1:20)) %>% .$wday %>% unique()
write.csv( f_tmp    , file = "G1_final_merge_data.csv")
write.csv( f_tmp  %>% filter( d %in% c(1:20))   , file = "G1_merge_data_train.csv")
write.csv( f_tmp  %>% filter( d %in% c(21:41))  , file = "G1_merge_data_val.csv")
write.csv( f_tmp    , file = "G1_merge_data.csv")
merge_data %>% mutate(id = as.integer(factor(id))) %>% filter( d %in% paste("d_", 1901:1931, sep = "")) %>% select(-d)
merge_data %>% mutate(id = as.integer(factor(id))) %>% filter( d %in% paste("d_", 1932:1941, sep = "")) %>% select(-d)

write.csv(merge_data %>% mutate(id = as.integer(factor(id))) %>% filter( d %in% paste("d_", 1901:1931, sep = "")) %>% select(-d) , file = "merge_data_train.csv")
write.csv(merge_data %>% mutate(id = as.integer(factor(id))) %>% filter( d %in% paste("d_", 1932:1941, sep = "")) %>% select(-d) , file = "merge_data_val.csv")

 merge_data %>% mutate(d = as.integer(factor(d))) 
 cbind( merge_data$d,
       as.integer(factor(merge_data$d))) %>% View

 
 merge_data %>% mutate(d = as.integer(factor(d))) %>% filter( d %in% c(1:20))   %>% select( -id ) %>% mutate(resm = (ans - n1 )^2) %>% sum(.$resm)/609800
 merge_data %>% mutate(d = as.integer(factor(d))) %>% filter( d %in% c(21:41))   %>% select( -id ) %>% mutate(resm = (ans - n1 )^2)%>% sum(.$resm)/640290
 
 
 
sum((merge_data$ans-yy)^2)/length(yy)

ttt <- mm %>% as_tibble() %>% mutate(final_pre = ff$X0) %>% mutate(re = (ans-final_pre)^2 ) %>% filter(tr_id == 56) 
ttt
 

yy <-  
merge_data[,-1:-3] %>% apply(.,1,mean)
 
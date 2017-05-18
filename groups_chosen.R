get_groups <- function(){
  sham1 <- c('Sham1.r1', 'Sham1.r2', 'Sham1.r3')
  sham2 <- c('Sham2.r1', 'Sham2.r2', 'Sham2.r3') 
  sham3 <- c('Sham3.r1', 'Sham3.r2', 'Sham3.r3')
  hour4 <- c('T00d4h.r2', 'T00d4h.r3', 'T00d4h.r4')
  day1  <- c('T01d.r1', 'T01d.r2', 'T01d.r4')
  day3  <- c('T03d.r1', 'T03d.r2', 'T03d.r3')
  day7  <- c('T07d.r1', 'T07d.r2', 'T07d.r3')
  day14 <- c('T14d.r1', 'T14d.r2', 'T14d.r3')
  day90 <- c('T90d.r1', 'T90d.r2', 'T90d.r3')
  
  ##################################################################################
  columns <- c(day1, day3, sham3)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST')))
  day1_day3__sham3 <- list(columns=columns, f=f, name1='day1_day3', name2='sham3')
  ##################################################################################
  columns <- c(day1, day3, sham3, day90)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST',
    'FIRST','FIRST','FIRST')))
  day1_day3__sham3_day90 <- list(columns=columns, f=f, name1='day1_day3', name2='sham3_day90')
  ####################################################################################################################################################################
  columns <- c(hour4, day1, day3, sham3)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST')))
  hour4_day1_day3__sham3 <- list(columns=columns, f=f,name1='hour4_day1_day3', name2='sham3')
  ####################################################################################################################################################################
  columns <- c(hour4, day1, day3, sham3, day90)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST',
    'FIRST','FIRST','FIRST')))
  hour4_day1_day3__sham3_day90 <- list(columns=columns, f=f, name1='hour4_day1_day3', name2='sham3_day90')
  ####################################################################################################################################################################
  columns <- c(day7, day14,sham3)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST')))
  day7_day14__sham3 <- list(columns=columns, f=f, name1='day7_day14', name2='sham3')
  ####################################################################################################################################################################
  columns <- c(day7, day14, sham3, day90)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST',
    'FIRST','FIRST','FIRST')))
  day7_day14__sham3_day90 <- list(columns=columns, f=f, name1='day7_day14', name2='sham3_day90')
  ####################################################################################################################################################################
  columns <- c(hour4, day1, day3, day7, day14,sham3)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST')))
  hour4_day1_day3_day7_day14__sham3 <- list(columns=columns, f=f,name1='hour4_day1_day3_day7_day14', name2='sham3')
  ####################################################################################################################################################################
  columns <- c(hour4, day1, day3, day7, day14,sham3, day90)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST',
    'FIRST','FIRST','FIRST')))
  hour4_day1_day3_day7_day14__sham3_day90 <- list(columns=columns, f=f, name1='hour4_day1_day3_day7_day14',name2='sham3_day90')
  ####################################################################################################################################################################
  columns <- c(day90, hour4, day1, day3, day7, day14,sham3)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST')))
  day90_hour4_day1_day3_day7_day14__sham3 <- list(columns=columns, f=f, name1='hour4_day1_day3_day7_day14_day90', name2='sham3')
  ####################################################################################################################################################################
  columns <- c(hour4, sham3)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST')))
  hour4__sham3 <- list(columns=columns, f=f,name1='hour4', name2='sham3')
  ####################################################################################################################################################################
  columns <- c(day1, sham3)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST')))
  day1__sham3 <- list(columns=columns, f=f,name1='day1', name2='sham3')
  ####################################################################################################################################################################
  columns <- c(day3, sham3)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST')))
  day3__sham3 <- list(columns=columns, f=f,name1='day3', name2='sham3')
  ####################################################################################################################################################################
  columns <- c(day7, sham3)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST')))
  day7__sham3 <- list(columns=columns, f=f,name1='day7', name2='sham3')
  ####################################################################################################################################################################
  columns <- c(day14, sham3)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST')))
  day14__sham3 <- list(columns=columns, f=f,name1='day14', name2='sham3')
  ####################################################################################################################################################################
  columns <- c(day90, sham3)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST')))
  day90__sham3 <- list(columns=columns, f=f,name1='day90', name2='sham3')
  ####################################################################################################################################################################
  columns <- c(day7, day14, hour4)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST')))
  day7_day14__hour4 <- list(columns=columns, f=f,name1='day7_day14', name2='hour4')
  ####################################################################################################################################################################
  columns <- c(day14, hour4)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST')))
  day14__hour4 <- list(columns=columns, f=f,name1='day14', name2='hour4')
  ####################################################################################################################################################################
  columns <- c(day7, hour4)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST')))
  day7__hour4 <- list(columns=columns, f=f,name1='day7', name2='hour4')
  ####################################################################################################################################################################
  columns <- c(day1, hour4)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST')))
  day1__hour4 <- list(columns=columns, f=f,name1='day1', name2='hour4')
  ####################################################################################################################################################################
  columns <- c(day3, hour4)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST')))
  day3__hour4 <- list(columns=columns, f=f,name1='day3', name2='hour4')
  ####################################################################################################################################################################
  columns <- c(day90, hour4)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST')))
  day90__hour4 <- list(columns=columns, f=f,name1='day90', name2='hour4')
  ####################################################################################################################################################################
  columns <- c(hour4, day90)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST')))
  hour4__day90 <- list(columns=columns, f=f,name1='hour4', name2='day90')
  ####################################################################################################################################################################
  columns <- c(day1, day3, hour4)
  f <- factor(as.character(c(
    'SECOND','SECOND','SECOND',
    'SECOND','SECOND','SECOND',
    'FIRST','FIRST','FIRST')))
  day1_day3__hour4 <- list(columns=columns, f=f, name1='day1_day3', name2='hour4')
  ####################################################################################################################################################################

  
  #### seven signatures chosen after talking with sophie
  data_list <- list('chosen_signatures'=list(day1_day3__sham3, 
                    day7_day14__sham3,
                    day14__sham3,
                    day7__sham3, 
                    day3__sham3, 
                    day1__sham3, 
                    day7_day14__hour4),
              'chosen_signatures_vs_4h'=list(day1_day3__hour4, 
                  day7_day14__hour4,
                  day90__hour4,
                  day14__hour4,
                  day7__hour4, 
                  day3__hour4, 
                  day1__hour4),
              'combined' = list(day1_day3__sham3, 
                  day1_day3__hour4,
                  day7_day14__sham3,
                  day7_day14__hour4,
                  day90__hour4,
                  day14__sham3,
                  day14__hour4,
                  day7__sham3, 
                  day7__hour4,
                  day3__sham3,
                  day3__hour4,
                  day1__sham3, 
                  day1__hour4
                  ))
}

signature_names <- c('day1_day3__sham3','day1_day3__sham3_strict_15fold','day1_day3__sham3_strict',
                      'day1_day3__sham3_day90', 'day1_day3__sham3_day90_strict_15fold', 'day1_day3__sham3_day90_strict',
                      'hour4_day1_day3__sham3', 'hour4_day1_day3__sham3_strict_15fold', 'hour4_day1_day3__sham3_strict',
                      'hour4_day1_day3__sham3_day90', 'hour4_day1_day3__sham3_day90_strict_15fold', 'hour4_day1_day3__sham3_day90_strict',                   
                      'day7_day14__sham3', 'day7_day14__sham3_strict_15fold', 'day7_day14__sham3_strict',     
                      'day7_day14__sham3_day90', 'day7_day14__sham3_day90_strict_15fold', 'day7_day14__sham3_day90_strict',                                 
                      'hour4_day1_day3_day7_day14__sham3', 'hour4_day1_day3_day7_day14__sham3_strict_15fold',  'hour4_day1_day3_day7_day14__sham3_strict',             
                      'hour4_day1_day3_day7_day14__sham3_day90', 'hour4_day1_day3_day7_day14__sham3_day90_strict_15fold', 'hour4_day1_day3_day7_day14__sham3_day90_strict',        
                      'hour4_day1_day3_day7_day14_day90__sham3', 'hour4_day1_day3_day7_day14_day90__sham3_strict_15fold', 'hour4_day1_day3_day7_day14_day90__sham3_strict',
                      'hour4__sham3', 'hour4__sham3_strict', 'hour4__sham3_strict_15fold',
                      'day1__sham3', 'day1__sham3_strict', 'day1__sham3_strict_15fold',
                      'day3__sham3', 'day3__sham3_strict', 'day3__sham3_strict_15fold',
                      'day7__sham3', 'day7__sham3_strict', 'day7__sham3_strict_15fold',
                      'day14__sham3', 'day14__sham3_strict', 'day14__sham3_strict_15fold',
                      'day90__sham3', 'day90__sham3_strict', 'day90__sham3_strict_15fold',
                      'day7__hour4', 'day7__hour4_strict', 'day7__hour4_strict_15fold',
                      'day14__hour4', 'day14__hour4_strict', 'day14__hour4_15fold',
                      'day7_day14__hour4', 'day7_day14__hour4_strict', 'day7_day14__hour4_strict_15fold',
                      'hour4__day90', 'hour4__day90_strict', 'hour4__day90_strict_15fold',
                      'day1_day3__hour4_strict',
                      'day90__hour4_strict',
                      'day3__hour4_strict',
                      'day1__hour4_strict')
signature_names_shortcut <- c('d1_d3__sham3','d1_d3__sham3_strict_15fold', 'd1_d3__sham3_strict',
                              'd1_d3__sham3_d90', 'd1_d3__sham3_d90_strict_15fold', 'd1_d3__sham3_d90_strict',        
                              'h4_d1_d3__sham3', 'h4_d1_d3__sham3_strict_15fold', 'h4_d1_d3__sham3_strict',
                              'h4_d1_d3__sham3_d90', 'h4_d1_d3__sham3_d90_strict_15fold', 'h4_d1_d3__sham3_d90_strict', 
                              'd7_d14__sham3', 'd7_d14__sham3_strict_15fold', 'd7_d14__sham3_strict',     
                              'd7_d14__sham3_d90', 'd7_d14__sham3_d90_strict_15fold', 'd7_d14__sham3_d90_strict',               
                              'h4_d1_d3_d7_d14__sham3', 'h4_d1_d3_d7_d14__sham3_strict_15fold', 'h4_d1_d3_d7_d14__sham3_strict',       
                              'h4_d1_d3_d7_d14__sham3_d90', 'h4_d1_d3_d7_d14__sham3_d90_strict_15fold', 'h4_d1_d3_d7_d14__sham3_d90_strict',        
                              'h4_d1_d3_d7_d14_d90__sham3', 'h4_d1_d3_d7_d14_d90__sham3_strict_15fold', 'h4_d1_d3_d7_d14_d90__sham3_strict',
                              'h4__sham3', 'h4__sham3_strict_15fold', 'h4__sham3_strict',
                              'd1__sham3',  'd1__sham3_strict_15fold', 'd1__sham3_strict',
                              'd3__sham3', 'd3__sham3_strict_15fold', 'd3__sham3_strict', 
                              'd7__sham3', 'd7__sham3_strict_15fold', 'd7__sham3_strict', 
                              'd14__sham3', 'd14__sham3_strict_15fold', 'd14__sham3_strict', 
                              'd90__sham3', 'd90__sham3_strict_15fold', 'd90__sham3_strict', 
                              'd7__h4',  'd7__h4_strict_15fold', 'd7__h4_strict',
                              'd14__h4', 'd14__h4_15fold', 'd14__h4_strict', 
                              'd7_d14__h4', 'd7_d14__h4_strict_15fold', 'd7_d4__h4_strict',
                              'h4__d90', 'h4__d90_strict_15_fold','h4__d90_strict',
                              'd1_d3__h4_strict',
                              'd90__h4_strict',
                              'd3__h4_strict',
                              'd1__h4_strict')

names(signature_names_shortcut) <- signature_names

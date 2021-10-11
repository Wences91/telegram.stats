#' read_log
#' 
#' @param x A string with the path to the Telegram group log file in JSON format
#' @description This function read the Telegram group log file (only in JSON format) and create three dataframes: 1. full data, 2. messages data, 3. services data.
#' @export
#' @importFrom jsonlite fromJSON
#' 

read_log <- function(x){
  
  df <- fromJSON('result.json')
  df_ms <- df[['messages']][which(df[['messages']]$type=='message'),]
  df_sv <- df[['messages']][which(df[['messages']]$type=='service'),]
  
  return(list(df, df_ms, df_sv))
  
}

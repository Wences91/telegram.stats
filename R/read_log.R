#' read_log
#' 
#' @param x A string with the path to the Telegram group log file in JSON format
#' @description This function read the Telegram group log file (only in JSON format) and create three dataframes: 1. full data, 2. messages data, 3. services data.
#' @export
#' @importFrom jsonlite fromJSON
#' 

read_log <- function(x){
  
  df <- jsonlite::fromJSON(x)
  df_ms <- df[['messages']][which(df[['messages']]$type=='message'),]
  df_sv <- df[['messages']][which(df[['messages']]$type=='service'),]
  
  message('Identified:\n')
  message(paste(dim(df_ms)[1], 'messages\n'))
  message(paste(length(unique(df_sv[which(df_sv$action=='invite_members'), 'actor_id'])), 'messages\n'))
  
  return(list(df, df_ms, df_sv))
  
}

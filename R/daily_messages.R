#' daily_messages
#' 
#' @param messages_dataframe A messages data.frame obtained from the Telegram group file
#' @description This function obtain a data.frame with the number of daily messages.
#' @export
#' @importFrom dplyr mutate select group_by summarise n
#' 

users_messages <- function(messages_dataframe){
  
  df_dm  <- dplyr::mutate(messages_dataframe, date=as.Date(substr(date, 1, 10), format='%Y-%m-%d'))
  df_dm  <- dplyr::select(df_dm, date)
  df_dm  <- dplyr::group_by(df_dm, date)
  df_dm  <- dplyr::summarise(df_dm, messages = dplyr::n())
  
  message(print(dim(df_dm)[1], 'days with any message'))
  
  empty_dates <- data.frame(date=seq(min(df_dm$date), max(df_dm$date), 1),
                            messages=0,
                            stringsAsFactors = FALSE)
  
  df_dm <- rbind.data.frame(df_dm, empty_dates[which(!(empty_dates$date %in% df_dm$date)),],
                            stringsAsFactors = FALSE)
  
  message(print(dim(df_dm)[1], 'days since the Telegram group creation'))
  
  return(df_dm)
  
}

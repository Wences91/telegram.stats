#' daily_messages
#' 
#' @param messages_dataframe A messages data.frame obtained from the Telegram group file
#' @param calendar Boolean, daily messages are grouped by week, month and year
#' @description This function obtain a data.frame with the number of daily messages.
#' @export
#' @importFrom dplyr mutate select group_by summarise n
#' @importFrom lubridate month
#' 

daily_messages <- function(messages_dataframe, calendar=FALSE){
  
  df_dm  <- dplyr::mutate(messages_dataframe, date=as.Date(substr(date, 1, 10), format='%Y-%m-%d'))
  df_dm  <- dplyr::select(df_dm, date)
  df_dm  <- dplyr::group_by(df_dm, date)
  df_dm  <- dplyr::summarise(df_dm, messages = dplyr::n())
  
  message(paste(dim(df_dm)[1], 'days with any message'))
  
  empty_dates <- data.frame(date=seq(min(df_dm$date), max(df_dm$date), 1),
                            messages=0,
                            stringsAsFactors = FALSE)
  
  df_dm <- rbind.data.frame(df_dm, empty_dates[which(!(empty_dates$date %in% df_dm$date)),],
                            stringsAsFactors = FALSE)
  
  message(paste(dim(df_dm)[1], 'days since the Telegram group creation'))
  
  
  if(calendar){
    df_dm$weekday <- as.POSIXlt(df_dm$date)$wday+1
    df_dm$weekday <- factor(df_dm$weekday,
                            levels=rev(1:7),
                            labels=rev(c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')),
                            ordered=TRUE) 
    df_dm$month <- factor(lubridate::month(df_dm$date),
                          levels=as.character(1:12),
                          labels=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                          ordered=TRUE) 
    df_dm$year<- factor(substr(df_dm$date,1,4))
    
    df_dm <- dplyr::group_by(df_dm, month, weekday, year)
    df_dm <- dplyr::summarise(df_dm, messages=sum(messages))
  }
  
  return(df_dm)
  
}

#' users_messages
#' 
#' @param messages_dataframe A messages data.frame obtained from the Telegram group file
#' @description This function obtain a data.frame with the users and its number of messages.
#' @export
#' @importFrom dplyr group_by summarise arrange desc n
#' 

users_messages <- function(messages_dataframe){
  
  df_users <- messages_dataframe[,c('from', 'from_id')]
  df_users <- dplyr::group_by(df_users,  from, from_id)
  df_users <- dplyr::summarise(df_users, messages = dplyr::n())
  df_users <- dplyr::arrange(df_users, dplyr::desc(messages))
  
  message(paste(dim(df_users)[1], 'users send messages'))
  
  return(df_users)
  
}

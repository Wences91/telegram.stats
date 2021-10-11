#' users_messages
#' 
#' @param messages_dataframe A messages data.frame obtained from the Telegram group fil
#' @description This function obtaint a data.frame with the users and its number of messages.
#' @export
#' @importFrom dplyr group_by summarise arrange desc
#' 

users_messages <- function(messages_dataframe){
  
  df_users <- messages_dataframe[,c('from', 'from_id')] %>%
    dplyr::group_by(from, from_id) %>%
    dplyr::summarise(messages = n()) %>%
    dplyr::arrange(dplyr::desc(messages))
  
  message(paste(dim(df_users)[1]))
  
  return(df_users)
  
}

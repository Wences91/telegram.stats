% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/daily_messages.R
\name{daily_messages}
\alias{daily_messages}
\title{daily_messages}
\usage{
daily_messages(messages_dataframe, calendar = FALSE)
}
\arguments{
\item{messages_dataframe}{A messages data.frame obtained from the Telegram group file}

\item{calendar}{Boolean, daily messages are grouped by week, month and year}
}
\description{
This function obtain a data.frame with the number of daily messages.
}

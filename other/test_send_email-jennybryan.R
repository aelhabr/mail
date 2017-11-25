
rm(list = ls())
setwd("O:/_other/projects/gmail")

library("gmailr")
use_secret_file("gmail.json")

email_from <- "anthonyelhabr@gmail.com"
email_to <- email_from
subject <- "testing gmailr"
body <- paste(letters, collapse = "")

test_email <- mime(
  To = email_to,
  From = email_from,
  Subject = subject,
  body = body)
send_message(test_email)
safely(test_email)

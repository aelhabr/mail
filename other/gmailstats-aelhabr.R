
#' @source: https://github.com/alkashef/gmailstats

library("gmailr")

stats_email_from <- "anthonyelhabr@gmail.com"
stats_email_to <- stats_email_from
stats_email_bcc <- "my gmail address"
stats_email_subject <- "My Weekly Gmail Stats"


getToField <- function(id) {
  to(message(id, format = "full"))
}

# Authenticate Gmail access
use_secret_file("gmail.json")

# Create gmail seach query
start_date <- Sys.Date() - 30
end_date <- Sys.Date()

gmail_search_query <-
  paste0("label:sent after:", start_date, "before:", end_date)

# Retrieve message id"s using the search query
ids <- id(messages(search = gmail_search_query))

# Get the "To" field using the id"s
to_list <- unique(lapply(ids, getToField))

# Create mesasge body
stats_email_body <- paste0(
  "Emails were sent to the following addresses between ",
  start_date,
  " and ",
  end_date,
  ": \n",
  paste(to_list, collapse = "\n")
)

# Send stats email
email <- mime(
  To = stats_email_to,
  From  = stats_email_from,
  Subject = stats_email_subject,
  body = stats_email_body
)

# Check return
ret_val <- send_message(email)

if (ret_val$status_code == 200) {
  print("Sent successfully!")
} else {
  print(paste0("An error occured. Status code: ", ret_val$status_code))
}

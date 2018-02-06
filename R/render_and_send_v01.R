
#'
#'
#'
setwd("O:/_other/projects/gmail")
rm(list = ls())

#'
#'
#'
filename_script <- "analyze_nfl_picks_v02"
filename_ext <- ".docx"
filename_output <- paste0("output/",
                          # filename_script)
                          filename_script,
                          filename_ext)

#'
#'
#'
# Parameters. ----
render <- TRUE
render_backup <- TRUE
send_email <- FALSE
email_test_mode <- TRUE

#'
#'
#'

if (render == TRUE) {
  # # rmarkdown::render(paste0(filename_script, ".R"))
  
  # The following render() format does not work with "word_document" if
  # setting the output to a separate document.
  # Consider sending Word documents because html document get sent
  # in the tag of an email (when sending from work, although it seems
  # to work fine when sending from home), and rendering as a pdf does not
  # work at work because Miktek is not installed.
  rmarkdown::render(
    input = paste0(filename_script, ".R"),
    output_format = "html_document",
    output_dir = "output",
    intermediates_dir = "output"
    # params = list(),
    # output_file = filename_output
  )
  
  # Use this format for Word dcoument.
  rmarkdown::render(input = paste0(filename_script, ".R"),
                    output_file = filename_output)
  
  if (render_backup == TRUE) {
    filename_output_timestamp <- paste0(
      "output/",
      filename_script,
      "_",
      format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
      filename_ext
    )
    # file.copy(from = filename_output, to = filename_output_timestamp)
  }
  
}

#'
#'
#'
if (send_email == TRUE) {
  library("gmailr")
  use_secret_file("gmail.json")
  
  email_address_from <- "anthonyelhabr@gmail.com"
  
  if (email_test_mode == TRUE) {
    email_address_to <- email_address_from
  } else {
    email_address_to <- c(email_address_from, "andrewelhabr@gmail.com")
  }
  email_subject <- "Weekly NFL Predictions Update"
  email_body <- "See the attachment." # paste(letters, collapse = "")
  
  html_msg <-
    mime() %>%
    to(email_address_to) %>%
    from(email_address_from) %>%
    subject(email_subject) %>%
    html_body(email_body)
  
  file_attachment <-
    html_msg %>%
    # subject(email_subject) %>%
    attach_file(filename_output)
  
  email_return_val <- send_message(file_attachment)
  
  
  # library("mailR")
  # # WARNING: Need to turn ON "Allow less secure apps" via https://myaccount.google.com/lesssecureapps.
  # # WARNING: STILL NOT WORKING.
  #
  # send.mail(
  #   from = email_address_from,
  #   to = email_address_to,
  #   subject = email_subject,
  #   # body = email_body,
  #   body = filename_output,
  #   html = TRUE,
  #   smtp = list(
  #     host.name = "smtp.gmail.com",
  #     port = 465,
  #     user.name = email_address_from,
  #     passwd = "nev4cocw.",
  #     ssl = TRUE
  #   ),
  #   authenticate = TRUE,
  #   send = TRUE
  # )
  #
  # send.mail(
  #   from = email_address_from,
  #   to = email_address_to,
  #   subject = email_subject,
  #   # body = email_body,
  #   body = filename_output,
  #   html = TRUE,
  #   smtp = list(host.name = "aspmx.l.google.com", port = 25),
  #   # attach.files = filename_output,
  #   authenticate = FALSE,
  #   send = TRUE
  # )
  #
  # library("sendmailR")
  # mail_control <- list(smtpServer = "ASPMX.L.GOOGLE.COM")
  # email_msg_attachment <- mime_part(x = filename_output, name = filename_script)
  # sendmail(from = email_address_from,
  #          to = email_address_to,
  #          subject = email_subject,
  #          msg = email_body, # email_msg_attachment
  #          control = mail_control,
  #          verboseShow = TRUE)
}

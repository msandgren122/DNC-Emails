small_emails <- list.files(pattern = "*.csv")

for (i in 1:length(small_emails)) assign(small_emails[i], 
                                         read.csv(small_emails[i]))



emails_full <- do.call(rbind, lapply(small_emails, function(x) read.csv(x, stringsAsFactors = FALSE)))
emails_full$date <- as.POSIXct(emails_full$date,
                               format = "%Y-%m-%d %H:%M")
write.csv(emails_full, "emails_full.csv", row.names = FALSE)

library(magrittr)

scrape(500)
emails <- read.csv("emails.csv", check.names = FALSE)
emails[] <- lapply(emails, as.character)
View(emails)

scrape <- function(max) {
  emails <- data.frame()
  for (i in 1:max) {
    enum = i
    link <- paste("https://wikileaks.org/dnc-emails/emailid/", 
                  toString(enum), 
                  sep = "")
    link <- readLines(link)
    
    date <- grep("\t\t\t\t\tDate", link) # YYYY-MM-DD HH:MM
    from <- grep("\t\t\t\t\tFrom", link) # From
    to <- (grep("\t\t\t\t\t\t", link))[2] # To
    sub <- grep("\t\t\t\t\tSubject", link) # Subject again
    start <- grep("\t\t\t\t<div class=\"email-content\" id=\"uniquer\">", link) #content start
    end <- min(grep("\t\t\t\t</div>", link)) # content end
    
    date <- gsub("\\t", "", link[date])
    date <- gsub("Date: ", "", date)
    from <- gsub("\\t", "", link[from])
    from <- gsub("From:", "", from)
    to <- gsub("\\t", "", link[to])
    to <- gsub("From:", "", to)
    sub <- gsub("\\t", "", link[sub])
    sub <- gsub("Subject: ", "", sub)
    
    body <- link[start:end]
    body <- body[!body %in% body[grep("https", body)]]
    body <- body[!body %in% body[grep("http", body)]]
    body <- body[!body %in% body[grep("<", body)]]
    body <- body[!body %in% body[grep("Sent: ", body)]]
    body <- body[!body %in% body[grep("To: ", body)]]
    body <- body[!body %in% body[grep("Subject: ", body)]]
    body <- body[!body %in% body[grep("Phone: ", body)]]
    body <- body[!body %in% body[grep("Email: ", body)]]
    body <- body[!body %in% body[grep("Work: ", body)]]
    body <- body[!body %in% body[grep("Sender: ", body)]]
    body <- body[!body %in% body[grep("Cc: ", body)]]
    body <- body[!body %in% body[grep(" wrote:", body)]]
    body <- body[!body %in% body[grep("NOTICE: This communication", body)]]
    body <- body[!body %in% body[grep("_____", body)]]
    body <- body[body != ""]
    body <- gsub("\\\\", "", body)
    body <- gsub("[^[:alnum:]]", " ", body)
    body <- gsub("[[:digit:]]+", " ", body)
    body <- tolower(body)
    body <- paste(body, sep = " ", collapse = "")
    body <- gsub("\\s+", " ", body)
    
    
    emails <- rbind(emails, data.frame(date, 
                                       sub, 
                                       from, 
                                       to, 
                                       body, 
                                       stringsAsFactors = FALSE))
    
  }
  write.csv(emails, "emails.csv", row.names = FALSE)
}

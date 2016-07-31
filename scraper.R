scrape(1, 1000)
scrape(1001, 2000)
scrape(2001, 3000)
scrape(3001, 4000)
scrape(4001, 5000)
scrape(5001, 6000)
scrape(6001, 7000)
scrape(7001, 8000)
scrape(8001, 9000)
scrape(9001, 10000)
scrape(10001, 11000)
scrape(11001, 12000)
scrape(12001, 13000)
scrape(13001, 14000)
scrape(14001, 15000)
scrape(15001, 16000)
scrape(16001, 17000)
scrape(17001, 18000)
scrape(18001, 19000)
scrape(19001, 19252)

emails <- read.csv("emails_1_1000.csv", check.names = FALSE)
emails[] <- lapply(emails, as.character)
View(emails)

scrape <- function(min, max) {
  emails <- data.frame()
  
  for (i in min:max) {
    print(i)
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
    if (length(start) == 0) {
      start <- 1
    } else {
      start <- start
    }
    
    min_len <- length(grep("\t\t\t\t</div>", link))
    if (min_len != 0) {
      end <- min(grep("\t\t\t\t</div>", link)) # content end  
    } else {
      end <- (start + 5)
    }
    
    date <- gsub("\\t", "", link[date])
    date <- gsub("Date: ", "", date)
    from <- gsub("\\t", "", link[from])
    from <- gsub("From:", "", from)
    to <- gsub("\\t", "", link[to])
    to <- gsub("From:", "", to)
    sub <- gsub("\\t", "", link[sub])
    sub <- gsub("Subject: ", "", sub)
    print(sub)
    
    if (abs(start - end) > 0) {
      body <- link[start:end]
    } else {
      body <- "NA"
    }

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
    
    emails <- rbind(emails, 
                    data.frame(date, 
                               sub, 
                               from, 
                               to, 
                               body, 
                               stringsAsFactors = FALSE))
    
  }
  write.csv(emails, paste(min, max, "emails.csv", sep = "_"), row.names = FALSE)
}

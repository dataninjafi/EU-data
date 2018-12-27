library(tidyverse)
library(stringi)
#install.packages("sorvi")
library(sorvi)
library(httr)
# urlAgendas <- "http://ec.europa.eu/transparency/regcomitology/index.cfm?do=OpenData.download&file=agendas&language=en"
# download.file(urlAgendas, "data-files/agendas.csv")
# urlCommittees <- "http://ec.europa.eu/transparency/regcomitology/index.cfm?do=OpenData.download&file=committees&language=en"
# download.file(urlCommittees, "data-files/comittees.csv")
# urlDraftMeasures <- "http://ec.europa.eu/transparency/regcomitology/index.cfm?do=OpenData.download&file=draft_measures&language=en"
# download.file(urlDraftMeasures, "data-files/draftMeasures.csv")
# urlVotingSheets <- "http://ec.europa.eu/transparency/regcomitology/index.cfm?do=OpenData.download&file=voting_sheets&language=en"
# download.file(urlVotingSheets, "data-files/votingSheets.csv")
# urlSummaryRecords <- "http://ec.europa.eu/transparency/regcomitology/index.cfm?do=OpenData.download&file=summaries&language=en"
# download.file(urlSummaryRecords, "data-files/summaryRecords.csv")
# urlOtherRecords <- "http://ec.europa.eu/transparency/regcomitology/index.cfm?do=OpenData.download&file=others&language=en"
# download.file(urlOtherRecords, "data-files/urlOtherRecords.csv")

agendas <- read.csv2("data-files/agendas.csv", stringsAsFactors = FALSE) %>%
  mutate(fileInfo = gsub("(\\b|^)(aAgenda( of the|$)?|draft|meeting( of the|$)|\\d{1,}\\w{1,3})\\b", "", Document.title, ignore.case = TRUE))
committees <- read.csv2("data-files/comittees.csv", stringsAsFactors = FALSE) 
committeeNames <- committees %>% 
  select(1,3,4) %>% 
  distinct() %>%
  setNames(c("committeeCode","committeeTitle", "Service"))
committeeErrata <- read.csv2("manual-errata/committee-errata.csv", stringsAsFactors = FALSE) 
committeeNames <- committeeNames %>% 
  bind_rows(committeeErrata) %>% 
  mutate(committeeTitle = stri_trans_totitle(committeeTitle),
         committeeTitle = trimws(committeeTitle),
         committeeTitle = gsub(" {2,}", " ", committeeTitle))
agendas <- agendas %>% 
  rename(committeeCode = Committee.Code) %>% 
  left_join(committeeNames)
agendas <- agendas %>% 
  mutate(urlValid = is_url(File))
getFileInfo <- function(url){
  #url <- "http://ec.europa.eu/transparency/regcomitology/index.cfm?do=Search.getPDF&ds_id=3&version=1&AttLang=en&db_number=1&docType=AGENDA"
  fileInfo <- curlGetHeaders(url) 
  fileInfo <- fileInfo %>% 
    grep("content", ., ignore.case = T, value = TRUE) %>% 
    gsub("content-|attachment;|application/|filename=|\\r|\\n", "", ., ignore.case = TRUE) %>%
    gsub("charset=", "charset: ",.) %>% 
    gsub("( ){2,}", " ", .) %>% 
    paste0(collapse = ",") %>% 
    gsub(":", "= ", .) %>% 
    strsplit(., "=|,|;") %>% 
    unlist() %>% 
    trimws() %>% 
    tolower()
  names <- fileInfo[seq(1, 8, 2)] 
  seq(2, 8, 2) %>% 
    fileInfo[.] %>% 
    as.list() %>% 
    data.frame() %>% 
    setNames(., names)
  
}

networkCurlGetHeaders <- function(url){
  tryCatch(curlGetHeaders(url),
           error = function(e) return(e))
}

files <- lapply(agendas$File, networkCurlGetHeaders)

extractLang <- function(metadata){
  if(length(metadata)==2) return(NA)
  grep("Content-Language: ", metadata, value = TRUE, ignore.case = TRUE) %>% 
    gsub("Content-Language: |\\r\\n", "", ., ignore.case = TRUE)
}

agendas$lang <- sapply(files, extractLang)

extractFilename <- function(metadata){
  if(length(metadata)==2) return(NA)
  grep("Content-Disposition: ", metadata, value = TRUE, ignore.case = TRUE) %>% 
    gsub("Content-Disposition: |\\r\\n|attachment;| filename=", "", ., ignore.case = TRUE)
}

agendas$filename <- sapply(files, extractFilename)

extractEncoding <- function(metadata){
  if(length(metadata)==2) return(NA)
  grep("Content-Type: ", metadata, value = TRUE, ignore.case = TRUE) %>% 
  #grep("Content-Type: ", ., value = TRUE, ignore.case = TRUE) %>% 
    str_extract("(?<=charset=).*(?=\\r\\n)")
}

#agendas$encoding <- sapply(files, extractEncoding)
#agendas$format <- agendas$filename %>% str_extract(., "(?<=\\.).*$")
agendas$id <- 1:nrow(agendas)

downLoads <- list.files("agendaDownloads")
format <-  str_extract( downLoads,"(?<=\\.).*$")
id <- str_extract(downLoads,"^.*(?=\\.)") %>% 
  as.numeric()

downloads <- data.frame(downLoads, format, id)

agendas <- agendas %>% 
  left_join(downloads)

agendas <- agendas %>% 
  mutate(Dossier.end.date = as.Date(Dossier.end.date, format="%d/%m/%Y")) %>% 
  mutate(oldPath = paste0("agendaDownloads/", downLoads))

agendas <- agendas %>% 
  mutate(committeeShort = ifelse(nchar(committeeTitle)<120,
                                 committeeTitle,
                                 paste0(str_extract(committeeTitle, "^.{60}"), 
                                        "___",
                                        str_extract(committeeTitle, ".{60}$"))),
         newPath = paste0("agendas\\", Service, "\\", committeeShort),
         newPath = gsub("[^a-zA-Z0-9_\\ ]+", "", newPath), 
         newPath = paste0(newPath, "\\", Dossier.end.date, "_", downLoads)
  )

folders <- agendas %>% 
  .$newPath %>% 
  dirname() %>% 
#  stri_trans_totitle() %>% 
  unique()

sapply(folders, function(x) dir.create(x, recursive = TRUE))

agendaDownloader <- function(url, id, format){
  if (!is.na(format)) download.file(url, paste0("agendaDownloads\\", id, ".", format), quiet =TRUE, method="curl")
}

mapply(agendaDownloader,
       agendas$File,
       agendas$id,
       agendas$format)

move <- agendas %>% 
  filter(File!="") 

mapply(file.copy, 
       move$oldPath,
       move$newPath)


# stats

dir.create("stats")

path <- file.path("stats", "stagendasByService.csv")

agendas %>% 
  count(Service) %>% 
  arrange(desc(n)) %>% 
  write_excel_csv(path = path)

path <- file.path("stats", "agendasByCommittee.csv")

agendas %>% 
  count(Service, committeeTitle) %>% 
  arrange(desc(n)) %>% 
  write_excel_csv(path = path)



committees <- committees %>% 
  mutate_if(is.integer, as.logical) 

committeeNest <- committees %>% 
  rename(committeeCode = ï..Committee.Code) %>% 
  group_by(committeeCode) %>%
  nest()
    

committeePath <- agendas %>% 
  select(newPath, committeeCode) %>% 
  mutate(newPath = dirname(newPath)) %>% 
  distinct() %>% 
  left_join(committeeNest)

writeCommitteInfo <- function(path, data){
  data <- as.data.frame(data)
  path <- file.path(path, "committeeInfo.csv")
  write_excel_csv(data, path = path)
}

mapply(writeCommitteInfo, 
       committeePath$newPath,
       committeePath$data)

zip(zipfile = "eu-data.zip", files =c("agendas", "stats"))
  
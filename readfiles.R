#install.packages("textreadr")
library(textreadr)
library(tidyverse)
#install.packages(c("cld2", "cld3"))
#install.packages("docxtractr")
library(docxtractr)
library(cld2, cld3)
library(antiword)
files <- list.files() %>% 
  grep(".doc$", ., value = TRUE, ignore.case = TRUE)

tiedot <- lapply(files, read_doc)
lapply(files, detect_language_mixed)
detect_language(tiedot[[1]])
write.table(tiedot[[1]], "testi.txt")
testi <- antiword::antiword(files[1])
write.table(testi, "testi.txt")

pdffiles <- list.files() %>% 
  grep(".pdf$", ., value = TRUE, ignore.case = TRUE)

listToNa <- function(data){
  ifelse(is.list(data), NA, data)
}

pdf_info_df <- function(path){
  data <- path %>% 
    pdf_info()
  keys <- data$keys
  data <- lapply(data, listToNa)
  data.frame(c(data,keys))
}

library(pdftools)

textProducers <- "^Amyuni PDF Converter|^PDF CoDe|^Microsoft|^itext|^OCR CoDe|^Acrobat Distiller|^gpl ghost|^Acrobat PDFWriter"

pdfmetadata <- lapply(pdffiles, pdf_info_df)
pdfmetadata <- bind_rows(pdfmetadata)





pdfmetadata %>% 
  mutate(isPrint = ifelse(grepl(textProducers, Producer, ignore.case = TRUE), TRUE, FALSE),
         Producer = ifelse(Producer=="", NA, Producer),
         isPrint = ifelse(is.na(Producer), NA, isPrint)) %>% 
  filter(is.na(Producer)) %>% View

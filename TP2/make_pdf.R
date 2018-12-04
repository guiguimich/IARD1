pacman::p_load(knitr,staplr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

knitr::knit2pdf("header.Rnw")
rmarkdown::render("body.Rmd", "all")

# needs pdftk toolkits
system(paste0("C:/\"Program Files (x86)\"/PDFtk/bin/pdftk ",
              getwd(),"/header.pdf ",
              getwd(),"/body.pdf cat output ",
              getwd(),"/report.pdf"),intern = T)


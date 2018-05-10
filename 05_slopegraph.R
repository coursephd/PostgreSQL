
###################################
# Create Slopegraphs
###################################

if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("leeper/slopegraph")

library(slopegraph)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd <- all_met_rmsd [, Code := str_replace_all(Code, "\\.", "_")]
cnt<- unique( all_met_rmsd [patient_gender != "" & Code != "", 
                            c("mr_no", "studyday","Code", "description","distype", "patient_gender"), ])
cnt <- cnt [, `:=` (mnth = ceiling( studyday /365.25), 
                    description = paste("'", trimws(distype), ": ", trimws(description), sep = ""),
                    Code =  paste("[", trimws(Code), "]", sep="")) ]
cntrow <- cnt [, .(permnth = uniqueN(mr_no) ), by =.(Code, mnth)]

cntrow02 <- dcast(data = cntrow, 
                       Code ~ paste("year", str_pad(mnth, width=2, pad="0", side= c("left")), sep=""),
                       value.var = c("permnth"),
                       fill=0)

cntrow02 <- cntrow02 [year01 < 200]
cntrow03 <- data.frame(cntrow02[,-1], row.names=cntrow02$Code)

slopegraph(cntrow03, col.lines = 'gray', col.lab = "black", 
           xlim = c(-.5,5.5), cex.lab = 0.5, cex.num = 0.5,
           xlabels = c('1 Year','2 Years','3 Years',
                       '4 Years', '5 Years', '6 Years', '7 Years'))
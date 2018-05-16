#####################
# This version is for m / f
# Use 04dis_gender_csv folder
#####################
all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd <- all_met_rmsd [, Code := str_replace_all(Code, "\\.", "_")]
cnt<- unique( all_met_rmsd [patient_gender != "" & Code != "", 
                            c("mr_no", "studyday","Code", "description","distype", "patient_gender"), ])
cnt <- cnt [, `:=` (mnth = round( studyday /30.25, digits = 0), 
                    description = paste("[", trimws(distype), ": ", trimws(description), "]", sep = ""),
                    Code =  paste("[", trimws(Code), "]", sep="")) ]
cntrow <- cnt [, .(permnth = uniqueN(Code) ), by =.(mr_no, mnth)]

cnt <- cnt [order(mr_no, studyday, Code, description, patient_gender)]
cnt2 <- unique(cnt [, c("mr_no", "Code", "description", "patient_gender", "distype"), ])

# Combinations for each patient
# Do these calculations for first rows
cnt3 <- cnt2[, `:=` (numcomb = seq_len(.N),
                     descomb = description,
                     discomb = Code,
                     grpcomb = paste(trimws(Code), collapse = " ", sep=)),
             by = .(mr_no, patient_gender)]

cnt30 <- cnt3 [numcomb > 1, `:=` (discomb = sapply(seq_len(.N), function(x) paste(Code[seq_len(x)], collapse = ">")),
                                  descomb = sapply(seq_len(.N), function(x) paste(description[seq_len(x)], collapse = ">")) ),
               by = .(mr_no, patient_gender)]

cnt31 <- rbind(cnt3 [numcomb ==1], cnt30 [numcomb > 1])

# Starting disease sttdis
stt <- cnt3 [ numcomb == 1, .(sttdis = paste(descomb, ">", patient_gender, sep="")), by =.(mr_no, patient_gender, Code, description)]
cnt3disprgs <- merge(cnt31, stt [, c("mr_no", "sttdis"), ], by = c("mr_no"))

cnt3disprgs <- cnt3disprgs [, .(npt = uniqueN(mr_no)), by = .(discomb, descomb, numcomb, grpcomb, sttdis, patient_gender)]

cnt3disprgs <- cnt3disprgs [order(sttdis, patient_gender, numcomb, discomb, grpcomb)]
cnt3disprgs <- cnt3disprgs [, node := 1:.N, by =.(sttdis, patient_gender, grpcomb, npt)]

cnt3disprgs <- cnt3disprgs [, treecomb := paste(sttdis, ">", descomb, " (N=", npt, ")", sep="")]
cnt3disprgs <- cnt3disprgs [order(sttdis, grpcomb, node)]

cnt3disprgs02 <- cnt3disprgs [numcomb > 1]

# These 2 subsets are for the CSV for D3js
sttdis <- unique(stt [, c("description"), ])
sttdisgen <- unique(stt [ sttdis != "", c("sttdis"), ])
frow <- data.table ( treecomb = "id,value")

# Rename to the same variable
setnames(sttdis, "description", "treecomb")
setnames(sttdisgen, "sttdis", "treecomb")

cnt3disprgs03 <- rbind(cnt3disprgs02 [, c("treecomb")], sttdis, sttdisgen) [order(treecomb)]
cnt3disprgs03 <- cnt3disprgs03[, treecomb := paste("Disease>", treecomb, sep="")]

#cnt3disprgs03 <- rbind(frow, cnt3disprgs03)

# No subset
fwrite(unique(cnt3disprgs03), 
       col.names = FALSE,
       quote = FALSE,
       "D:\\Hospital_data\\ProgresSQL\\misc\\jsfolder\\999temp\\decode_gender.csv")
#####################################################################################
# End of program
#####################################################################################

# Create Json file using the following commands:
# This is the working directory path.
SimpleJar=Hospital_data/ProgresSQL/misc/jsfolder/999temp
java -classpath `cygpath -wp /cygdrive/d/${SimpleJar}:./json-simple-1.1.1.jar` D3Taxonomy decode_gender.csv ">"

#After this, execute browser-sync or HTTP-server command on Cygwin
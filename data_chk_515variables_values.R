library(Hmisc)
library(data.table)
library(stringi)
library(stringr)

sec001 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec001.rds")
sec001_1 <- sec001 [, (names(sec001) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec001)) ), with =FALSE]

sec001_1t <- melt (data = sec001_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec001), value = TRUE) )
sec001_1t <- sec001_1t [, value := toupper(value)] [order(variable, value)]
sec001_1t <- unique (sec001_1t [, -c("mr_no", "patient_id", "subvis")])

sec001_1t <- sec001_1t [, nrow :=1:.N, by = .(variable)]

secmax001 <- sec001_1t [, .(secmax = max(nrow)), by =.(variable)]
sec001_1t2 <- dcast(sec001_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec002 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec002.rds")
sec002_1 <- sec002 [, (names(sec002) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec002)) ), with =FALSE]

sec002_1t <- melt (data = sec002_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec002), value = TRUE) )
sec002_1t <- sec002_1t [, value := toupper(value)] [order(variable, value)]
sec002_1t <- unique (sec002_1t [, -c("mr_no", "patient_id", "subvis")])

sec002_1t <- sec002_1t [, nrow :=1:.N, by = .(variable)]

secmax002 <- sec002_1t [, .(secmax = max(nrow)), by =.(variable)]
sec002_1t2 <- dcast(sec002_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec003 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec003.rds")
sec003_1 <- sec003 [, (names(sec003) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec003)) ), with =FALSE]

sec003_1t <- melt (data = sec003_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec003), value = TRUE) )
sec003_1t <- sec003_1t [, value := toupper(value)] [order(variable, value)]
sec003_1t <- unique (sec003_1t [, -c("mr_no", "patient_id", "subvis")])

sec003_1t <- sec003_1t [, nrow :=1:.N, by = .(variable)]

secmax003 <- sec003_1t [, .(secmax = max(nrow)), by =.(variable)]
sec003_1t2 <- dcast(sec003_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec004 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec004.rds")
sec004_1 <- sec004 [, (names(sec004) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec004)) ), with =FALSE]

sec004_1t <- melt (data = sec004_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec004), value = TRUE) )
sec004_1t <- sec004_1t [, value := toupper(value)] [order(variable, value)]
sec004_1t <- unique (sec004_1t [, -c("mr_no", "patient_id", "subvis")])

sec004_1t <- sec004_1t [, nrow :=1:.N, by = .(variable)]

secmax004 <- sec004_1t [, .(secmax = max(nrow)), by =.(variable)]
sec004_1t2 <- dcast(sec004_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec006 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec006.rds")
sec006_1 <- sec006 [, (names(sec006) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec006)) ), with =FALSE]

sec006_1t <- melt (data = sec006_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec006), value = TRUE) )
sec006_1t <- sec006_1t [, value := toupper(value)] [order(variable, value)]
sec006_1t <- unique (sec006_1t [, -c("mr_no", "patient_id", "subvis")])

sec006_1t <- sec006_1t [, nrow :=1:.N, by = .(variable)]

secmax006 <- sec006_1t [, .(secmax = max(nrow)), by =.(variable)]
sec006_1t2 <- dcast(sec006_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec007 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec007.rds")
sec007_1 <- sec007 [, (names(sec007) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec007)) ), with =FALSE]

sec007_1t <- melt (data = sec007_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec007), value = TRUE) )
sec007_1t <- sec007_1t [, value := toupper(value)] [order(variable, value)]
sec007_1t <- unique (sec007_1t [, -c("mr_no", "patient_id", "subvis")])

sec007_1t <- sec007_1t [, nrow :=1:.N, by = .(variable)]

secmax007 <- sec007_1t [, .(secmax = max(nrow)), by =.(variable)]
sec007_1t2 <- dcast(sec007_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec008 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec008.rds")
sec008_1 <- sec008 [, (names(sec008) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec008)) ), with =FALSE]

sec008_1t <- melt (data = sec008_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec008), value = TRUE) )
sec008_1t <- sec008_1t [, value := toupper(value)] [order(variable, value)]
sec008_1t <- unique (sec008_1t [, -c("mr_no", "patient_id", "subvis")])

sec008_1t <- sec008_1t [, nrow :=1:.N, by = .(variable)]

secmax008 <- sec008_1t [, .(secmax = max(nrow)), by =.(variable)]
sec008_1t2 <- dcast(sec008_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec009 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec009.rds")
sec009_1 <- sec009 [, (names(sec009) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec009)) ), with =FALSE]

sec009_1t <- melt (data = sec009_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec009), value = TRUE) )
sec009_1t <- sec009_1t [, value := toupper(value)] [order(variable, value)]
sec009_1t <- unique (sec009_1t [, -c("mr_no", "patient_id", "subvis")])

sec009_1t <- sec009_1t [, nrow :=1:.N, by = .(variable)]

secmax009 <- sec009_1t [, .(secmax = max(nrow)), by =.(variable)]
sec009_1t2 <- dcast(sec009_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec010 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec010.rds")
sec010_1 <- sec010 [, (names(sec010) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec010)) ), with =FALSE]

sec010_1t <- melt (data = sec010_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec010), value = TRUE) )
sec010_1t <- sec010_1t [, value := toupper(value)] [order(variable, value)]
sec010_1t <- unique (sec010_1t [, -c("mr_no", "patient_id", "subvis")])

sec010_1t <- sec010_1t [, nrow :=1:.N, by = .(variable)]

secmax010 <- sec010_1t [, .(secmax = max(nrow)), by =.(variable)]
sec010_1t2 <- dcast(sec010_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec011 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec011.rds")
sec011_1 <- sec011 [, (names(sec011) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec011)) ), with =FALSE]

sec011_1t <- melt (data = sec011_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec011), value = TRUE) )
sec011_1t <- sec011_1t [, value := toupper(value)] [order(variable, value)]
sec011_1t <- unique (sec011_1t [, -c("mr_no", "patient_id", "subvis")])

sec011_1t <- sec011_1t [, nrow :=1:.N, by = .(variable)]

secmax011 <- sec011_1t [, .(secmax = max(nrow)), by =.(variable)]
sec011_1t2 <- dcast(sec011_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec012 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec012.rds")
sec012_1 <- sec012 [, (names(sec012) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec012)) ), with =FALSE]

sec012_1t <- melt (data = sec012_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec012), value = TRUE) )
sec012_1t <- sec012_1t [, value := toupper(value)] [order(variable, value)]
sec012_1t <- unique (sec012_1t [, -c("mr_no", "patient_id", "subvis")])

sec012_1t <- sec012_1t [, nrow :=1:.N, by = .(variable)]

secmax012 <- sec012_1t [, .(secmax = max(nrow)), by =.(variable)]
sec012_1t2 <- dcast(sec012_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec013 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec013.rds")
sec013_1 <- sec013 [, (names(sec013) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec013)) ), with =FALSE]

sec013_1t <- melt (data = sec013_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec013), value = TRUE) )
sec013_1t <- sec013_1t [, value := toupper(value)] [order(variable, value)]
sec013_1t <- unique (sec013_1t [, -c("mr_no", "patient_id", "subvis")])

sec013_1t <- sec013_1t [, nrow :=1:.N, by = .(variable)]

secmax013 <- sec013_1t [, .(secmax = max(nrow)), by =.(variable)]
sec013_1t2 <- dcast(sec013_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))
sec014 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec014.rds")
sec014_1 <- sec014 [, (names(sec014) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec014)) ), with =FALSE]

sec014_1t <- melt (data = sec014_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec014), value = TRUE) )
sec014_1t <- sec014_1t [, value := toupper(value)] [order(variable, value)]
sec014_1t <- unique (sec014_1t [, -c("mr_no", "patient_id", "subvis")])

sec014_1t <- sec014_1t [, nrow :=1:.N, by = .(variable)]

secmax014 <- sec014_1t [, .(secmax = max(nrow)), by =.(variable)]
sec014_1t2 <- dcast(sec014_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))
sec017 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec017.rds")
sec017_1 <- sec017 [, (names(sec017) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec017)) ), with =FALSE]

sec017_1t <- melt (data = sec017_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec017), value = TRUE) )
sec017_1t <- sec017_1t [, value := toupper(value)] [order(variable, value)]
sec017_1t <- unique (sec017_1t [, -c("mr_no", "patient_id", "subvis")])

sec017_1t <- sec017_1t [, nrow :=1:.N, by = .(variable)]

secmax017 <- sec017_1t [, .(secmax = max(nrow)), by =.(variable)]
sec017_1t2 <- dcast(sec017_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec019 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec019.rds")
sec019_1 <- sec019 [, (names(sec019) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec019)) ), with =FALSE]

sec019_1t <- melt (data = sec019_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec019), value = TRUE) )
sec019_1t <- sec019_1t [, value := toupper(value)] [order(variable, value)]
sec019_1t <- unique (sec019_1t [, -c("mr_no", "patient_id", "subvis")])

sec019_1t <- sec019_1t [, nrow :=1:.N, by = .(variable)]

secmax019 <- sec019_1t [, .(secmax = max(nrow)), by =.(variable)]
sec019_1t2 <- dcast(sec019_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec020 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec020.rds")
sec020_1 <- sec020 [, (names(sec020) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec020)) ), with =FALSE]

sec020_1t <- melt (data = sec020_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec020), value = TRUE) )
sec020_1t <- sec020_1t [, value := toupper(value)] [order(variable, value)]
sec020_1t <- unique (sec020_1t [, -c("mr_no", "patient_id", "subvis")])

sec020_1t <- sec020_1t [, nrow :=1:.N, by = .(variable)]

secmax020 <- sec020_1t [, .(secmax = max(nrow)), by =.(variable)]
sec020_1t2 <- dcast(sec020_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec021 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec021.rds")
sec021_1 <- sec021 [, (names(sec021) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec021)) ), with =FALSE]

sec021_1t <- melt (data = sec021_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec021), value = TRUE) )
sec021_1t <- sec021_1t [, value := toupper(value)] [order(variable, value)]
sec021_1t <- unique (sec021_1t [, -c("mr_no", "patient_id", "subvis")])

sec021_1t <- sec021_1t [, nrow :=1:.N, by = .(variable)]

secmax021 <- sec021_1t [, .(secmax = max(nrow)), by =.(variable)]
sec021_1t2 <- dcast(sec021_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec022 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec022.rds")
sec022_1 <- sec022 [, (names(sec022) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec022)) ), with =FALSE]

sec022_1t <- melt (data = sec022_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec022), value = TRUE) )
sec022_1t <- sec022_1t [, value := toupper(value)] [order(variable, value)]
sec022_1t <- unique (sec022_1t [, -c("mr_no", "patient_id", "subvis")])

sec022_1t <- sec022_1t [, nrow :=1:.N, by = .(variable)]

secmax022 <- sec022_1t [, .(secmax = max(nrow)), by =.(variable)]
sec022_1t2 <- dcast(sec022_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec023 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec023.rds")
sec023_1 <- sec023 [, (names(sec023) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec023)) ), with =FALSE]

sec023_1t <- melt (data = sec023_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec023), value = TRUE) )
sec023_1t <- sec023_1t [, value := toupper(value)] [order(variable, value)]
sec023_1t <- unique (sec023_1t [, -c("mr_no", "patient_id", "subvis")])

sec023_1t <- sec023_1t [, nrow :=1:.N, by = .(variable)]

secmax023 <- sec023_1t [, .(secmax = max(nrow)), by =.(variable)]
sec023_1t2 <- dcast(sec023_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec024 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec024.rds")
sec024_1 <- sec024 [, (names(sec024) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec024)) ), with =FALSE]

sec024_1t <- melt (data = sec024_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec024), value = TRUE) )
sec024_1t <- sec024_1t [, value := toupper(value)] [order(variable, value)]
sec024_1t <- unique (sec024_1t [, -c("mr_no", "patient_id", "subvis")])

sec024_1t <- sec024_1t [, nrow :=1:.N, by = .(variable)]

secmax024 <- sec024_1t [, .(secmax = max(nrow)), by =.(variable)]
sec024_1t2 <- dcast(sec024_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec026 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec026.rds")
sec026_1 <- sec026 [, (names(sec026) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec026)) ), with =FALSE]

sec026_1t <- melt (data = sec026_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec026), value = TRUE) )
sec026_1t <- sec026_1t [, value := toupper(value)] [order(variable, value)]
sec026_1t <- unique (sec026_1t [, -c("mr_no", "patient_id", "subvis")])

sec026_1t <- sec026_1t [, nrow :=1:.N, by = .(variable)]

secmax026 <- sec026_1t [, .(secmax = max(nrow)), by =.(variable)]
sec026_1t2 <- dcast(sec026_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec027 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec027.rds")
sec027_1 <- sec027 [, (names(sec027) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec027)) ), with =FALSE]

sec027_1t <- melt (data = sec027_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec027), value = TRUE) )
sec027_1t <- sec027_1t [, value := toupper(value)] [order(variable, value)]
sec027_1t <- unique (sec027_1t [, -c("mr_no", "patient_id", "subvis")])

sec027_1t <- sec027_1t [, nrow :=1:.N, by = .(variable)]

secmax027 <- sec027_1t [, .(secmax = max(nrow)), by =.(variable)]
sec027_1t2 <- dcast(sec027_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec030 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec030.rds")
sec030_1 <- sec030 [, (names(sec030) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec030)) ), with =FALSE]

sec030_1t <- melt (data = sec030_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec030), value = TRUE) )
sec030_1t <- sec030_1t [, value := toupper(value)] [order(variable, value)]
sec030_1t <- unique (sec030_1t [, -c("mr_no", "patient_id", "subvis")])

sec030_1t <- sec030_1t [, nrow :=1:.N, by = .(variable)]

secmax030 <- sec030_1t [, .(secmax = max(nrow)), by =.(variable)]
sec030_1t2 <- dcast(sec030_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec031 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec031.rds")
sec031_1 <- sec031 [, (names(sec031) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec031)) ), with =FALSE]

sec031_1t <- melt (data = sec031_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec031), value = TRUE) )
sec031_1t <- sec031_1t [, value := toupper(value)] [order(variable, value)]
sec031_1t <- unique (sec031_1t [, -c("mr_no", "patient_id", "subvis")])

sec031_1t <- sec031_1t [, nrow :=1:.N, by = .(variable)]

secmax031 <- sec031_1t [, .(secmax = max(nrow)), by =.(variable)]
sec031_1t2 <- dcast(sec031_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec032 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec032.rds")
sec032_1 <- sec032 [, (names(sec032) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec032)) ), with =FALSE]

sec032_1t <- melt (data = sec032_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec032), value = TRUE) )
sec032_1t <- sec032_1t [, value := toupper(value)] [order(variable, value)]
sec032_1t <- unique (sec032_1t [, -c("mr_no", "patient_id", "subvis")])

sec032_1t <- sec032_1t [, nrow :=1:.N, by = .(variable)]

secmax032 <- sec032_1t [, .(secmax = max(nrow)), by =.(variable)]
sec032_1t2 <- dcast(sec032_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec033 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec033.rds")
sec033_1 <- sec033 [, (names(sec033) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec033)) ), with =FALSE]

sec033_1t <- melt (data = sec033_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec033), value = TRUE) )
sec033_1t <- sec033_1t [, value := toupper(value)] [order(variable, value)]
sec033_1t <- unique (sec033_1t [, -c("mr_no", "patient_id", "subvis")])

sec033_1t <- sec033_1t [, nrow :=1:.N, by = .(variable)]

secmax033 <- sec033_1t [, .(secmax = max(nrow)), by =.(variable)]
sec033_1t2 <- dcast(sec033_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec034 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec034.rds")
sec034_1 <- sec034 [, (names(sec034) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec034)) ), with =FALSE]

sec034_1t <- melt (data = sec034_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec034), value = TRUE) )
sec034_1t <- sec034_1t [, value := toupper(value)] [order(variable, value)]
sec034_1t <- unique (sec034_1t [, -c("mr_no", "patient_id", "subvis")])

sec034_1t <- sec034_1t [, nrow :=1:.N, by = .(variable)]

secmax034 <- sec034_1t [, .(secmax = max(nrow)), by =.(variable)]
sec034_1t2 <- dcast(sec034_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec035 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec035.rds")
sec035_1 <- sec035 [, (names(sec035) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec035)) ), with =FALSE]

sec035_1t <- melt (data = sec035_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec035), value = TRUE) )
sec035_1t <- sec035_1t [, value := toupper(value)] [order(variable, value)]
sec035_1t <- unique (sec035_1t [, -c("mr_no", "patient_id", "subvis")])

sec035_1t <- sec035_1t [, nrow :=1:.N, by = .(variable)]

secmax035 <- sec035_1t [, .(secmax = max(nrow)), by =.(variable)]
sec035_1t2 <- dcast(sec035_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec036 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec036.rds")
sec036_1 <- sec036 [, (names(sec036) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec036)) ), with =FALSE]

sec036_1t <- melt (data = sec036_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec036), value = TRUE) )
sec036_1t <- sec036_1t [, value := toupper(value)] [order(variable, value)]
sec036_1t <- unique (sec036_1t [, -c("mr_no", "patient_id", "subvis")])

sec036_1t <- sec036_1t [, nrow :=1:.N, by = .(variable)]

secmax036 <- sec036_1t [, .(secmax = max(nrow)), by =.(variable)]
sec036_1t2 <- dcast(sec036_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec037 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec037.rds")
sec037_1 <- sec037 [, (names(sec037) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec037)) ), with =FALSE]

sec037_1t <- melt (data = sec037_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec037), value = TRUE) )
sec037_1t <- sec037_1t [, value := toupper(value)] [order(variable, value)]
sec037_1t <- unique (sec037_1t [, -c("mr_no", "patient_id", "subvis")])

sec037_1t <- sec037_1t [, nrow :=1:.N, by = .(variable)]

secmax037 <- sec037_1t [, .(secmax = max(nrow)), by =.(variable)]
sec037_1t2 <- dcast(sec037_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec038 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec038.rds")
sec038_1 <- sec038 [, (names(sec038) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec038)) ), with =FALSE]

sec038_1t <- melt (data = sec038_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec038), value = TRUE) )
sec038_1t <- sec038_1t [, value := toupper(value)] [order(variable, value)]
sec038_1t <- unique (sec038_1t [, -c("mr_no", "patient_id", "subvis")])

sec038_1t <- sec038_1t [, nrow :=1:.N, by = .(variable)]

secmax038 <- sec038_1t [, .(secmax = max(nrow)), by =.(variable)]
sec038_1t2 <- dcast(sec038_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec044 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec044.rds")
sec044_1 <- sec044 [, (names(sec044) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec044)) ), with =FALSE]

sec044_1t <- melt (data = sec044_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec044), value = TRUE) )
sec044_1t <- sec044_1t [, value := toupper(value)] [order(variable, value)]
sec044_1t <- unique (sec044_1t [, -c("mr_no", "patient_id", "subvis")])

sec044_1t <- sec044_1t [, nrow :=1:.N, by = .(variable)]

secmax044 <- sec044_1t [, .(secmax = max(nrow)), by =.(variable)]
sec044_1t2 <- dcast(sec044_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec077 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec077.rds")
sec077_1 <- sec077 [, (names(sec077) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec077)) ), with =FALSE]

sec077_1t <- melt (data = sec077_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec077), value = TRUE) )
sec077_1t <- sec077_1t [, value := toupper(value)] [order(variable, value)]
sec077_1t <- unique (sec077_1t [, -c("mr_no", "patient_id", "subvis")])

sec077_1t <- sec077_1t [, nrow :=1:.N, by = .(variable)]

secmax077 <- sec077_1t [, .(secmax = max(nrow)), by =.(variable)]
sec077_1t2 <- dcast(sec077_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))


sec078 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec078.rds")
sec078_1 <- sec078 [, (names(sec078) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec078)) ), with =FALSE]

sec078_1t <- melt (data = sec078_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec078), value = TRUE) )
sec078_1t <- sec078_1t [, value := toupper(value)] [order(variable, value)]
sec078_1t <- unique (sec078_1t [, -c("mr_no", "patient_id", "subvis")])

sec078_1t <- sec078_1t [, nrow :=1:.N, by = .(variable)]

secmax078 <- sec078_1t [, .(secmax = max(nrow)), by =.(variable)]
sec078_1t2 <- dcast(sec078_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))


sec079 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec079.rds")
sec079_1 <- sec079 [, (names(sec079) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec079)) ), with =FALSE]

sec079_1t <- melt (data = sec079_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec079), value = TRUE) )
sec079_1t <- sec079_1t [, value := toupper(value)] [order(variable, value)]
sec079_1t <- unique (sec079_1t [, -c("mr_no", "patient_id", "subvis")])

sec079_1t <- sec079_1t [, nrow :=1:.N, by = .(variable)]

secmax079 <- sec079_1t [, .(secmax = max(nrow)), by =.(variable)]
sec079_1t2 <- dcast(sec079_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))


sec080 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec080.rds")
sec080_1 <- sec080 [, (names(sec080) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec080)) ), with =FALSE]

sec080_1t <- melt (data = sec080_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec080), value = TRUE) )
sec080_1t <- sec080_1t [, value := toupper(value)] [order(variable, value)]
sec080_1t <- unique (sec080_1t [, -c("mr_no", "patient_id", "subvis")])

sec080_1t <- sec080_1t [, nrow :=1:.N, by = .(variable)]

secmax080 <- sec080_1t [, .(secmax = max(nrow)), by =.(variable)]
sec080_1t2 <- dcast(sec080_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))


sec081 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec081.rds")
sec081_1 <- sec081 [, (names(sec081) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec081)) ), with =FALSE]

sec081_1t <- melt (data = sec081_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec081), value = TRUE) )
sec081_1t <- sec081_1t [, value := toupper(value)] [order(variable, value)]
sec081_1t <- unique (sec081_1t [, -c("mr_no", "patient_id", "subvis")])

sec081_1t <- sec081_1t [, nrow :=1:.N, by = .(variable)]

secmax081 <- sec081_1t [, .(secmax = max(nrow)), by =.(variable)]
sec081_1t2 <- dcast(sec081_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))


sec082 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec082.rds")
sec082_1 <- sec082 [, (names(sec082) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec082)) ), with =FALSE]

sec082_1t <- melt (data = sec082_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec082), value = TRUE) )
sec082_1t <- sec082_1t [, value := toupper(value)] [order(variable, value)]
sec082_1t <- unique (sec082_1t [, -c("mr_no", "patient_id", "subvis")])

sec082_1t <- sec082_1t [, nrow :=1:.N, by = .(variable)]

secmax082 <- sec082_1t [, .(secmax = max(nrow)), by =.(variable)]
sec082_1t2 <- dcast(sec082_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec083 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec083.rds")
sec083_1 <- sec083 [, (names(sec083) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec083)) ), with =FALSE]

sec083_1t <- melt (data = sec083_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec083), value = TRUE) )
sec083_1t <- sec083_1t [, value := toupper(value)] [order(variable, value)]
sec083_1t <- unique (sec083_1t [, -c("mr_no", "patient_id", "subvis")])

sec083_1t <- sec083_1t [, nrow :=1:.N, by = .(variable)]

secmax083 <- sec083_1t [, .(secmax = max(nrow)), by =.(variable)]
sec083_1t2 <- dcast(sec083_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))


sec084 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec084.rds")
sec084_1 <- sec084 [, (names(sec084) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec084)) ), with =FALSE]

sec084_1t <- melt (data = sec084_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec084), value = TRUE) )
sec084_1t <- sec084_1t [, value := toupper(value)] [order(variable, value)]
sec084_1t <- unique (sec084_1t [, -c("mr_no", "patient_id", "subvis")])

sec084_1t <- sec084_1t [, nrow :=1:.N, by = .(variable)]

secmax084 <- sec084_1t [, .(secmax = max(nrow)), by =.(variable)]
sec084_1t2 <- dcast(sec084_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))


sec085 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec085.rds")
sec085_1 <- sec085 [, (names(sec085) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec085)) ), with =FALSE]

sec085_1t <- melt (data = sec085_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec085), value = TRUE) )
sec085_1t <- sec085_1t [, value := toupper(value)] [order(variable, value)]
sec085_1t <- unique (sec085_1t [, -c("mr_no", "patient_id", "subvis")])

sec085_1t <- sec085_1t [, nrow :=1:.N, by = .(variable)]

secmax085 <- sec085_1t [, .(secmax = max(nrow)), by =.(variable)]
sec085_1t2 <- dcast(sec085_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))


sec086 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec086.rds")
sec086_1 <- sec086 [, (names(sec086) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec086)) ), with =FALSE]

sec086_1t <- melt (data = sec086_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec086), value = TRUE) )
sec086_1t <- sec086_1t [, value := toupper(value)] [order(variable, value)]
sec086_1t <- unique (sec086_1t [, -c("mr_no", "patient_id", "subvis")])

sec086_1t <- sec086_1t [, nrow :=1:.N, by = .(variable)]

secmax086 <- sec086_1t [, .(secmax = max(nrow)), by =.(variable)]
sec086_1t2 <- dcast(sec086_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))


sec088 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec088.rds")
sec088_1 <- sec088 [, (names(sec088) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec088)) ), with =FALSE]

sec088_1t <- melt (data = sec088_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec088), value = TRUE) )
sec088_1t <- sec088_1t [, value := toupper(value)] [order(variable, value)]
sec088_1t <- unique (sec088_1t [, -c("mr_no", "patient_id", "subvis")])

sec088_1t <- sec088_1t [, nrow :=1:.N, by = .(variable)]

secmax088 <- sec088_1t [, .(secmax = max(nrow)), by =.(variable)]
sec088_1t2 <- dcast(sec088_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))


sec089 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec089.rds")
sec089_1 <- sec089 [, (names(sec089) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec089)) ), with =FALSE]

sec089_1t <- melt (data = sec089_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec089), value = TRUE) )
sec089_1t <- sec089_1t [, value := toupper(value)] [order(variable, value)]
sec089_1t <- unique (sec089_1t [, -c("mr_no", "patient_id", "subvis")])

sec089_1t <- sec089_1t [, nrow :=1:.N, by = .(variable)]

secmax089 <- sec089_1t [, .(secmax = max(nrow)), by =.(variable)]
sec089_1t2 <- dcast(sec089_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))


sec099 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec099.rds")
sec099_1 <- sec099 [, (names(sec099) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec099)) ), with =FALSE]

sec099_1t <- melt (data = sec099_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec099), value = TRUE) )
sec099_1t <- sec099_1t [, value := toupper(value)] [order(variable, value)]
sec099_1t <- unique (sec099_1t [, -c("mr_no", "patient_id", "subvis")])

sec099_1t <- sec099_1t [, nrow :=1:.N, by = .(variable)]

secmax099 <- sec099_1t [, .(secmax = max(nrow)), by =.(variable)]
sec099_1t2 <- dcast(sec099_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))


sec100 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec100.rds")
sec100_1 <- sec100 [, (names(sec100) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec100)) ), with =FALSE]

sec100_1t <- melt (data = sec100_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec100), value = TRUE) )
sec100_1t <- sec100_1t [, value := toupper(value)] [order(variable, value)]
sec100_1t <- unique (sec100_1t [, -c("mr_no", "patient_id", "subvis")])

sec100_1t <- sec100_1t [, nrow :=1:.N, by = .(variable)]

secmax100 <- sec100_1t [, .(secmax = max(nrow)), by =.(variable)]
sec100_1t2 <- dcast(sec100_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec101 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec101.rds")
sec101_1 <- sec101 [, (names(sec101) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec101)) ), with =FALSE]

sec101_1t <- melt (data = sec101_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec101), value = TRUE) )
sec101_1t <- sec101_1t [, value := toupper(value)] [order(variable, value)]
sec101_1t <- unique (sec101_1t [, -c("mr_no", "patient_id", "subvis")])

sec101_1t <- sec101_1t [, nrow :=1:.N, by = .(variable)]

secmax101 <- sec101_1t [, .(secmax = max(nrow)), by =.(variable)]
sec101_1t2 <- dcast(sec101_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec102 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec102.rds")
sec102_1 <- sec102 [, (names(sec102) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec102)) ), with =FALSE]

sec102_1t <- melt (data = sec102_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec102), value = TRUE) )
sec102_1t <- sec102_1t [, value := toupper(value)] [order(variable, value)]
sec102_1t <- unique (sec102_1t [, -c("mr_no", "patient_id", "subvis")])

sec102_1t <- sec102_1t [, nrow :=1:.N, by = .(variable)]

secmax102 <- sec102_1t [, .(secmax = max(nrow)), by =.(variable)]
sec102_1t2 <- dcast(sec102_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec103 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec103.rds")
sec103_1 <- sec103 [, (names(sec103) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec103)) ), with =FALSE]

sec103_1t <- melt (data = sec103_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec103), value = TRUE) )
sec103_1t <- sec103_1t [, value := toupper(value)] [order(variable, value)]
sec103_1t <- unique (sec103_1t [, -c("mr_no", "patient_id", "subvis")])

sec103_1t <- sec103_1t [, nrow :=1:.N, by = .(variable)]

secmax103 <- sec103_1t [, .(secmax = max(nrow)), by =.(variable)]
sec103_1t2 <- dcast(sec103_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec106 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec106.rds")
sec106_1 <- sec106 [, (names(sec106) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec106)) ), with =FALSE]

sec106_1t <- melt (data = sec106_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec106), value = TRUE) )
sec106_1t <- sec106_1t [, value := toupper(value)] [order(variable, value)]
sec106_1t <- unique (sec106_1t [, -c("mr_no", "patient_id", "subvis")])

sec106_1t <- sec106_1t [, nrow :=1:.N, by = .(variable)]

secmax106 <- sec106_1t [, .(secmax = max(nrow)), by =.(variable)]
sec106_1t2 <- dcast(sec106_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec111 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec111.rds")
sec111_1 <- sec111 [, (names(sec111) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec111)) ), with =FALSE]

sec111_1t <- melt (data = sec111_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec111), value = TRUE) )
sec111_1t <- sec111_1t [, value := toupper(value)] [order(variable, value)]
sec111_1t <- unique (sec111_1t [, -c("mr_no", "patient_id", "subvis")])

sec111_1t <- sec111_1t [, nrow :=1:.N, by = .(variable)]

secmax111 <- sec111_1t [, .(secmax = max(nrow)), by =.(variable)]
sec111_1t2 <- dcast(sec111_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))


sec112 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec112.rds")
sec112_1 <- sec112 [, (names(sec112) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec112)) ), with =FALSE]

sec112_1t <- melt (data = sec112_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec112), value = TRUE) )
sec112_1t <- sec112_1t [, value := toupper(value)] [order(variable, value)]
sec112_1t <- unique (sec112_1t [, -c("mr_no", "patient_id", "subvis")])

sec112_1t <- sec112_1t [, nrow :=1:.N, by = .(variable)]

secmax112 <- sec112_1t [, .(secmax = max(nrow)), by =.(variable)]
sec112_1t2 <- dcast(sec112_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))


sec114 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec114.rds")
sec114_1 <- sec114 [, (names(sec114) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec114)) ), with =FALSE]

sec114_1t <- melt (data = sec114_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec114), value = TRUE) )
sec114_1t <- sec114_1t [, value := toupper(value)] [order(variable, value)]
sec114_1t <- unique (sec114_1t [, -c("mr_no", "patient_id", "subvis")])

sec114_1t <- sec114_1t [, nrow :=1:.N, by = .(variable)]

secmax114 <- sec114_1t [, .(secmax = max(nrow)), by =.(variable)]
sec114_1t2 <- dcast(sec114_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))


sec115 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec115.rds")
sec115_1 <- sec115 [, (names(sec115) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec115)) ), with =FALSE]

sec115_1t <- melt (data = sec115_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec115), value = TRUE) )
sec115_1t <- sec115_1t [, value := toupper(value)] [order(variable, value)]
sec115_1t <- unique (sec115_1t [, -c("mr_no", "patient_id", "subvis")])

sec115_1t <- sec115_1t [, nrow :=1:.N, by = .(variable)]

secmax115 <- sec115_1t [, .(secmax = max(nrow)), by =.(variable)]
sec115_1t2 <- dcast(sec115_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))


sec116 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec116.rds")
sec116_1 <- sec116 [, (names(sec116) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec116)) ), with =FALSE]

sec116_1t <- melt (data = sec116_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec116), value = TRUE) )
sec116_1t <- sec116_1t [, value := toupper(value)] [order(variable, value)]
sec116_1t <- unique (sec116_1t [, -c("mr_no", "patient_id", "subvis")])

sec116_1t <- sec116_1t [, nrow :=1:.N, by = .(variable)]

secmax116 <- sec116_1t [, .(secmax = max(nrow)), by =.(variable)]
sec116_1t2 <- dcast(sec116_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))


sec119 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec119.rds")
sec119_1 <- sec119 [, (names(sec119) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec119)) ), with =FALSE]

sec119_1t <- melt (data = sec119_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec119), value = TRUE) )
sec119_1t <- sec119_1t [, value := toupper(value)] [order(variable, value)]
sec119_1t <- unique (sec119_1t [, -c("mr_no", "patient_id", "subvis")])

sec119_1t <- sec119_1t [, nrow :=1:.N, by = .(variable)]

secmax119 <- sec119_1t [, .(secmax = max(nrow)), by =.(variable)]
sec119_1t2 <- dcast(sec119_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec120 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec120.rds")
sec120_1 <- sec120 [, (names(sec120) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec120)) ), with =FALSE]

sec120_1t <- melt (data = sec120_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec120), value = TRUE) )
sec120_1t <- sec120_1t [, value := toupper(value)] [order(variable, value)]
sec120_1t <- unique (sec120_1t [, -c("mr_no", "patient_id", "subvis")])

sec120_1t <- sec120_1t [, nrow :=1:.N, by = .(variable)]

secmax120 <- sec120_1t [, .(secmax = max(nrow)), by =.(variable)]
sec120_1t2 <- dcast(sec120_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))


sec122 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec122.rds")
sec122_1 <- sec122 [, (names(sec122) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec122)) ), with =FALSE]

sec122_1t <- melt (data = sec122_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec122), value = TRUE) )
sec122_1t <- sec122_1t [, value := toupper(value)] [order(variable, value)]
sec122_1t <- unique (sec122_1t [, -c("mr_no", "patient_id", "subvis")])

sec122_1t <- sec122_1t [, nrow :=1:.N, by = .(variable)]

secmax122 <- sec122_1t [, .(secmax = max(nrow)), by =.(variable)]
sec122_1t2 <- dcast(sec122_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec123 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec123.rds")
sec123_1 <- sec123 [, (names(sec123) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec123)) ), with =FALSE]

sec123_1t <- melt (data = sec123_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec123), value = TRUE) )
sec123_1t <- sec123_1t [, value := toupper(value)] [order(variable, value)]
sec123_1t <- unique (sec123_1t [, -c("mr_no", "patient_id", "subvis")])

sec123_1t <- sec123_1t [, nrow :=1:.N, by = .(variable)]

secmax123 <- sec123_1t [, .(secmax = max(nrow)), by =.(variable)]
sec123_1t2 <- dcast(sec123_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))

sec124 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec124.rds")
sec124_1 <- sec124 [, (names(sec124) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec124)) ), with =FALSE]

sec124_1t <- melt (data = sec124_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec124), value = TRUE) )
sec124_1t <- sec124_1t [, value := toupper(value)] [order(variable, value)]
sec124_1t <- unique (sec124_1t [, -c("mr_no", "patient_id", "subvis")])

sec124_1t <- sec124_1t [, nrow :=1:.N, by = .(variable)]

secmax124 <- sec124_1t [, .(secmax = max(nrow)), by =.(variable)]
sec124_1t2 <- dcast(sec124_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))


sec126 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec126.rds")
sec126_1 <- sec126 [, (names(sec126) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec126)) ), with =FALSE]

sec126_1t <- melt (data = sec126_1, 
                   id.vars = c("mr_no", "patient_id", "subvis"), 
                   measure.vars = grep("^sec", names(sec126), value = TRUE) )
sec126_1t <- sec126_1t [, value := toupper(value)] [order(variable, value)]
sec126_1t <- unique (sec126_1t [, -c("mr_no", "patient_id", "subvis")])

sec126_1t <- sec126_1t [, nrow :=1:.N, by = .(variable)]

secmax126 <- sec126_1t [, .(secmax = max(nrow)), by =.(variable)]
sec126_1t2 <- dcast(sec126_1t, 
                    nrow ~ variable, 
                    value.var = c("value"))



# Get the unique variables and maximum number of unique values for 515 variables
t2 <- lapply(ls(pattern="secmax*"), get)
t2all <- data.table( rbindlist (t2))

# Get the unique values for 515 variables combined into 1 dataset
sect <- paste( ls(pattern="*1t2$"), collapse = "," )
t22all <- Reduce(function(...) merge(..., all = TRUE, by = "nrow"),
                 list( sec001_1t2,sec002_1t2,sec003_1t2,sec004_1t2,sec006_1t2,sec007_1t2,sec008_1t2,sec009_1t2,sec010_1t2,sec011_1t2,sec012_1t2,sec013_1t2,sec014_1t2,sec017_1t2,sec019_1t2,sec020_1t2,sec021_1t2,sec022_1t2,sec023_1t2,sec024_1t2,sec026_1t2,sec027_1t2,sec030_1t2,sec031_1t2,sec032_1t2,sec033_1t2,sec034_1t2,sec035_1t2,sec036_1t2,sec037_1t2,sec038_1t2,sec044_1t2,sec077_1t2,sec078_1t2,sec079_1t2,sec080_1t2,sec081_1t2,sec082_1t2,sec083_1t2,sec084_1t2,sec085_1t2,sec086_1t2,sec088_1t2,sec089_1t2,sec099_1t2,sec100_1t2,sec101_1t2,sec102_1t2,sec103_1t2,sec106_1t2,sec111_1t2,sec112_1t2,sec114_1t2,sec115_1t2,sec116_1t2,sec119_1t2,sec120_1t2,sec122_1t2,sec123_1t2,sec124_1t2,sec126_1t2 ) )

rm(list= ls (pattern = "sec*"))

fwrite(t2all, "D:/Hospital_data/ProgresSQL/data_chk/_unq515variables.csv")
fwrite(t22all, "D:/Hospital_data/ProgresSQL/data_chk/_unq515dist_values.csv")

saveRDS (t2all, "D:/Hospital_data/ProgresSQL/data_chk/_unq515variables.rds")
saveRDS (t22all, "D:/Hospital_data/ProgresSQL/data_chk/_unq515dist_values.rds")
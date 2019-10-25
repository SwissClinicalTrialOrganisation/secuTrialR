context("diff secuTrial exports")

ctu05_en_1 <- read_secuTrial(system.file("extdata", "sT_exports", "shortnames",
                                         "s_export_CSV-xls_CTU05_short_miss_en_utf8.zip",
                                         package = "secuTrialR"))

ctu05_en_2 <- read_secuTrial(system.file("extdata", "sT_exports", "shortnames",
                                         "s_export_CSV-xls_CTU05_short_ref_miss_en_utf8.zip",
                                         package = "secuTrialR"))

ctu05_fr <- read_secuTrial(system.file("extdata", "sT_exports", "encodings",
                                       "s_export_CSV-xls_CTU05_short_ref_miss_fr_iso8859-1.zip",
                                       package = "secuTrialR"))

bmd <- read_secuTrial(system.file("extdata", "sT_exports", "BMD",
                                  "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                  package = "secuTrialR"))




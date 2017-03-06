## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
library(eidith)
library(tidyverse)

## ----false-setup, echo = TRUE, eval = FALSE------------------------------
#  library(eidith)
#  library(tidyverse)

## ----example, echo = TRUE, eval = FALSE----------------------------------
#  ed_tests_report(status = c("Under Sequence Review", "Active testing ongoing"))

## ----report, echo = TRUE, eval = FALSE-----------------------------------
#  report <- ed_tests_report()

## ----report2, echo = TRUE, eval = FALSE----------------------------------
#  ed_fasta(report, "report.fasta")

## ----subset, echo = TRUE, eval = FALSE-----------------------------------
#  report <- ed_tests_report()
#  entero_report <- filter(report, test_rq == "Enteroviruses")
#  

## ----subset-entero, echo = TRUE, eval = FALSE----------------------------
#  ed_fasta(entero_report, "entero.fasta")

## ----subset-prot, echo = TRUE, eval = FALSE------------------------------
#  report <- ed_tests_report()
#  johne_report <- filter(report, meth == "Johne, VP1 gene")

## ----fake-virus, echo = TRUE, eval = FALSE-------------------------------
#  ed_fasta_group(report, grouping = "virus")
#  

## ----fake-protocol, echo = TRUE, eval = FALSE----------------------------
#  ed_fasta_group(report, grouping = "method")

## ----fake-both, echo = TRUE, eval = FALSE--------------------------------
#  ed_fasta_group(report, grouping = "both")

## ----excel1, echo = TRUE, eval = FALSE-----------------------------------
#  report <- ed_tests_report()
#  ed_report_excel(report, "report.csv")

## ----excel2, echo = TRUE, eval = FALSE-----------------------------------
#  ed_report_excel(entero_report, "entero_report.csv")
#  ed_report_excel(johne_report, "johne_report.csv")


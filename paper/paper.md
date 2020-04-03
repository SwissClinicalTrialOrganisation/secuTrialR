---
title: 'secuTrialR: Seamless interaction with clinical trial databases in R'
tags:
  - R software
  - clinical trials
  - data management
  - descriptive statistics
  - secuTrial
authors:
 - name: Patrick R. Wright
   orcid: 0000-0002-1153-0846 
   affiliation: 1
 - name: Alan G. Haynes
   orcid: 0000-0003-1374-081X
   affiliation: 2
 - name: Milica Markovic
   orcid: 0000-0002-6973-6679
   affiliation: 1
affiliations:
 - name: University Hospital Basel, Clinical Trial Unit, Basel, Switzerland
   index: 1
 - name: University of Bern, Clinical Trial Unit, Bern, Switzerland
   index: 2
date: 02 April 2020
bibliography: paper.bib
---

# Summary

Elementary clinical trials have been conducted for hundreds of years [@meinert1986clinical]. The most famous early example
is the proof that sailors scurvy can be cured by the consumption of citrus fruit [@lind_2014] performed by James Lind
in the 18th century. Since these initial days of clinical research, trials have significantly evolved methodically, ethically
and technologically. While it was viable and legitimate to collect clinical trials data in unversioned
spread sheets in the past, this is no longer true and digital clinical data management systems (CDMS) have taken over.
CDMS allow constraint based and version controlled data entry into a clinical trial database which ensures traceability, 
integrity and quality of study data.
There is a vast market of heterogeneous CDMS solutions each with individual advantages and limitations [@kuchinke_etal_2010].
One limitation is often the interaction with the data after it has been collected. Specifically, a CMDS may be
tailored for optimal data capture while, at least to some extent, disregarding ease of use of study data after
the conclusion of data entry. It is, however, vital that the interaction between data and data analysts takes place
fast and seamlessly in order to avoid loss of valuable time due to technical friction. This point is prominently
highlighted by the currently ongoing coronavirus pandemic [@callaway_etal_2020] in which issues have been reported
regarding the timely transfer of information for up-to-date infection counts ultimately leading to delayed
conclusions and actions [@risklayer_news]. While this is a stark example it still serves to show how
severe the influence of technical friction between digital systems can be.
To this end we developed the R statistics software package secuTrialR, which enables
seamless interaction with data collected in the commercially available CDMS secuTrial.
Next to loading the data it performs data transformation for dates, date times and categorical data to reduce
the data preparation overhead. Furthermore, secuTrialR includes standard functionalities to 
show descriptive statistics for secuTrial data exports such as study recruitment or completeness
of entered data per case report form.

# References


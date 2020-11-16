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
   affiliation: "1, 3"
 - name: Alan G. Haynes
   orcid: 0000-0003-1374-081X
   affiliation: "2, 4"
 - name: Milica Markovic
   orcid: 0000-0002-6973-6679
   affiliation: "1, 3"
affiliations:
 - name: University Hospital Basel, Clinical Trial Unit, Basel, Switzerland
   index: 1
 - name: CTU Bern, University of Bern
   index: 2
 - name: Data Management Platform of the Swiss Clinical Trial Organisation (SCTO)
   index: 3
 - name: Statistics and Methodology Platform of the Swiss Clinical Trial Organisation (SCTO)
   index: 4
date: 14 April 2020
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
One limitation can be the interaction with the data after it has been collected. Specifically, a CDMS may be
tailored for optimal data capture while, at least to some extent, disregarding ease-of-use of study data after
the conclusion of data entry. It is, however, vital that the interaction between data sources and data analysts is
fast and seamless in order to avoid loss of valuable time due to technical overhead. This point has been prominently
highlighted by the currently ongoing coronavirus pandemic [@callaway_etal_2020] in which issues have been reported
regarding the timely and complete transfer of information for the preparation of up-to-date infection
counts [@spiegel_meldeluecke; @bbc_excel]. These issues led to confusion and may have ultimately
delayed important actions. While this is a stark example, it still serves to show how severe the influence
of technical friction between digital systems can be.  

To this end we have developed the open source R statistics [@r_citation] software package `secuTrialR`, which enables
seamless interaction with data collected in the commercially available CDMS
[secuTrial](https://www.secutrial.com) (vendor [interActive Systems Berlin](https://interactive-systems.de/)).
Next to parsing and reading the data it performs data transformation for dates, date times and categorical data
to reduce the data preparation overhead and allow a swift transition into the analytical phase.
Furthermore, `secuTrialR` includes standard functionalities to
show descriptive statistics such as study recruitment or completeness of entered data per case report form
for secuTrial data exports.

# Statement of need

Due to the size and complexity of clinical trial and registry databases, technical friction during the initial interaction
with data exported from secuTrial can be expected. Our own first hand experience has revealed that this overhead can sometimes
significantly redirect scarce time and energy away from analysis and towards data management. However, if possible, the amount of time
spent on data management should be as small as possible. The use of `secuTrialR` leads to a pronounced reduction of time
necessary for data management, enables swift quantitative analyses through preimplemented functionalities and, most importantly,
standardizes the interaction with data exports from secuTrial, thus allowing robust and reproducible science.

While some CDMS provide APIs (e.g. REDCap [@Harris2009; @Harris2019]) or Open Database Connectivity (ODBC) connections (e.g. 
[2mt's WebSpirit](http://www.2mt-software.de)) to download data easily, using secuTrial's SOAP API involves querying
individual datapoints. This results in an extraordinarily high number of 
queries even to download a relatively small database, and high demand on servers. As such, approaches such as those 
for REDCap (e.g. the [REDCapR](https://CRAN.R-project.org/package=REDCapR) package which can interface to REDCap's REST 
API and download all data in a single query, but does no data preparation) are not suitable for secuTrial. 
Another approach is to parse data exported manually from websites (e.g. the [ox](https://github.com/acobos/ox) package for importing [OpenClinica](https://www.openclinica.com) exports into R). This approach is used in `secuTrialR`.

# Design

All secuTrial data exports share a certain common technical structure independent of the specific database at hand.
In `secuTrialR` we make use of this information to build an S3 object of class `secuTrialdata`, which is a list, while the
data is being read into R. All downstream functions implemented in `secuTrialR` expect a `secuTrialdata` object as input
but custom analyses with other compenents of R statistics are also an option (see Figure 1).
While editing the `secuTrialdata` object is technically possible, this is not advisable.
Rather it should be treated as raw data archive from which data can be extracted for analysis. However, if necessary,
it is possible to extract subsets of `secuTrialdata` objects with the `subset_secuTrial()` function and return
intact `secuTrialdata` objects. The individual elements of the secuTrialdata object can be accessed via regular list 
access operations or the `as.data.frame()` method, which assigns all objects to an environment of choice.

![secuTrialR information flow](secuTrialR_information_flow.png)
Figure 1: Information flow from secuTrial to R statistics and within R. Arrows indicate the 
direction from gray towards black. "..." indicates further functions working with `secuTrialdata`
objects.

# Availability

`secuTrialR` is available on [GitHub](https://github.com/SwissClinicalTrialOrganisation/secuTrialR),
[CRAN](https://cran.r-project.org/package=secuTrialR), [Anaconda Cloud](https://anaconda.org/conda-forge/r-secutrialr) and
should be functional on all major operating systems.

# Dependencies

The development of `secuTrialR` made extensive use of the `tidyverse` [@tidyverse_cit] and greatly benefited from
the `devtools` package [@devtools_cit] and `RStudio` [@rstudio_cit]. Furthermore, `tcltk` and `igraph` [@igraph_cit]
are incorporated.

# interActive Systems statement

InterActive Systems (iAS) has given permission for the open source development of this software
package but accepts no responsibility for the correctness of any functionalities within.

iAS has read and approved this manuscript.

# Acknowledgements

The authors thank Pascal Benkert, Nicole Bruni, Gilles Dutilh, Olivia Ebner, Stefanie von Felten, 
Thomas Fabbro, Inessa Kraft, Arnaud Künzi, Daniel Lengwiler, Armando Lenz, Pia Neuschwander, Henry Owusu, Hans Rock, Claudia Rokitta,
Marie Roumet, Constantin Sluka, Klaus Steigmiller, Suvitha Subramaniam, Miriam Wegmann, Laura Werlen and Thomas Zumbrunn for ideas,
testing and constructive feedback on the `secuTrialR` package. We also thank [Michael Sachs](https://github.com/sachsmc) 
and [Francisco Estupiñán-Romero](https://github.com/pacoramon) for kindly reviewing this manuscript and the R package and making additional 
recommendations, and [Charlotte Soneson](https://github.com/csoneson) for acting as editor.
Furthermore, the authors thank the State Secretariat of Education, Research and Innovation and the Swiss National
Science Foundation for the funding of this project and the Swiss Clinical Trial Organisation for its ongoing support.

# Conflict of interest

The authors are not employees but customers of interActive Systems (iAS). The authors therefore declare
no conflict of interest.

# References

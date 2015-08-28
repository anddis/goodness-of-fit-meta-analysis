### Goodness of fit for dose–response meta-analysis of binary outcomes

**Andrea Discacciati<sup>1,2</sup>, Alessio Crippa<sup>1,2</sup>, and Nicola Orsini<sup>1,2</sup>**

_<sup>1</sup>Unit of Nutritional Epidemiology, Institute of Environmental Medicine, Karolinska Institutet, Stockholm, Sweden
<sup>2</sup>Unit of Biostatistics, Institute of Environmental Medicine, Karolinska Institutet, Stockholm, Sweden_

---

This repository contains the R code and the datasets needed to reproduce  Example 1 (Lactose intake and risk of ovarian cancer) and Example 2 (Coffee consumption and risk of stroke) contained in the manuscript "Goodness of fit for dose–response meta-analysis of binary outcomes", currently under peer review.

Note: version 2.0.0 of the R package `dosresmeta` is required. It can be downloaded from http://github.com/alecri/dosresmeta or installed directly from R:
        
    install.packages("devtools")
    devtools::install_github("alecri/dosresmeta")
  
Quick description of the files contained in this repository:
* `data/ovarian.txt`: dataset for Example 1 
* `data/stroke.txt`: dataset for Example 2
* `ex_lactose_ovarian.R`: R code for Example 1
* `ex_coffee_stroke.R`: R code for Example 2
* `functions.R`: additional functions used in the examples

---

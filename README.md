### Goodness of fit for dose–response meta-analysis of binary outcomes

**[Andrea Discacciati](http://anddis.github.io/)<sup>1,2</sup>, [Alessio Crippa](http://alecri.github.io)<sup>1,2</sup>, and [Nicola Orsini](http://nicolaorsini.altervista.org)<sup>1,2</sup>**

_<sup>1</sup>Unit of Nutritional Epidemiology, Institute of Environmental Medicine, Karolinska Institutet, Stockholm, Sweden
<sup>2</sup>Unit of Biostatistics, Institute of Environmental Medicine, Karolinska Institutet, Stockholm, Sweden_

---

Data and R code to reproduce the analyses in the paper "Goodness of fit for dose–response meta-analysis of binary outcomes" are available in this repository.

Quick description of the files contained in this repository:
* `data/ovarian.txt`: dataset for Example 1
* `data/stroke.txt`: dataset for Example 2
* `ex_lactose_ovarian.R`: R code for Example 1
* `ex_coffee_stroke.R`: R code for Example 2
* `functions.R`: additional functions used in the examples
* `ex_lactose_ovarian-2.R`: R code for Example 1, added 2017-08-17 (uses the `gof` function from `dosresmeta`, see below)
* `ex_coffee_stroke-2.R`: R code for Example 2, added 2017-08-17 (uses the `gof` function from `dosresmeta`, see below)

Note: version 2.0.0 of the R package `dosresmeta` is required. It can be downloaded from http://github.com/alecri/dosresmeta or installed from R using the following command lines:

    install.packages("devtools")
    devtools::install_github("alecri/dosresmeta")

**Edit (2017-08-17)**

The R package `dosresmeta` has been updated on CRAN to verison 2.0.0 and can be installed using the following command line:

	install.packages("dosresmeta")

Furthermore, `dosresmeta` now includes the `gof` function that implements the goodness of fit tools presented in our paper. Type `?gof` in R for more information.

The new function is illustrated in the files `ex_lactose_ovarian-2.R` and `ex_coffee_stroke-2.R`, which reproduce the examples in the paper.

The code in `ex_lactose_ovarian.R`, `ex_coffee_stroke.R` and `functions.R` continues to work but is now obsolete.

---

Reference: A.Discacciati, A.Crippa, N.Orsini. _Goodness of fit for dose–response meta-analysis of binary outcomes_. Research Synthesis Methods. 10.1002/jrsm.1194. http://onlinelibrary.wiley.com/doi/10.1002/jrsm.1194/pdf

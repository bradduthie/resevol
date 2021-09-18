![](https://raw.githubusercontent.com/bradduthie/resevol/6b5f68620fb1f45f0bfdc5aa4e853a8305d04bf5/notebook/images/resevol_logo.png)


Resistance Evolution (resevol) simulation package
--------------------------------------------------------------------------------

**The resevol R package is a tool for simulating social-ecological individual-based models (IBMs) for the ecology and evolution of agricultural pest species. Simulations model a spatially explicit landscape broken down into one or more independent farms on which one of up to 10 crops can be grown and one of up to 10 pesticides can be applied. Crop and pesticide application can be rotated during a simulation at different spatial and temporal scales to simulate the effects of heterogeneity of pest environment. Haploid or diploid pest genomes are modelled explicitly with an arbitrary number of loci that map to any number of traits. This mapping of loci to traits can be set with a pre-specified trait correlation structure, which is found using an evolutionary algorithm run using the mine_gmatrix() function. Individual pest traits can affect movement, reproduction, feeding, pesticide tolerance, metabolism, and other individual characteristics. Simulations of pest populations dynamics run with the run_sim_farm() function can track individual pest locations, pedigree, behaviour, and trait evolution.**

--------------------------------------------------------------------------------

*This software was developed as part of the project for [Enhancing Diversity to Overcome Resistance Evolution](https://gtr.ukri.org/projects?ref=BB%2FS018956%2F1) (ENDORSE) led by [Dr Luc Bussi&egrave;re](https://lucbussiere.com/), [Dr Ricardo Polanczyk](https://www.fcav.unesp.br/#!/docentes/ricardo-antonio-polanczyk/), and [Dr Matthew Tinsley](https://www.stir.ac.uk/people/256411) (Co-Investigators: [Nils Bunnefeld](https://www.stir.ac.uk/people/257162), [Yelitza Colmenarez](https://www.cabi.org/cabi-people/yelitza-colmenarez/), [Nat&aacute;lia Corniani](https://www.cabi.org/cabi-people/natalia-corniani/), [Renata de Lima](https://bv.fapesp.br/en/pesquisador/46301/renata-de-lima/), [Brad Duthie](https://bradduthie.github.io), [Steve Edgington](https://www.cabi.org/cabi-people/steven-edgington/), [Leonardo Fraceto](https://bv.fapesp.br/en/pesquisador/3059/leonardo-fernandes-fraceto/), [Belinda Luke](https://www.cabi.org/cabi-people/belinda-luke/), and [Rosie Mangan](https://sites.google.com/site/rosemarygmangan/home)). The ENDORSE project is a joint Newton funded international partnership between the Biotechnology and Biological Sciences Research Council (BBSRC) in the UK and the S&atilde;o Paulo Research Foundation (FAPESP) in Brazil under BBSRC award reference BB/S018956/1 and FAPESP award reference 2018/21089-3. ENDORSE is a partnership among Universidade Estadual Paulista (UNESP), the University of Stirling (UoS), and the Centre for Agricultural and Biosciences International (CABI). This software package is authored by [Brad Duthie](https://github.com/bradduthie) and [Rose McKeon](https://github.com/rosemckeon), and is maintained by [Brad Duthie](https://github.com/bradduthie).*

--------------------------------------------------------------------------------


## Installation

**Install from CRAN**

To install [this package](https://CRAN.R-project.org/package=resevol) from CRAN.

```
install.packages("resevol")
```

**Install with GitHub**

To install this package from GitHub, make sure that the `devtools` library is installed.

```
install.packages("devtools")
library(devtools)
```

Use `install_github` to install using `devtools`.

```
install_github("bradduthie/resevol")
```


## Documentation

To get started with the resevol package, we strongly recommend first reading through the **Get Started** vignette.



# gdxtools

The gdxtools package converts data (parameter or variables) from a GDX file (produced by the GAMS software) into a data.frame. It also provides a function to get information on a GDX file. This package is based on the R interface provided by GAMS software [gdxrrw](http://support.gams.com/doku.php?id=gdxrrw:interfacing_gams_and_r)

## Installation

```R
install.packages("gdxtools.zip",repos=NULL)
```

or

```R
library("devtools")
install_github('lolow/gdxtools')
```

## Usage

```R
library(gdxtools)

# define a gdx
mygdx <- gdx('results.gdx')

# get information on all items in the gdx
all_items(mygdx)

# create a data.frame from a parameter
travel_cost <- mygdx["travel_cost"]

# create a data.frame from the marginal value of a variable
m_travel_time <- mygdx["travel_time", field="m"]

```

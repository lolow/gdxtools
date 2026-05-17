# gdxtools

`gdxtools` reads and writes GDX files (the GAMS database exchange format) from R, exposing parameters, sets, variables and equations as plain data.frames with domain columns + a `value` column. From v1.0.0 onward the package is backed by [gamstransfer](https://cran.r-project.org/package=gamstransfer), the GDX I/O package maintained by GAMS Software GmbH. No compiled code ships with `gdxtools`.

## Installation

```R
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("lolow/gdxtools")
```

`gamstransfer` (CRAN) is the only required dependency. A working GAMS installation is needed only if you call `gams()` to run a `.gms` file.

## Reading a GDX

```R
library(gdxtools)

mygdx <- gdx("results.gdx")

print(mygdx)                    # <results.gdx, 8 symbols>
print(mygdx$parameters$name)    # parameters available
all_items(mygdx)                # lists of names by symbol type

# Parameter — returns a data.frame with domain columns + value
travel_cost <- mygdx["travel_cost"]

# Variable / equation fields: "l" (level, default), "m" (marginal),
# "lo" (lower bound), "up" (upper bound)
lo_travel_time     <- mygdx["travel_time",     field = "lo"]
m_time_constraint  <- mygdx["time_constraint", field = "m"]

# Extract the same item from several GDX files
allparam <- batch_extract("myparam", files = c("test1.gdx", "test2.gdx"))
```

## Writing a GDX

```R
# Parameters
param1 <- data.frame(x = c("1","2","4","8"), value = 1:4)
attributes(param1) <- c(attributes(param1), gams = "definition of parameter 1")
param2 <- data.frame(a = c("london","paris","tahiti"), value = c(50, 0.2, 1e-2))
write.gdx("test.gdx", params = list(param1 = param1, param2 = param2))

# Sets — pass each one as a data.frame; column names become its dimensions
myset1 <- data.frame(a = c("london","paris","tahiti"))
myset2 <- data.frame(a = c("london","paris","tahiti"),
                     b = c("tahiti","tahiti","paris"))
write.gdx("test1.gdx", sets = list(city = myset1, road = myset2))

# Variables — give level / lower / upper as separate named lists; missing
# entries fall back to free-var defaults (level 0, lower -Inf, upper +Inf)
var_l  <- data.frame(a = c("london","paris","tahiti"), value = 0.2)
var_lo <- data.frame(a = c("london","paris","tahiti"), value = 1e-2)
var_up <- data.frame(a = c("london","paris","tahiti"), value = 50)
write.gdx("test2.gdx",
          vars_l  = list(travel_time = var_l),
          vars_lo = list(travel_time = var_lo),
          vars_up = list(travel_time = var_up))

# Compressed output
write.gdx("test.gdx", params = list(param1 = param1), compress = TRUE)

# write2.gdx — alias of write.gdx for params + sets only (kept for
# backward compatibility; both use the same fast path)
write2.gdx("test.gdx", params = list(param1 = param1, param2 = param2))
```

### Policies for NA values and duplicate keys

`write.gdx()` exposes two policy arguments that default to the legacy v0.7 semantics so existing code keeps producing bit-identical output. Opt in when you want the alternate behaviour:

```R
# NA / NaN values in parameter `value` columns
write.gdx("out.gdx", params = list(p = p), na = "drop")   # default: silently drop
write.gdx("out.gdx", params = list(p = p), na = "keep")   # preserve as GAMS NA
write.gdx("out.gdx", params = list(p = p), na = "error")  # stop on any NA

# Duplicate index keys
write.gdx("out.gdx", params = list(p = p), dup = "first")  # default: keep first
write.gdx("out.gdx", params = list(p = p), dup = "last")   # keep last (GAMS-process semantic)
write.gdx("out.gdx", params = list(p = p), dup = "error")  # stop on duplicates
```

A warning fires when rows are actually dropped so the count is always audible.

## Migrating from 0.7.x

| 0.7 call                                                                                 | 1.0 equivalent                                |
|-----------------------------------------------------------------------------------------|-----------------------------------------------|
| `write.gdx(..., removeLST = FALSE, usetempdir = FALSE)`                                  | arguments accepted but ignored (no .gms/.lst) |
| `write.gdx(..., digits = 16)`                                                            | accepted but ignored (full double precision)  |
| `rgdx`, `wgdx`, `gdxInfo`, `rgdx.param`, `rgdx.set`, `rgdx.scalar` (low-level gdxrrw)    | removed — use `gamstransfer::Container` directly |
| `igdx()`, `gams()`                                                                       | kept as thin shims (PATH-based GAMS discovery + `system2` runner) |

A sparse parameter no longer densifies on read: only entries actually stored in the GDX come back. Variables and equations are unaffected.

See [`NEWS.md`](NEWS.md) for the complete list of behaviour changes and bug fixes in 1.0.0.

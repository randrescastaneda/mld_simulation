library(wbpip)
library(data.table)
library(collapse)
library(ggplot2)

#--------- read subfunctions ---------
source(here::here("R/utils.R"))


minval <- 1
maxval <- 1e5
mn     <- (maxval - minval)/2
size   <- 1e6 # data size


#--------- Original data ---------
set.seed(42)
wf_unif <- sort(runif(size, min= minval, max= maxval))
wf_lnor <- sort(sample(rlnorm(size, meanlog = 2),
                       replace = TRUE))
weight <-  sample(runif(size, min = 1, max = 100))

wf_lnor <- (wf_lnor*fmean(x = wf_unif, w = weight)) / fmean(x = wf_lnor, w = weight)


#--------- parameters ---------
sh <- c(1:10, 15, 20, 25, 30, 40, 50)/100  # shares of zeros
sc <- c(.5, 1, 2, 5, 10, 100, 1e3, 1e4, 1e5) # Scales
rp <- c("ignore", "1", "min", "shr") # replaces


params <- tidyr::expand_grid(sh, sc, rp)
params <- as.list(params)


mld_unif <- purrr::pmap_df(params,
                           .f = new_mld,
                           welfare = wf_unif,
                           weight  = weight)

mld_unif$dist <-  "unif"

mld_lnor <- purrr::pmap_df(params,
                           .f = new_mld,
                           welfare = wf_lnor,
                           weight  = weight)
mld_lnor$dist <-  "lnor"

pushover("MLDs ready")

dt <- rbindlist(list(mld_unif, mld_lnor),
                use.names = TRUE,
                fill = TRUE)

dt[,
  `:=`(
     sh = as.factor(share),
     sc = as.factor(scale)
   )
   ]

gr <- ggplot(dt[share <= .2],
       aes(x = sh,
           y = sc,
           fill = mld)) +
  geom_tile() +
  facet_grid(dist~replace)

plotly::ggplotly(gr)


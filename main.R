library(tercen)
library(dplyr)

do.spline <- function(df, spar, all_knots, n_knots) {
  m <- smooth.spline(x = df$.x, y = df$.y, spar = spar, all.knots = all_knots, nknots = n_knots)
  x_fit = df$.x
  y_fit = fitted(m)
  y_res = resid(m)
  df_out <- data.frame(
    .ri = df$.ri[1],
    .ci = df$.ci[1],
    x_fit,
    y_fit,
    y_res
  )
  return(df_out)
}

(ctx = tercenCtx())


spar = 0.7
if(!is.null(ctx$op.value('smooth_par'))) spar <- as.numeric(ctx$op.value('smooth_par'))
all_knots = FALSE
if(!is.null(ctx$op.value('all_knots'))) all_knots <- as.numeric(ctx$op.value('all_knots'))
n_knots = 10
if(!is.null(ctx$op.value('n_knots'))) n_knots <- as.numeric(ctx$op.value('n_knots'))

ctx %>% 
  select(.ci, .ri, .x, .y) %>%
  group_by(.ci, .ri) %>%
  do(do.spline(., spar, all_knots, n_knots)) %>% 
  ctx$addNamespace() %>%
  ctx$save()


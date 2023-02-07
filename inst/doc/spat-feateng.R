## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 6, 
  fig.width = 6, 
  fig.align = "center"
)

## -----------------------------------------------------------------------------
library(ptools)
library(sp)     #for plotting functions

# Plot shootings in NYC
plot(nyc_bor)
plot(nyc_shoot,pch='.',add=TRUE)

## -----------------------------------------------------------------------------
hm_grid <- prep_grid(nyc_bor,5280/2)

# Plot shootings in NYC
plot(nyc_bor)
plot(hm_grid,col='grey',border='white',add=TRUE)

## -----------------------------------------------------------------------------
lim_grid <- prep_grid(nyc_bor,5280/2,point_over=nyc_shoot)

# Plot shootings in NYC
plot(nyc_bor)
plot(lim_grid,col='grey',border='white',add=TRUE)

## -----------------------------------------------------------------------------
lim_grid2 <- prep_grid(nyc_bor,5280/2,clip_level = 0.3)

# Plot shootings in NYC
plot(nyc_bor)
plot(lim_grid2,col='grey',border='white',add=TRUE)

# See how many fewer than original
print(nrow(hm_grid))   #original
print(nrow(lim_grid2)) #no dongles less than 30%
print(nrow(lim_grid))  #only overlap 1 shooting

## -----------------------------------------------------------------------------
hex_grid <- prep_hexgrid(nyc_bor,(5280/2)^2,clip_level = 0.3)

# Plot shootings in NYC
plot(hex_grid,col='lightblue',border='white')
plot(nyc_bor,lwd=2,add=TRUE)

## -----------------------------------------------------------------------------
hex_lim <- prep_hexgrid(nyc_bor,(5280/2)^2,point_over=nyc_shoot,point_n=4)

# Plot shootings in NYC
plot(hex_lim,col='lightblue',border='white')
plot(nyc_bor,lwd=2,add=TRUE)

## -----------------------------------------------------------------------------
# Make a smaller sample of the liquor stores in NYC
liq_samp <- nyc_liq[sample(rownames(nyc_liq@data),20),]
# Buffer, it is hard to view all the little islands
nyc_buff <- buff_sp(nyc_bor,5000)
# Now create Voronoi/Thiessen areas
liq_vor <- vor_sp(nyc_buff,liq_samp)

# Plot to show off
plot(liq_vor,col='tan',border='white')
plot(liq_samp,add=TRUE)
plot(nyc_buff,border='black',lwd=2,add=TRUE)

## -----------------------------------------------------------------------------
hex_grid$shoot_cnt <- count_xy(hex_grid,nyc_shoot)
spplot(hex_grid,zcol='shoot_cnt')

## -----------------------------------------------------------------------------
print(sum(hex_grid$shoot_cnt))
print(nrow(nyc_shoot))

## -----------------------------------------------------------------------------
covid_date <- as.Date('03/22/2020', format="%m/%d/%Y")
nyc_shoot$pre <- 1*(nyc_shoot$OCCUR_DATE < covid_date)
nyc_shoot$post <- 1*(nyc_shoot$OCCUR_DATE >= covid_date)
hex_grid$shoot_pre <- count_xy(hex_grid,nyc_shoot,weight='pre')
hex_grid$shoot_post <- count_xy(hex_grid,nyc_shoot,weight='post')
plot(hex_grid$shoot_pre,hex_grid$shoot_post)

## -----------------------------------------------------------------------------
hex_grid$kliq_1mile <- kern_xy(hex_grid,nyc_liq,5280)
spplot(hex_grid,zcol='kliq_1mile')

## -----------------------------------------------------------------------------
hex_grid$dist_cafe <- dist_xy(hex_grid,nyc_cafe)
spplot(hex_grid,zcol='dist_cafe')

## -----------------------------------------------------------------------------
dist_shoot <- dist_xy(nyc_cafe,nyc_shoot,fxy=c('X_COORD_CD','Y_COORD_CD'))
summary(dist_shoot)

## -----------------------------------------------------------------------------
hex_grid$dcnt_cafe1mile <- dcount_xy(hex_grid,nyc_cafe,5280)
plot(hex_grid$dist_cafe,hex_grid$dcnt_cafe1mile,xlim=c(0,10000))
abline(v=5280) # not perfect, because of difference between polygon distance vs centroid distance


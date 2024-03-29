#' Creates buffer of sp polygon object
#'
#' Creates buffer of sp polygon object. Intended to replace raster::buffer, which relies on rgeos
#'
#' @param area SpatialPolygon or SpatialPolygonDataFrame that defines the area
#' @param radius scaler for the size of the buffer (in whatever units the polygon is projected in)
#' @param dissolve boolean (default TRUE), to dissolve into single object, or leave as multiple objects
#'
#' @details Under the hood, this converts sp objects into sf objects and uses `st_buffer`.
#' When `dissolve=TRUE`, it uses `st_union(area)` and then buffers.
#'
#' @returns
#' A SpatialPolygonDataFrame object (when dissolve=FALSE), or a SpatialPolygon object (when dissolve=TRUE)
#' @export
#' @examples
#' \donttest{
#' library(sp) #for sp plot methods
#' # large grid cells
#' data(nyc_bor)
#' res <- buff_sp(nyc_bor,7000)
#' plot(nyc_bor)
#' plot(res,border='BLUE',add=TRUE)
#' 
#' # When dissolve=FALSE, still returns individual units
#' # that can overlap
#' res2 <- buff_sp(nyc_bor,7000,dissolve=FALSE)
#' plot(res2)
#' }
buff_sp <- function(area,radius,dissolve=TRUE){
    area_sf <- sf::st_as_sf(area)
    if (dissolve){
        area_sf <- sf::st_union(area_sf)
    }
    buff_sf <- sf::st_buffer(area_sf,radius)
    buff_sp <- sf::as_Spatial(buff_sf)
    return(buff_sp)
}

#' Creates vector grid cells over study area
#'
#' Creates grid cells of given size over particular study area.
#'
#' @param outline SpatialPolygon or SpatialPolygonDataFrame that defines the area to draw grid cells over
#' @param size scaler for the size of the grid cells (one side), in whatever units the outline is in
#' @param clip_level , you can clip grid cells if they are not entirely inside the outlined area, defaults to `0`
#' so any cells at least touching are included
#' @param point_over default `NULL`, but can pass in SpatialPoints and will only include grid cells that have at least one point
#' @param point_n default 0, only used if passing in `point_over`. Will return only grid cells with greater than `point_n` points
#'
#' @details This generates a vector grid over the study area of interest. Intentionally working with vector data for use with 
#' other feature engineering helper functions (that can pass in X/Y). 
#' @returns
#' A SpatialPolygonDataFrame object with columns
#'  - `id`, integer id value (not the same as row.names!)
#'  - `x`, x centroid of grid cell
#'  - `y`, y centroid of grid cell
#'  - `cover`, proportion that grid cell is covered by `outline`
#'  - `count`, optional (only if you pass in `point_over`)
#' @export
#' @examples
#' \donttest{
#' library(sp) #for sp plot methods
#' # large grid cells
#' data(nyc_bor)
#' res <- prep_grid(nyc_bor,5000)
#' plot(nyc_bor)
#' plot(res,border='BLUE',add=TRUE)
#' 
#' # clipping so majority of grid is inside outline
#' res <- prep_grid(nyc_bor,2000,clip_level=0.5)
#' plot(nyc_bor)
#' plot(res,border='BLUE',add=TRUE)
#' 
#' # only grid cells that have at least one shooting
#' data(nyc_shoot)
#' res <- prep_grid(nyc_bor,2000,clip_level=0,nyc_shoot)
#' plot(nyc_bor)
#' plot(res,border='RED',add=TRUE)
#' }
#
#' @references
#' Wheeler, A. P. (2018). The effect of 311 calls for service on crime in DC at microplaces. *Crime & Delinquency*, 64(14), 1882-1903.
#' 
#' Wheeler, A. P., & Steenbeek, W. (2021). Mapping the risk terrain for crime using machine learning. *Journal of Quantitative Criminology*, 37(2), 445-480.
prep_grid <- function(outline, size, clip_level=0, point_over=NULL, point_n=0){
    # Creating the initial raster full grid
    base_raster <- raster::raster(ext=raster::extent(outline), res=size)
    raster::projection(base_raster) <- raster::crs(outline)
    # Getting grid cells inside out the outline
    mask_raster <- raster::rasterize(outline, base_raster, getCover=TRUE)
    base_poly <- raster::rasterToPolygons(mask_raster,dissolve=FALSE)
    sel_poly <- base_poly[base_poly$layer > clip_level,]
    # Adding in XY coordinates
    xy_df <- sp::coordinates(sel_poly)
    sel_poly$x <- xy_df[,1]
    sel_poly$y <- xy_df[,2]
    sel_poly$id <- 1:nrow(sel_poly)
    sel_poly <- sel_poly[,c('id','x','y','layer')]
    names(sel_poly)[4] <- 'cover'
    # If you pass in spatial points, also extract those covered
    # By at least one point (and get counts of points)
    if (!is.null(point_over)){
        pt_sf <- sf::st_as_sf(point_over)
        poly_sf <- sf::st_as_sf(sel_poly)
        ch <- lengths(sf::st_intersects(poly_sf, pt_sf))
        sel_poly$count <- ch
        # selecing out minimal point number
        sel_poly <- sel_poly[c(sel_poly$count > point_n),]
        sel_poly$id <- 1:nrow(sel_poly)
        names(sel_poly)[5] <- 'count'
    }
    return(sel_poly)
}

# How should I expose this function?
hex_area <- function(side){
  area <- 6 * (  (sqrt(3)*side^2)/4 )
  return(area)
}

#' Creates hexagon grid cells over study area
#'
#' Creates hexagon grid cells of given area over particular study area.
#'
#' @param outline SpatialPolygon or SpatialPolygonDataFrame that defines the area to draw hexgrid cells over
#' @param area scaler for the area of the grid cells in whatever units the outline is in
#' @param clip_level , you can clip grid cells if they are not entirely inside the outlined area, defaults to `0`
#' so any cells at least touching are included. Specify as proportion (so should not be greater than 1!)
#' @param point_over default `NULL`, but can pass in SpatialPoints and will only include grid cells that have at least one point
#' @param point_n default 0, only used if passing in `point_over`. Will return only grid cells with greater than `point_n` points
#'
#' @details This generates a vector hex grid over the study area of interest. Hexgrids are sometimes preferred over square grid cells to  
#' prevent aliasing like artifacts in maps (runs of particular values).
#' 
#' @returns
#' A SpatialPolygonDataFrame object with columns
#'  - `id`, integer id value (not the same as row.names!)
#'  - `x`, x centroid of grid cell
#'  - `y`, y centroid of grid cell
#'  - `cover`, optional (only if clip_level > 0) proportion that grid cell is covered by `outline`
#'  - `count`, optional (only if you pass in `point_over`), total N of points over
#' @export
#' @examples
#' \donttest{
#' library(sp) #for sp plot methods
#' #Base example, some barely touch
#' hnyc <- prep_hexgrid(nyc_bor,area=20000^2)
#' plot(hnyc)
#' plot(nyc_bor,border='red',add=TRUE)
#' #Example clipping hexagons that have dongle hexagons
#' hex_clip <- prep_hexgrid(nyc_bor,area=20000^2,clip_level=0.3)
#' plot(hex_clip,border='blue')
#' plot(nyc_bor,border='red',add=TRUE)
#' summary(hnyc)
#' 
#' #Example clipping hexagons with no overlap crimes
#' hnyc <- prep_hexgrid(nyc_bor,area=4000^2,point_over=nyc_shoot)
#' plot(hnyc)
#' plot(nyc_shoot,pch='.',add=TRUE)
#' }
#'
#' @references
#' Circo, G. M., & Wheeler, A. P. (2021). Trauma Center Drive Time Distances and Fatal Outcomes among Gunshot 
#' Wound Victims. *Applied Spatial Analysis and Policy*, 14(2), 379-393.
#' 
prep_hexgrid <- function(outline,area,clip_level=0,point_over=NULL,point_n=0){
    # Convert area to side
    dim_hex <- hex_dim(area)
    # Buffer outline by just over the size
    width_hex <- dim_hex[2]
    buff_len <- width_hex*1.1
    buff <- buff_sp(outline,buff_len)
    # Get Hexagon sampling
    #hex_pts <- sp::spsample(buff,cellsize=width_hex,type='hexagonal')
    #hex_pols <- sp::HexPoints2SpatialPolygons(hex_pts)
    buff_sf <- sf::st_as_sf(buff)
    outline_sf <- sf::st_union(sf::st_as_sf(outline))
    hex_sf <- sf::st_make_grid(buff_sf,cellsize=width_hex,what = "polygons",square = FALSE)
    # Get over original
    dist_hex <- methods::as(sf::st_distance(hex_sf,outline_sf),"numeric")
    hex_orig <- sf::as_Spatial(hex_sf)
    hex_orig <- hex_orig[c(dist_hex < 0.001),]
    # Get over original
    coord_hex <- sp::coordinates(hex_orig)
    tot_n <- dim(coord_hex)[1]
    hex_orig$id <- 1:tot_n #turns into SpatialDataFrame
    hex_orig$xG <- coord_hex[,1]
    hex_orig$yG <- coord_hex[,2]
    hex_orig$area <- area
    hex_sf <- sf::st_as_sf(hex_orig)
    # If you pass in spatial points, also extract those covered
    # By at least one point (and get counts of points)
    if (!is.null(point_over)){
        pt_sf <- sf::st_as_sf(point_over)
        ch <- lengths(sf::st_intersects(hex_sf, pt_sf))
        hex_orig$count <- ch
        # selecing out minimal point number
        hex_orig <- hex_orig[c(hex_orig$count > point_n),]
        hex_orig$id <- 1:nrow(hex_orig)
        last_nm <- length(names(hex_orig))
        names(hex_orig)[last_nm] <- 'count'
    }
    # Probably want this to be 0 if using point clip
    # If clip_level > 0, calculate intersection area
    if (clip_level > 0){
        # Tiny buffer to fix weird polys and collapse to one
        buff_tiny <- buff_sp(outline,0.001)
        tot_n <- nrow(hex_orig)
        # For not relying on rgeos, convert to terra
        buff_sf <- sf::st_as_sf(buff_tiny)
        rc <- sf::st_intersection(hex_sf,buff_sf)
        res_inter <- sf::st_area(rc)
        res_inter <- methods::as(res_inter,"numeric")
        res_inter <- res_inter/area
        hex_orig$cover <- res_inter
        # Selecting out area over
        hex_orig <- hex_orig[c(hex_orig$cover > clip_level),]
        hex_orig$id <- 1:nrow(hex_orig)
    }
    # Giving strange x redundant error earlier
    names(hex_orig)[2] <- "x"
    names(hex_orig)[3] <- "y"
    return(hex_orig)
}


#' Distance to nearest based on centroid
#'
#' Given a base X/Y dataset, calculates distance to nearest for another feature X/Y dataset
#'
#' @param base base dataset (eg gridcells)
#' @param feat feature dataset (eg another crime generator)
#' @param bxy vector of strings that define what the base xy fields are defined as, defaults `c('x','y')`
#' @param fxy vector of strings that define what the base xy fields are defined as, defaults `c('x','y')`
#'
#' @details This generates a distance to nearest, based on the provided x/y coordinates (so if using polygons pass the centroid).
#' This uses kd-trees from RANN, so should be reasonably fast. But I do no projection checking, that is on you. You should not
#' use this with spherical coordinates. Useful for feature engineering for crime generators.
#' @returns
#' A vector of distances from base dataset xy to the nearest feature xy
#' @export
#' @examples
#' \donttest{
#' data(nyc_bor); data(nyc_cafe)
#' gr_nyc <- prep_grid(nyc_bor,15000,clip_level=0.3)
#' gr_nyc$dist_cafe <- dist_xy(gr_nyc,nyc_cafe)
#' head(gr_nyc@data)
#' sp::spplot(gr_nyc,zcol='dist_cafe')
#' }
#'
#' @seealso 
#' [count_xy()] for counting points inside of base polygon
#'
#' [dcount_xy()] for counting points within distance of base polygon
#'
#' [kern_xy()] for estimating gaussian density of points for features at base polygon xy coords
#'
#' [bisq_xy()] for estimate bi-square kernel of points for features at base polygon xy coords
#'
#' [idw_xy()] for estimate inverese distance weighted of points for features at base polygon xy coords 
#' 
#' @references
#' Caplan, J. M., Kennedy, L. W., & Miller, J. (2011). Risk terrain modeling: Brokering criminological theory and GIS methods for crime forecasting. *Justice Quarterly*, 28(2), 360-381.
#'
#' Wheeler, A. P., & Steenbeek, W. (2021). Mapping the risk terrain for crime using machine learning. *Journal of Quantitative Criminology*, 37(2), 445-480.
#'
dist_xy <- function(base,feat,bxy=c('x','y'),fxy=c('x','y')){
    # I do not check to make sure projection is same
    # To allow passing in data frames
    bclass <- substring(class(base)[1],1,2)
    fclass <- substring(class(feat)[1],1,2)
    if (bclass == 'Sp'){ bdat <- base@data[,bxy]
    } else { bdat <- base[,bxy] }
    if (fclass == 'Sp'){ fdat <- feat@data[,fxy]
    } else { fdat <- feat[,bxy] }
    resnn <- RANN::nn2(data=fdat,query=bdat,k=1)
    return(c(resnn$nn.dists))
}


#' Count of points in polygon
#'
#' Given a base X/Y dataset, calculates number of feature points that fall inside
#'
#' @param base base dataset (eg gridcells), needs to be SpatialPolygonsDataFrame
#' @param feat feature dataset (eg another crime generator), needs to be SpatialPointsDataFrame
#' @param weight if 1 (default), does not use weights, else pass in string that is the variable name for weights in `feat`
#'
#' @details This generates a count (or weighted count) of features inside of the base areas. Both should be projected in the same units.
#' Uses `sp::over()` methods in the function.
#' @returns
#' A vector of counts (or weighted sums)
#' @export
#' @examples
#' \donttest{
#' data(nyc_liq); data(nyc_bor)
#' gr_nyc <- prep_grid(nyc_bor,10000)
#' gr_nyc$liq_cnt <- count_xy(gr_nyc,nyc_liq)
#' gr_nyc$table_cnt <- count_xy(gr_nyc,nyc_cafe,'SWC_TABLES')
#' head(gr_nyc@data)
#' sp::spplot(gr_nyc,zcol='liq_cnt')
#' }
#' 
#' @seealso 
#' [dist_xy()] for calculating distance to nearest
#' 
#' [dcount_xy()] for counting points within distance of base polygon
#' 
#' [kern_xy()] for estimating gaussian density of points for features at base polygon xy coords
#' 
#' [bisq_xy()] to estimate bi-square kernel weights of points for features at base polygon xy coords
#' 
#' [idw_xy()] to estimate inverse distance weights of points for features at base polygon xy coords
#' 
#' @references
#' Wheeler, A. P. (2019). Quantifying the local and spatial effects of alcohol outlets on crime. *Crime & Delinquency*, 65(6), 845-871.
#' 
count_xy <- function(base,feat,weight=1){
    if (weight == 1){
        ch <- sp::over(base,feat[,1],fn=length)
    } else {
        ch <- sp::over(base,feat[,weight],fn=sum)
    }
    ch[is.na(ch)] <- 0
    return( ch[,1])
}


#' Count of points within distance of polygon
#'
#' Given a base X/Y dataset, calculates number of feature points that are within particular distance
#'
#' @param base base dataset (eg gridcells), needs to be SpatialPolygonsDataFrame
#' @param feat feature dataset (eg another crime generator), needs to be SpatialPointsDataFrame
#' @param d scaler distance to count (based on polygon boundary for base, not centroid)
#' @param weight if 1 (default), does not use weights, else pass in string that is the variable name for weights in `feat`
#'
#' @details This generates a count (or weighted count) of features within specified distance of the `base` *polygon* border. 
#' Both should be projected in the same units. Uses `raster::buffer()` on `feat` dataset (which calls `rgeos`) and `sp::over` functions.
#' @returns
#' A vector of counts (or weighted sums)
#' @export
#' @examples
#' \donttest{
#' data(nyc_cafe); data(nyc_bor)
#' gr_nyc <- prep_grid(nyc_bor,15000)
#' gr_nyc$dcafe_8k <- dcount_xy(gr_nyc,nyc_cafe,8000)
#' head(gr_nyc@data)
#' }
#' 
#' @seealso 
#' [dist_xy()] for calculating distance to nearest
#' 
#' [count_xy()] for counting points inside polygon
#' 
#' [kern_xy()] for estimating gaussian density of points for features at base polygon xy coords
#' 
#' [bisq_xy()] to estimate bi-square kernel weights of points for features at base polygon xy coords
#' 
#' [idw_xy()] to estimate inverse distance weights of points for features at base polygon xy coords
#'
#' @references
#' Groff, E. R. (2014). Quantifying the exposure of street segments to drinking places nearby. *Journal of Quantitative Criminology*, 30(3), 527-548.
#' 
dcount_xy <- function(base,feat,d,weight=1){
   # Calculate buffers
   buff <- buff_sp(base,d,dissolve=FALSE)
   # Use over to count or sum
   if (weight == 1){
       ov_buff <- sp::over(buff,feat[,1],fn=length)
   } else {
       ov_buff <- sp::over(buff,feat[,weight],fn=sum)
   }
   ov_buff[is.na(ov_buff)] <- 0
   return(as.numeric(ov_buff[,1]))
}

# Gaussian Kernel density, no norm
kern_fun <- function(d,b,w=1){
    rd <- stats::dnorm(d,0,b)
    if (w == 1){
        return( sum(rd) )
    }
    else { 
        return( sum(rd*w) )
    }
}

#' Kernel density of nearby areas 
#'
#' Given a base X/Y dataset, calculates guassian kernel density for nearby points in feat dataset
#'
#' @param base base dataset (eg gridcells), needs to be SpatialPolygonsDataFrame or SpatialPointsDataFrame
#' @param feat feature dataset (eg another crime generator), needs to be SpatialPointsDataFrame
#' @param bandwidth scaler bandwidth for the normal KDE
#' @param weight if 1 (default), does not use weights, else pass in string that is the variable name for weights in `feat`
#'
#' @details This generates a density of nearby features at particular control points (specified by `base`). Useful for risk terrain
#' style feature engineering given nearby crime generators. Loops through all pairwise distances (and uses `dnorm()`). So will be slow
#' for large base + feature datasets (although should be OK memory wise). Consider aggregating/weighting data if `feat` is very large.
#' @returns
#' A vector of densities (or weighted densities)
#' @export
#' @examples
#' \donttest{
#' data(nyc_cafe); data(nyc_bor)
#' gr_nyc <- prep_grid(nyc_bor,15000)
#' gr_nyc$kdecafe_5k <- kern_xy(gr_nyc,nyc_cafe,8000)
#' head(gr_nyc@data)
#' sp::spplot(gr_nyc,zcol='kdecafe_5k')
#' }
#'
#' @seealso 
#' [dist_xy()] for calculating distance to nearest
#' 
#' [count_xy()] for counting points inside polygon
#' 
#' [kern_xy()] for estimating gaussian density of points for features at base polygon xy coords
#' 
#' [bisq_xy()] to estimate bi-square kernel weights of points for features at base polygon xy coords
#' 
#' [idw_xy()] to estimate inverse distance weights of points for features at base polygon xy coords
#' 
#' @references
#' Caplan, J. M., Kennedy, L. W., & Miller, J. (2011). Risk terrain modeling: Brokering criminological theory and GIS methods for crime forecasting. *Justice Quarterly*, 28(2), 360-381.
#'
#' Wheeler, A. P., & Steenbeek, W. (2021). Mapping the risk terrain for crime using machine learning. *Journal of Quantitative Criminology*, 37(2), 445-480.
#' 
kern_xy <- function(base,feat,bandwidth,weight=1){
    nr <- nrow(base)
    res_weight <- vector('numeric',nr)
    fco <- sp::coordinates(feat)
    fx <- fco[,1]
    fy <- fco[,2]
    bco <- sp::coordinates(base)
    if (weight == 1){ fw <- 1
    } else { fw <- feat@data[,weight] }
    for (i in 1:nr){
        l <- t(bco[i,])
        di <- sqrt( (l[1]-fx)^2 + (l[2]-fy)^2 )
        res_weight[i] <- kern_fun(di,bandwidth,fw) 
    }
    return(res_weight)
}

bisq_fun <- function(d,b){
    ifelse(d < b, ( 1 - (d/b)^2 )^2, 0)
}

#' Bisquare weighted sum
#'
#' Given a base X/Y dataset, calculates bisquare weighted sums of points from feature dataset
#'
#' @param base base dataset (eg gridcells), needs to be SpatialPolygonsDataFrame
#' @param feat feature dataset (eg another crime generator), needs to be SpatialPointsDataFrame
#' @param bandwidth distances above this value do not contribute to the bi-square weight
#' @param weight if 1 (default), does not use attribute weights, else pass in string that is the variable name for weights in `feat`
#'
#' @details This generates bi-square distance weighted sums of features within specified distance of the `base` centroid. 
#' Bisquare weights are calculated as:
#' \deqn{w_{ij} = [ 1 - (d_{ij}/b)^2 ]^2 }
#' where d_ij is the Euclidean distance between the base point and and the feature point. If d < b, then w_ij equals 0. These are then multiplied
#' and summed so each base point gets a cumulative weighted sum. See the GWR book for a reference. 
#' Uses loops and calculates all pairwise distances, so can be slow for large base and feature datasets. Consider
#' aggregating/weighting feature dataset if it is too slow. Useful for quantifying features nearby (Groff, 2014), or for egohoods
#' (e.g. spatial smoothing of demographic info, Hipp & Boessen, 2013). 
#' @returns
#' A vector of bi-square weighted sums
#' @export
#' @examples
#' \donttest{
#' data(nyc_cafe); data(nyc_bor)
#' gr_nyc <- prep_grid(nyc_bor,15000)
#' gr_nyc$bscafe <- bisq_xy(gr_nyc,nyc_cafe,12000)
#' }
#'
#' @seealso 
#' [dist_xy()] for calculating distance to nearest
#' 
#' [count_xy()] for counting points inside polygon
#' 
#' [kern_xy()] for estimating gaussian density of points for features at base polygon xy coords
#' 
#' [bisq_xy()] to estimate bi-square kernel weights of points for features at base polygon xy coords
#' 
#' [idw_xy()] to estimate inverse distance weights of points for features at base polygon xy coords 
#' 
#' @references
#' Fotheringham, A. S., Brunsdon, C., & Charlton, M. (2003). G*eographically weighted regression: the analysis of spatially varying relationships*. John Wiley & Sons.
#' 
#' Groff, E. R. (2014). Quantifying the exposure of street segments to drinking places nearby. *Journal of Quantitative Criminology*, 30(3), 527-548.
#' 
#' Hipp, J. R., & Boessen, A. (2013). Egohoods as waves washing across the city: A new measure of “neighborhoods”. Criminology, 51(2), 287-327.
#' 
bisq_xy <- function(base,feat,bandwidth,weight=1){
    nr <- nrow(base)
    res_weight <- vector('numeric',nr)
    fco <- sp::coordinates(feat)
    fx <- fco[,1]
    fy <- fco[,2]
    bco <- sp::coordinates(base)
    if (weight == 1){ fw <- 1
    } else { fw <- feat@data[,weight] }
    for (i in 1:nr){
        l <- t(bco[i,])
        di <- sqrt( (l[1]-fx)^2 + (l[2]-fy)^2 )
        bis <- bisq_fun(di,bandwidth)
        res_weight[i] <- sum(bis*fw)
    }
    return(res_weight)
}

# Inverse Distance weight
idw_fun <- function(d,clip){
    dc <- ifelse(d > clip, 1/d, 1/clip) 
    return(dc)
}

#' Inverse distance weighted sums
#'
#' Given a base X/Y dataset, calculates clipped inverse distance weighted sums of points from feature dataset
#'
#' @param base base dataset (eg gridcells), needs to be SpatialPolygonsDataFrame
#' @param feat feature dataset (eg another crime generator), needs to be SpatialPointsDataFrame
#' @param clip scaler minimum value for weight, default `1` (so weights cannot be below 0)
#' @param weight if 1 (default), does not use weights, else pass in string that is the variable name for weights in `feat`
#'
#' @details This generates a inverse distance weighted sum of features within specified distance of the `base` centroid. 
#' Weights are clipped to never be below `clip` value, which prevents division by 0 (or division by a very small distance number)
#' Uses loops and calculates all pairwise distances, so can be slow for large base and feature datasets. Consider
#' aggregating/weighting feature dataset if it is too slow. Useful for quantifying features nearby (Groff, 2014), or for egohoods
#' (e.g. spatial smoothing of demographic info, Hipp & Boessen, 2013). 
#' @returns
#' A vector of IDW weighted sums
#' @export
#' @examples
#' \donttest{
#' data(nyc_cafe); data(nyc_bor)
#' gr_nyc <- prep_grid(nyc_bor,15000)
#' gr_nyc$idwcafe <- idw_xy(gr_nyc,nyc_cafe)
#' head(gr_nyc@data)
#' }
#' 
#' @seealso 
#' [dist_xy()] for calculating distance to nearest
#' 
#' [count_xy()] for counting points inside polygon
#' 
#' [kern_xy()] for estimating gaussian density of points for features at base polygon xy coords
#' 
#' [bisq_xy()] to estimate bi-square kernel weights of points for features at base polygon xy coords
#' 
#' [idw_xy()] to estimate inverse distance weights of points for features at base polygon xy coords 
#' 
#' @references
#' Groff, E. R. (2014). Quantifying the exposure of street segments to drinking places nearby. *Journal of Quantitative Criminology*, 30(3), 527-548.
#' 
#' Hipp, J. R., & Boessen, A. (2013). Egohoods as waves washing across the city: A new measure of “neighborhoods”. Criminology, 51(2), 287-327.
#' 
idw_xy <- function(base,feat,clip=1,weight=1){
    nr <- nrow(base)
    res_weight <- vector('numeric',nr)
    fco <- sp::coordinates(feat)
    fx <- fco[,1]
    fy <- fco[,2]
    bco <- sp::coordinates(base)
    if (weight == 1){ fw <- 1
    } else { fw <- feat@data[,weight] }
    for (i in 1:nr){
        l <- t(bco[i,])
        di <- sqrt( (l[1]-fx)^2 + (l[2]-fy)^2 )
        id <- idw_fun(di,clip)
        res_weight[i] <- sum(id*fw)
    }
    return(res_weight)
}


# These are functions to replace reliance on maptools
# do not export, no need for namespace inside vor_sp
# function
conv_sppoly_ow <- function(SP){
    if (!is.na(sp::is.projected(SP)) && !sp::is.projected(SP))
        stop("Only projected coordinates may be converted to spatstat class objects")
    pls <- methods::slot(SP, "polygons")
    nParts <- sapply(pls, function(x) length(methods::slot(x, "Polygons")))
    nOwin <- sum(nParts)
    if (nOwin == 1) {
        pl <- methods::slot(pls[[1]], "Polygons")
        crds <- methods::slot(pl[[1]], "coords")
        colnames(crds) <- c("x", "y")
        rD <- pl[[1]]@ringDir
        if (rD == 1)
            crds <- crds[nrow(crds):1, ]
        crds <- crds[-nrow(crds), ]
        res <- spatstat.geom::owin(poly = list(x = crds[, 1],
            y = crds[, 2]))
    }
    else if (nOwin > 1) {
        opls <- vector(mode = "list", length = nOwin)
        io <- 1
        for (i in seq(along = pls)) {
            pl <- methods::slot(pls[[i]], "Polygons")
            for (j in 1:nParts[i]) {
                crds <- methods::slot(pl[[j]], "coords")
                colnames(crds) <- c("x", "y")
                rD <- methods::slot(pl[[j]], "ringDir")
                hole <- methods::slot(pl[[j]], "hole")
                if (rD == -1 && hole)
                  crds <- crds[nrow(crds):1, ]
                else if (rD == 1 && !hole)
                  crds <- crds[nrow(crds):1, ]
                crds <- crds[-nrow(crds), ]
                opls[[io]] <- list(x = crds[, 1], y = crds[,
                  2])
                io <- io + 1
            }
        }
        if (!spatstat.geom::spatstat.options("checkpolygons"))
            res <- spatstat.geom::owin(sp::bbox(SP)[1, ], sp::bbox(SP)[2,
                ], poly = opls, check = FALSE)
        else res <- spatstat.geom::owin(poly = opls)
    }
    else stop("no valid polygons")
    res
}


conv_ow_poly <- function(x, id = "1"){
    stopifnot(spatstat.geom::is.owin(x))
    x <- spatstat.geom::as.polygonal(x)
    closering <- function(df) {
        df[c(seq(nrow(df)), 1), ]
    }
    pieces <- lapply(x$bdry, function(p) {
        sp::Polygon(coords = closering(cbind(p$x, p$y)), hole = spatstat.utils::is.hole.xypolygon(p))
    })
    z <- sp::Polygons(pieces, id)
    return(z)
}

conv_sst_sp <- function(x){
    stopifnot(spatstat.geom::is.tess(x))
    y <- spatstat.geom::tiles(x)
    nam <- names(y)
    z <- list()
    for (i in seq(y)) {
        zi <- try(conv_ow_poly(y[[i]], nam[i]), silent = TRUE)
        if (inherits(zi, "try-error")) {
            warning(paste("tile", i, "defective\n", as.character(zi)))
        }
        else {
            z[[i]] <- zi
        }
    }
    return(sp::SpatialPolygons(z))
}


#' Voronoi tesselation from input points
#'
#' Given an outline and feature points, calculates Voronoi areas
#'
#' @param outline object that can be coerced to a spatstat window via `as.owin` (so SpatialPolygonDataFrame, SpatialPolygon, owin)
#' @param feat A SpatialPointsDataFrame object (if duplicate X/Y coordinates will get errors)
#'
#' @details Outline should be a single polygon area. Uses spatstats `dirichlet` and window to compute the Voronoi tesselation.
#' Will generate errors if feat has duplicate X/Y points. Useful to create areas for other functions, 
#' such as `dcount_xy()` or `count_xy()`. Common spatial unit of analysis used in crime research when using points (e.g. intersections
#' and street midpoints). 
#' 
#' @returns
#' A SpatialPolygonsDataFrame object, including the dataframe for all the info in the orignal `feat@data` dataframe.
#' @export
#' @examples
#' \donttest{
#' library(sp) # for sample/coordinates
#' data(nyc_bor)
#' nyc_buff <- buff_sp(nyc_bor,50000)
#' po <- sp::spsample(nyc_buff,20,'hexagonal')
#' po$id <- 1:dim(coordinates(po))[1] # turns into SpatialDataFrame
#' vo <- vor_sp(nyc_buff,po)
#' plot(vo)
#' plot(nyc_buff,border='RED',lwd=3, add=TRUE)
#' }
#' 
#' @references
#' Wheeler, A. P. (2018). The effect of 311 calls for service on crime in DC at microplaces. *Crime & Delinquency*, 64(14), 1882-1903.
#' 
#' Wheeler, A. P. (2019). Quantifying the local and spatial effects of alcohol outlets on crime. *Crime & Delinquency*, 65(6), 845-871.
#' 
vor_sp <- function(outline,feat){
    # Create the window
    outline_win <- conv_sppoly_ow(outline) 
    # Create the spatstat ppp
    fxy <- sp::coordinates(feat)
    pp <- spatstat.geom::ppp(fxy[,1], fxy[,2], window=outline_win)
    # Create the tesselation
    tess <- spatstat.geom::dirichlet(pp)
    # Just making own function, take out maptools dependency
    sp_object <- conv_sst_sp(tess)
    f2 <- feat@data #cleaning up the polygon ID
    pid <- sapply(methods::slot(sp_object, "polygons"), function(x) methods::slot(x, "ID"))
    row.names(f2) <- pid
    spdf <- sp::SpatialPolygonsDataFrame(sp_object, f2)
    sp::proj4string(spdf) <- sp::proj4string(outline)
    return(spdf)
}



#ToDo CPP functions for loops
#' NUVEL-1 Global model of current plate motions
#'
#' NNR-NUVEL-1 global model of current plate motions by DeMets et al. 1990
#'
#' @docType data
#'
#' @usage data('nuvel1')
#'
#' @format An object of class `data.frame`
#' \describe{
#'   \item{plate.name}{The rotating plate}
#'   \item{plate.rot}{The abbreviation of the plate's name}
#'   \item{lat,lon}{Coordinates of the Pole of Rotation}
#'   \item{angle}{The amount of rotation (angle in 1 Myr)}
#'   \item{plate.fix}{The anchored plate, i.e. `plate.rot` moves relative
#'   to  `plate.fix`}
#'   \item{source}{Reference to underlying study}
#' }
#'
#' @references DeMets, C., Gordon, R. G., Argus, D. F., & Stein, S. (1990).
#' Current plate motions. *Geophysical Journal International*, **101**(2),
#' 425-478. \doi{10.1111/j.1365-246X.1990.tb06579.x}.
#' @keywords datasets
#'
#' @examples
#' data("nuvel1")
#' head("nuvel1")
"nuvel1"

#' Global model of current plate motions
#'
#' PB2002 global model of current plate motions by Bird 2003
#'
#' @docType data
#'
#' @usage data('pb2002')
#'
#' @format An object of class `data.frame`
#' \describe{
#'   \item{plate.name}{The rotating plate}
#'   \item{plate.rot}{The abbreviation of the plate's name}
#'   \item{lat,lon}{Coordinates of the Pole of Rotation}
#'   \item{angle}{The amount of rotation (angle in 1 Myr)}
#'   \item{plate.fix}{The anchored plate, i.e. `plate.rot` moves relative
#'   to `plate.fix`}
#'   \item{source}{Reference to underlying study}
#' }
#'
#' @references  Bird, P. (2003), An updated digital model of plate boundaries,
#' *Geochem. Geophys. Geosyst.*, **4**, 1027, doi: 10.1029/2001GC000252, 3.
#'
#' @keywords datasets
#'
#' @examples
#' data("pb2002")
#' head("pb2002")
"pb2002"


#' Global model of current plate motions
#'
#' Compilation of global models for current plate motions, including
#' NUVEL1 (DeMets et al. 1990),
#' NNR-NUVEL1A (DeMets et al., 1990),
#' NNR-MORVEL56 (Argus et al., 2011),
#' REVEL (Sella et al., 2002),
#' GSRM2.1 (Kreemer et al., 2014),
#' HS2-NUVEL1 (Gripp and Gordon, 1990),
#' HS3-NUVEL1A (Gripp and Gordon, 2002),
#' P073 (Chase 1978),
#' AM1-2 (Minster and Jordan, 1978),
#' ITRF2020-PPM (Altamimi et al. 2023),
#' and
#' PB2002 (Bird, 2003)
#'
#' @docType data
#'
#' @usage data('cpm_models')
#'
#' @format list containing objects of class `data.frame`
#' \describe{
#'   \item{plate.name}{The rotating plate}
#'   \item{plate.rot}{The abbreviation of the plate's name}
#'   \item{lat,lon}{Coordinates of the Pole of Rotation}
#'   \item{angle}{The amount of rotation (angle in 1 Myr)}
#'   \item{plate.fix}{The anchored plate, i.e. `plate.rot` moves relative
#'   to  `plate.fix`}
#'   \item{model}{Model for current global plate motion}
#' }
#' @references
#' Altamimi, Z., Métivier, L., Rebischung, P., Collilieux, X., Chanard, K.,
#' Barnéoud, J., 2023. ITRF2020 Plate Motion Model. *Geophys. Res. Lett.*
#' **50**, 1–7. \doi{10.1029/2023GL106373}
#'
#' Argus, D.F., Gordon, R.G., 1991. No-net-rotation model of
#' current plate velocities incorporating plate motion model NUVEL-1.
#' *Geophys. Res. Lett.* **18**, 2039–2042. doi: 10.1029/91GL01532
#'
#' Argus, D. F., Gordon, R. G., & DeMets, C. (2011). Geologically
#' current motion of 56 plates relative to the no-net-rotation reference frame.
#' *Geochemistry, Geophysics, Geosystems*, **12**(11).
#' 10.1029/2011GC003751.
#'
#' Chase, C.G. (1978). Plate kinematics: The Americas, East Africa, and the rest
#' of the world. *Earth Planet. Sci. Lett.* **37**, 355–368.
#' doi: \doi{10.1016/0012-821X(78)90051-1}
#'
#' Bird, P. (2003), An updated digital model of plate boundaries,
#' *Geochem. Geophys. Geosyst.*, **4**, 1027, doi: 10.1029/2001GC000252, 3.
#'
#' DeMets, C., Gordon, R. G., Argus, D. F., & Stein, S. (1990).
#' Current plate motions. *Geophysical Journal International*, **101**(2),
#' 425-478. \doi{10.1111/j.1365-246X.1990.tb06579.x}.
#'
#' Gripp, A. E., & Gordon, R. G. (2002). Young tracks of hotspots and current
#' plate velocities. *Geophysical Journal International*, **150**(2), 321<U+2013>361.
#' \doi{10.1046/j.1365-246X.2002.01627.x}.
#'
#' Kreemer, C., Blewitt, G., & Klein, E. C. (2014). A geodetic plate motion
#' and Global Strain Rate Model. *Geochemistry, Geophysics, Geosystems*,
#' **15**(10), 3849<U+2013>3889. doi: 10.1002/2014GC005407.
#'
#' Minster, J. and Jorda, T. (1978). Present-day plate motions.
#' *Journal of Geophysical Research*, **83**, \doi{10.1029/jb083ib11p05331}.
#'
#' Sella, G. F., Dixon, T. H., & Mao, A. (2002). REVEL: A model for Recent
#' plate velocities from space geodesy. *Journal of Geophysical Research: Solid
#' Earth*, **107**(B4). doi: 10.1029/2000jb000033.
#' @keywords datasets
#' @examples
#' data("cpm_models")
#' head(cpm_models[[1]])
"cpm_models"

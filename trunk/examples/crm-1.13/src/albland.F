#include <params.h>
      subroutine albland(lat     ,ioro    ,sndpth  ,coszrs  ,albs    ,
     $                   albl    ,albsd   ,albld   )
C-----------------------------------------------------------------------
C
C Compute surface albedos over land
C
C Computes surface albedos for direct/diffuse incident radiation for
C two spectral intervals:
C   s = 0.2-0.7 micro-meters
C   l = 0.7-5.0 micro-meters
C
C Uses knowledge of surface type to specify albedo, as follows:
C
C Land without    Albedos specified by two dimensional surface albedo
C     snow        fields, which distinguish surfaces with strong solar
C                 zenith angle dependence from those with weaker solar
C                 zenith angle dependence; alb independent of surface
C                 moisture or other physical factors.
C
C Land with snow  Snow depth (liquid water equivalent) used, along with
C                 aerodynamic roughness to define a horizontal fraction
C                 of land surface covered with snow; snow albedos are
C                 comptd as functions of solar zenith angle; these snow
C                 albedos are then weighted by the horizontal fraction
C                 of coverage with the underlying surface albedos
C                 computed above to produce total grid mean albedo.
C
C land with ice   Surface albedos specified as functions of spectral
C                 interval; combined with overlying snow in a similar
C                 manner to the case of land with snow.
C
C Note, the code collects together surfaces of the same type for various
C computations in order to vectorize longitude loops.
C
C For more details , see Briegleb, Bruce P., 1992: Delta-Eddington
C Approximation for Solar Radiation in the NCAR Community Climate Model,
C Journal of Geophysical Research, Vol 97, D7, pp7603-7612).
C
C The details of the land surface albedo arrays can be found in the
C common block description below.
C
C---------------------------Code history--------------------------------
C
C Original version:        CCM1
C Standardized:            J. Rosinski, June 1992
C Reviewed:                J. Kiehl, B. Briegleb, August 1992
C Rewritten for land only: J. Rosinski, May, 1994
C
C-----------------------------------------------------------------------
c
c $Id: albland.F,v 1.1.1.1 1995/02/09 23:26:33 ccm2 Exp $
c $Author: ccm2 $
c
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <prgrid.h>
C-----------------------------------------------------------------------
#include <albedo.h>
C------------------------------Commons----------------------------------
#include <crdalb.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      integer lat          ! Lat index for two dimensional data arrays
      integer ioro(plond)  ! Surface type flag (ocean, land, sea ice)
      real sndpth(plond),  ! Snow depth (liquid water equivalent)
     $     coszrs(plond)   ! Cosine solar zenith angle
C
C Output arguments
C
      real albs(plond),    ! Srf alb for direct rad   0.2-0.7 micro-ms
     $     albl(plond),    ! Srf alb for direct rad   0.7-5.0 micro-ms
     $     albsd(plond),   ! Srf alb for diffuse rad  0.2-0.7 micro-ms
     $     albld(plond)    ! Srf alb for diffuse rad  0.7-5.0 micro-ms
C
C---------------------------Local variables-----------------------------
C
      integer i,ii         ! Longitude indices
      integer indx(plond)  ! Longitude index array (land)
      integer npts         ! Number of land points
      real rs,             ! Empirical fact strng znth angl dependence
     $     rw,             ! Empirical fact weak  znth angl dependence
     $     frsnow,         ! Horizontal fraction of snow cover
     $     snwhgt,         ! Physical snow height
     $     rghsnw          ! Roughness for horizontal snow cover fractn
      real salbs(plond),   ! Snow alb for direct rad  0.2-0.7 micro-ms
     $     salbl(plond),   ! Snow alb for direct rad  0.7-5.0 micro-ms
     $     salbsd(plond),  ! Snow alb for diffuse rad  0.2-0.7 micro-ms
     $     salbld(plond)   ! Snow alb for diffuse rad  0.7-5.0 micro-ms
C
C Externals
C
      external wheneq
C
C-----------------------------------------------------------------------
C
C Find land surfaces
C
      call wheneq(plon,ioro,1,1,indx,npts)
CDIR$ IVDEP
      do ii=1,npts
         i = indx(ii)
         if (coszrs(i).gt.0.0) then         ! Sun above horizon
C
C Use empirical factors to adjust surface albedos for zenith angle
C effects, distinguishing between strong and weakly dependent surfaces:
C
            rs = 1.4/(1. + .8*coszrs(i))
            rw = 1.1/(1. + .2*coszrs(i))
            albs(i)  = albvss(i,lat)*frctst(i,lat)*rs +
     $                 albvsw(i,lat)*(1. - frctst(i,lat))*rw
            albl(i)  = albnis(i,lat)*frctst(i,lat)*rs +
     $                 albniw(i,lat)*(1. - frctst(i,lat))*rw
            albsd(i) = albvss(i,lat)*frctst(i,lat) +
     $                 albvsw(i,lat)*(1. - frctst(i,lat))
            albld(i) = albnis(i,lat)*frctst(i,lat) +
     $                 albniw(i,lat)*(1. - frctst(i,lat))
            if (sndpth(i).gt.0.) then
               salbsd(i) = snws
               salbld(i) = snwl
            end if
         else
C
C Sun below horizon: set land albedos to zero
C
            albs(i) = 0.
            albl(i) = 0.
            albsd(i) = 0.
            albld(i) = 0.
         end if
      end do
CDIR$ IVDEP
      do ii=1,npts
         i = indx(ii)
         if (sndpth(i).gt.0. .and. coszrs(i).gt.0.) then
            if (coszrs(i).lt.0.5) then
C
C Zenith angle regime 1 ( coszrs < 0.5 ).
C Set direct snow albedos (limit to 0.98 max)
C
               salbs(i) = amin1(0.98,salbsd(i) + (1. - salbsd(i))*0.5*
     $                          ((3./(1. + 4.*coszrs(i))) - 1.))
               salbl(i) = amin1(0.98,salbld(i) + (1. - salbld(i))*0.5*
     $                          ((3./(1. + 4.*coszrs(i))) - 1.))
            else
C
C Zenith angle regime 2 ( coszrs >= 0.5 )
C
               salbs(i) = snws
               salbl(i) = snwl
            end if
C
C Compute both diffuse and direct total albedos
C
            snwhgt = 20.*sndpth(i)
            rghsnw = amax1(rghnss(i,lat),0.25)
            frsnow = snwhgt/(rghsnw + snwhgt)
            albs(i)  = albs(i) *(1. - frsnow) + salbs(i) *frsnow
            albl(i)  = albl(i) *(1. - frsnow) + salbl(i) *frsnow
            albsd(i) = albsd(i)*(1. - frsnow) + salbsd(i)*frsnow
            albld(i) = albld(i)*(1. - frsnow) + salbld(i)*frsnow
         end if
      end do
C   
      return
#ifdef LOGENTRY 
$Log: albland.F,v $
c Revision 1.1.1.1  1995/02/09  23:26:33  ccm2
c Start Omega Model. Split into spectral/sld modules
c
c Revision 1.1  1994/09/16  21:07:27  rosinski
c This is the first part of plx23.
c 
#endif
      end

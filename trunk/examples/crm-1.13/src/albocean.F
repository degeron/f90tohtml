#include <params.h>
      subroutine albocean(lat     ,ioro    ,sndpth  ,coszrs  ,albs    ,
     $                    albl    ,albsd   ,albld   )
C-----------------------------------------------------------------------
C
C Compute surface albedos
C
C Computes surface albedos for direct/diffuse incident radiation for
C two spectral intervals:
C   s = 0.2-0.7 micro-meters
C   l = 0.7-5.0 micro-meters
C
C Uses knowledge of surface type to specify albedo, as follows:
C
C Ocean           Uses solar zenith angle to compute albedo for direct
C                 radiation; diffuse radiation values constant; albedo
C                 independent of spectral interval and other physical
C                 factors such as ocean surface wind speed.
C
C Ocean with      Surface albs specified; combined with overlying snow
C   sea ice       in a similar manner to the case of land with snow.
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
c $Id: albocean.F,v 1.1.1.1 1995/02/09 23:26:33 ccm2 Exp $
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
      integer lat           ! Lat index for two dimensional data arrays
      integer ioro(plond)   ! Surface type flag (ocean, land, sea ice)
      real sndpth(plond),   ! Snow depth (liquid water equivalent)
     $     coszrs(plond)    ! Cosine solar zenith angle
C
C Output arguments
C
      real albs(plond),     ! Srf alb for direct rad   0.2-0.7 micro-ms
     $     albl(plond),     ! Srf alb for direct rad   0.7-5.0 micro-ms
     $     albsd(plond),    ! Srf alb for diffuse rad  0.2-0.7 micro-ms
     $     albld(plond)     ! Srf alb for diffuse rad  0.7-5.0 micro-ms
C
C---------------------------Local variables-----------------------------
C
      integer i,ii,         ! Longitude indices
     $        indx(plond),  ! Indices for computation points (ocean or sea ice)
     $        indxo(plond), ! Indices for computation points (ocean)
     $        indxsi(plond),! Indices for computation points (sea ice)
     $        npts,         ! Number of ocean/sea ice points
     $        nptso,        ! Number of ocean points
     $        nptssi        ! Number of sea ice points
      real frsnow,          ! Horizontal fraction of snow cover
     $     snwhgt,          ! Physical snow height
     $     rghsnw           ! Roughness for horizontal snow cover fractn
      real salbs(plond),    ! Snow alb for direct rad  0.2-0.7 micro-ms
     $     salbl(plond),    ! Snow alb for direct rad  0.7-5.0 micro-ms
     $     salbsd(plond),   ! Snow alb for diffuse rad  0.2-0.7 micro-ms
     $     salbld(plond)    ! Snow alb for diffuse rad  0.7-5.0 micro-ms
C
C Externals
C
      external  wheneq      ! When equal funct gives indices for condtn
      external  whenne      ! When not equal funct gives indices for condtn
C
C-----------------------------------------------------------------------
C
C Find ocean and sea ice surfaces.
C
      call whenne(plon,ioro,1,1,indx,npts)      ! Ocean or sea ice
      call wheneq(plon,ioro,1,0,indxo,nptso)    ! Open ocean
      call wheneq(plon,ioro,1,2,indxsi,nptssi)  ! Sea ice
C
C Initialize all ocean/sea ice surface albedos to zero
C
      do ii=1,npts
         i = indx(ii)
         albs(i) = 0.
         albl(i) = 0.
         albsd(i) = 0.
         albld(i) = 0.
      end do
      do ii=1,nptssi
         i = indxsi(ii)
         if (coszrs(i).gt.0.0) then
            albs(i)  = sices
            albl(i)  = sicel
            albsd(i) = albs(i)
            albld(i) = albl(i)
            salbsd(i) = snws
            salbld(i) = snwl
         end if
      end do
CDIR$ IVDEP
      do ii=1,nptssi
         i = indxsi(ii)
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
C Ice-free ocean albedos function of solar zenith angle only, and
C independent of spectral interval:
C
CDIR$ IVDEP
      do ii=1,nptso
         i = indxo(ii)
         if (coszrs(i).gt.0.) then
            albl(i)  = (.026/(coszrs(i)**1.7 + .065)) +
     $                 (.15*(coszrs(i) - 0.10)*
     $                      (coszrs(i) - 0.50)*
     $                      (coszrs(i) - 1.00)  )
            albs(i)  = albl(i)
            albld(i) = 0.06
            albsd(i) = 0.06
         end if
      end do
C   
      return
#ifdef LOGENTRY 
$Log: albocean.F,v $
c Revision 1.1.1.1  1995/02/09  23:26:33  ccm2
c Start Omega Model. Split into spectral/sld modules
c
c Revision 1.1  1994/09/16  21:07:29  rosinski
c This is the first part of plx23.
c 
#endif
      end

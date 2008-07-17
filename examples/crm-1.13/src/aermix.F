#include <params.h>
      subroutine aermix(pint  ,pmid   ,h2ommr  ,t   ,aermmr  ,
     $                  rh    )
C-----------------------------------------------------------------------
C
C Specify aerosol mixing ratio and compute relative humidity
C for later adjustment of aerosol optical properties
C
C Currently (March 1995), aerosol mass mixing ratio is specified in
C such a manner that the column visible aerosol optical depth is a
C specified global number (tauvis). This means that the actual mixing
C ratio depends on pressure thickness of the lowest three atmospheric
C layers near the surface.
C
C---------------------------Code history--------------------------------
C
C Original version:  B. Briegleb  March 1995
C
C-----------------------------------------------------------------------
c
c $Id: aermix.F,v 1.1 1997/11/03 22:21:34 zender Exp zender $
c $Author: zender $
c
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <pmgrid.h>
#include <ptrrgrid.h>
#include <pagrid.h>
C------------------------------Commons----------------------------------
#include <crdcon.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real pint(plond,plevrp),  ! Radiation level interface pressures (dynes/cm2)
     $     pmid(plond,plevr),   ! Radiation level mid-level pressures (dynes/cm2)
     $     h2ommr(plond,plev),  ! Radiation level specific humidity   (g/g)
     $     t(plond,plev)        ! Radiation level temperatures        (K)
C
C Output arguments
C
      real aermmr(plond,plevr), ! Radiation level aerosol mass mixing ratio
     $     rh(plond,plevr)      ! Radiation level relative humidity (fraction)
C
C---------------------------Local variables-----------------------------
C
      integer i,     ! longitude index
     $        k,     ! level index
     $        mxaerl ! max nmbr aerosol levels counting up from surface
C
      real tauvis,  ! visible optical depth
     $     kaervs,  ! visible extinction coefficiant of aerosol (m2/g)
     $     omgvis,  ! visible omega0
     $     gvis,    ! visible forward scattering asymmetry parameter
     $     rhcnst   ! constant relative humidity factor
C
C Relative humidity factor
C
      real rhfac,              ! multiplication factor for kaer
     $     rhpc,               ! level relative humidity in %
     $     a0,                 ! constant in rh mult factor
     $     a1,                 ! constant in rh mult factor
     $     a2,                 ! constant in rh mult factor
     $     a3                  ! constant in rh mult factor
c
      data a0 / -9.2906106183    /
      data a1 /  0.52570211505   /
      data a2 / -0.0089285760691 /
      data a3 /  5.0877212432e-05/
C
      data mxaerl /  3  /
c++csz
c      data tauvis / .12 /
      data tauvis / .14 /
c--csz
      data kaervs / 5.3012 /
      data omgvis / 0.999999 /
      data gvis   / 0.694889 /
      data rhcnst / .80 /
C
C--------------------------------------------------------------------------
C
C Set relative humidity and factor; then aerosol amount:
C
      do i=1,plon
        do k=1,plevr
C
          rh(i,k) = rhcnst
C
C Compute relative humidity factor:
C
          if( rh(i,k) .gt. .90 ) then
            rhfac = 2.8
          else if (rh(i,k) .lt. .60 ) then
            rhfac = 1.0
          else
            rhpc  = 100. * rh(i,k)
            rhfac = (a0 + a1*rhpc + a2*rhpc**2 + a3*rhpc**3)
          endif
C
C Find constant aerosol mass mixing ratio for specified levels
C in the column, converting units where appropriate
C
          if( k .ge. plevrp-mxaerl ) then
            aermmr(i,k) = gravit*tauvis /
     $              (1.e4*kaervs*rhfac*(1.-omgvis*gvis*gvis)*
     $              (pint(i,plevrp)-pint(i,plevrp-mxaerl)))
          else
            aermmr(i,k) = 0.0
          endif
C
        enddo
      enddo        
C
C
      return
#ifdef LOGENTRY 
$Log: aermix.F,v $
Revision 1.1  1997/11/03 22:21:34  zender
Initial revision

c Revision 1.1  1995/03/17  18:54:06  ccm2
c add globally uniform background aerosol.
c
c
#endif
      end

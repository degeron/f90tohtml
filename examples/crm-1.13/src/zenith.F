#include <params.h>
      subroutine zenith(calday  ,dodiavg ,clat    ,coszrs  )
C-----------------------------------------------------------------------
C
C Compute cosine of solar zenith angle for albedo and radiation computations
C
C---------------------------Code history--------------------------------
C
C Original version:  J. Rosinski, May, 1994
C
C-----------------------------------------------------------------------
c
c $Id: zenith.F,v 1.1.1.1 1995/02/09 23:27:16 ccm2 Exp $
c $Author: ccm2 $
c
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <pmgrid.h>
C------------------------------Commons----------------------------------
#include <crdcon.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real calday              ! Calendar day, including fraction
      logical dodiavg          ! true => do diurnal averaging
      real clat                ! Current latitude (radians)
C
C Output arguments
C
      real coszrs(plond)       ! Cosine solar zenith angle
C
C---------------------------Local variables-----------------------------
C
      integer i     ! Longitude loop index
      real phi,     ! Greenwich calendar day + local time + long offset
     $     theta,   ! Earth orbit seasonal angle in radians
     $     delta,   ! Solar declination angle  in radians
     $     sinc,    ! Sine   of latitude
     $     cosc,    ! Cosine of latitude
     $     sind,    ! Sine   of declination
     $     cosd     ! Cosine of declination
      real frac,    ! Daylight fraction
     $     arg,     ! ?
     $     tsun,    ! temporary term in diurnal averaging
     $     coszrsu  ! uniform cosine zenith solar angle 
C
C-----------------------------------------------------------------------
C
C Compute solar distance factor and cosine solar zenith angle usi
C day value where a round day (such as 213.0) refers to 0z at
C Greenwich longitude.
C
C Use formulas from Paltridge, G.W. and C.M.R. Platt 1976: Radiative
C Processes in Meterology and Climatology, Elsevier Scientific
C Publishing Company, New York  p. 57, p. 62,63.
C
      theta = 2.*pie*calday/dayspy
C
C Solar declination in radians:
C
      delta = .006918 - .399912*cos(theta) + .070257*sin(theta) -
     $        .006758*cos(2.*theta) + .000907*sin(2.*theta) -
     $        .002697*cos(3.*theta) + .001480*sin(3.*theta)
C
C Compute local cosine solar zenith angle,
C
      sinc = sin(clat)
      sind = sin(delta)
      cosc = cos(clat)
      cosd = cos(delta)
C
C If using diurnal averaging, then compute the average local cosine solar 
C zenith angle using formulas from paltridge and platt 1976  p. 57, p. 62,63.
C
      if (dodiavg) then
         arg = -(sinc/cosc)*(sind/cosd)
         if (arg .lt. -1.) then
            frac = 1.0
         else if (arg .gt. 1.) then
            frac = 0.0
         else
            frac = (1./pie)*acos(arg)
         endif
         tsun = pie*frac
         if (tsun .gt. 0.) then
            coszrsu =  sinc*sind + (cosc*cosd*sin(tsun))/tsun
         else
            coszrsu = 0.0
         endif
         do i=1,plon
            coszrs(i) = coszrsu
         end do
      else                       ! No diurnal averaging
C
C Calday is the calender day for Greenwich, including fraction
C of day; the fraction of the day represents a local time at
C Greenwich; to adjust this to produce a true instantaneous time
C For other longitudes, we must correct for the local time change:
C local time based on the longitude and day of year
C then compute the local cosine solar zenith angle
C
         do i=1,plon
            phi       = calday + (real(i-1)/real(plon))
            coszrs(i) = sinc*sind - cosc*cosd*cos(2.*pie*phi)
         end do
      end if
C
      return
#ifdef LOGENTRY 
$Log: zenith.F,v $
c Revision 1.1.1.1  1995/02/09  23:27:16  ccm2
c Start Omega Model. Split into spectral/sld modules
c
c Revision 1.1  1994/09/16  21:09:32  rosinski
c This is the first part of plx23.
c 
#endif
      end






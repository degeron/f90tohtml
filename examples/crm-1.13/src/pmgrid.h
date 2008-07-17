c
c $Id: pmgrid.h,v 1.1 1997/11/03 22:21:34 zender Exp zender $
c $Author: zender $
c
C
C Basic grid point resolution parameters
C
      integer
     $     plon,    ! number of longitudes
     $     plev,    ! number of vertical levels
     $     plat,    ! number of latitudes
     $     pcnst,   ! number of constituents (including water vapor)
     $     plevmx,  ! number of subsurface levels
     $     plevp,   ! plev + 1
     $     nxpt,    ! no.of points outside active domain for interpolant
     $     jintmx,  ! number of extra latitudes in polar region
     $     plond,   ! slt extended domain longitude
     $     platd,   ! slt extended domain lat.
     $     plevd    ! fold plev,pcnst indices into one
C
      parameter(plon   = PLON,
     $          plev   = PLEV,
     $          plat   = PLAT,
     $          pcnst  = PCNST,
     $          plevmx = 4,
     $          plevp  = plev + 1,
     $          nxpt   = 1,
     $          jintmx = 1,
c++csz
c     $          plond  = plon + 1 + 2*nxpt,
     $          plond  = PLON,
c--csz
     $          platd  = plat + 2*nxpt + 2*jintmx,
     $          plevd  = plev*(3 + pcnst))
C
#ifdef LOGENTRY 
$Log: pmgrid.h,v $
Revision 1.1  1997/11/03 22:21:34  zender
Initial revision

 * Revision 1.2  1995/02/10  01:09:06  ccm2
 * Add spectral code to omega, create omega0_1
 *
 * Revision 1.1.1.1  1994/04/21  23:55:13  ccm2
 * First cvs version of plx22
 * 
#endif

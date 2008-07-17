c
c $Id: comozp.h,v 1.1.1.1 1995/02/09 23:27:16 ccm2 Exp $
c $Author: ccm2 $
c
C
C ozone mixing ratios, pressures and times
C
      integer pnoz,     ! Maximum number of levels in ozone input data
     $        pozlon    ! Number of ozone longitudes
      parameter (pnoz=100)
      parameter (pozlon=1)
      common /comozp/nyroz   ,ldoyoz  ,ndoyoz  ,cplos   ,cplol   ,
     $               ozmix(plat,pnoz),ozmixm(pozlon,pnoz,plat,2),
     $               pin(pnoz),koz
C
      integer nyroz  ! Year of ozone data just after current date
      real ldoyoz,   ! Day of yr of ozone data prior to current date
     $     ndoyoz,   ! Day of yr of ozone data after current date
     $     cplos,    ! Constant for ozone path length calculation
     $     cplol,    ! Constant for pressure-weighted o3 path length calc.
     $     ozmix,    ! Time-interpolated mixing ratios
     $     ozmixm,   ! Two consecutive time slices of ozone mixing ratios
     $     pin       ! Pressures at model interfaces
      integer koz    ! Actual number of levels in ozone input data
C
#ifdef LOGENTRY 
$Log: comozp.h,v $
 * Revision 1.1.1.1  1995/02/09  23:27:16  ccm2
 * Start Omega Model. Split into spectral/sld modules
 *
 * Revision 1.2  1994/09/16  21:07:51  rosinski
 * This is the first part of plx23.
 * 
#endif

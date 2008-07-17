c
c $Id: ptrrgrid.h,v 1.1.1.1 1995/02/09 23:26:59 ccm2 Exp $
c $Author: ccm2 $
c
C
C Define radiation vertical grid and buffer length for abs/ems out-of-core file
C
      integer
     $     plevr,   ! number of vertical levels
     $     plevrp,  ! plevr + 1
     $     plngbuf  ! length of absorptivity/emissivity record
C
      parameter(plevr = PLEVR,
     $          plevrp = plevr + 1,
     $          plngbuf = 512*((plond*plevrp*plevrp + plond*plevr*4 +
     $                          plond*plevrp)/512 + 1))
C
#ifdef LOGENTRY 
$Log: ptrrgrid.h,v $
 * Revision 1.1.1.1  1995/02/09  23:26:59  ccm2
 * Start Omega Model. Split into spectral/sld modules
 *
 * Revision 1.1.1.1  1994/04/21  23:55:13  ccm2
 * First cvs version of plx22
 * 
#endif

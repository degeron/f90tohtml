c
c $Id: implicit.h,v 1.1.1.1 1995/02/09 23:26:52 ccm2 Exp $
c $Author: ccm2 $
c
      implicit none
#if ( defined RS6000 )
      implicit automatic (a-z)
#endif
#ifdef LOGENTRY 
$Log: implicit.h,v $
 * Revision 1.1.1.1  1995/02/09  23:26:52  ccm2
 * Start Omega Model. Split into spectral/sld modules
 *
 * Revision 1.1  1994/09/16  21:22:36  rosinski
 * Add new .h files
 * 
#endif

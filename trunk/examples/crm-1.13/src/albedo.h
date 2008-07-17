c
c $Id: albedo.h,v 1.1.1.1 1995/02/09 23:26:33 ccm2 Exp $
c $Author: ccm2 $
c
      real snws,           ! Snow albedo for 0.2-0.7 micro-meters
     $     snwl,           ! Snow albedo for 0.7-5.0 micro-meters
     $     sices,          ! Sea ice albedo for 0.2-0.7 micro-meters
     $     sicel           ! Sea ice albedo for 0.7-5.0 micro-meters
      parameter (snws  = 0.95,
     $           snwl  = 0.70,
     $           sices = 0.70,
     $           sicel = 0.50)
#ifdef LOGENTRY 
$Log: albedo.h,v $
 * Revision 1.1.1.1  1995/02/09  23:26:33  ccm2
 * Start Omega Model. Split into spectral/sld modules
 *
 * Revision 1.1  1994/09/16  21:22:33  rosinski
 * Add new .h files
 * 
#endif

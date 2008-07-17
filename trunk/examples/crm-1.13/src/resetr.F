#include <params.h>
      subroutine resetr(pa      ,kdim    ,pvalue  )
C-----------------------------------------------------------------------
C
C Reset array pa(kdim) to pvalue
C
C---------------------------Code history--------------------------------
C
C Original version:  CCM1
C Standardized:      L. Bath, June 1992
C
C-----------------------------------------------------------------------
c
c $Id: resetr.F,v 1.1.1.1 1995/02/09 23:27:04 ccm2 Exp $
c $Author: ccm2 $
c
      implicit none
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      integer kdim      ! Dimension of array pa
      real pvalue       ! Value to store in pa
C
C Output arguments
C
      real pa(kdim)     ! Array to reset
C
C---------------------------Local variable------------------------------
C
      integer j         ! Loop index
C
C-----------------------------------------------------------------------
C
      do j=1,kdim
         pa(j) = pvalue
      end do
C
      return
#ifdef LOGENTRY 
$Log: resetr.F,v $
c Revision 1.1.1.1  1995/02/09  23:27:04  ccm2
c Start Omega Model. Split into spectral/sld modules
c
c Revision 1.1.1.1  1994/04/21  23:55:07  ccm2
c First cvs version of plx22
c 
#endif
      end

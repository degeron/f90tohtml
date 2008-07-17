#include <params.h>
      integer function isrchfgt(n, array, inc, target)
c
c $Id: isrchfgt.F,v 1.1.1.1 1995/02/09 23:26:15 ccm2 Exp $
c $Author: ccm2 $
c
      integer n,inc
      real array(*),target
      integer i
      if (n.le.0) then
         isrchfgt = 0
         return
      end if
      ind = 1
      do i=1,n
         if (array(ind).gt.target) then
            isrchfgt = i
            return
         else
            ind = ind + inc
         end if
      end do
      isrchfgt = n + 1
      return
#ifdef LOGENTRY 
$Log: isrchfgt.F,v $
c Revision 1.1.1.1  1995/02/09  23:26:15  ccm2
c Start Omega Model. Split into spectral/sld modules
c
c Revision 1.1  1994/09/16  20:03:32  ccm2
c First revision of plx22/util routines
c 
#endif
      end


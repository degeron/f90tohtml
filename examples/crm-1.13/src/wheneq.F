#include <params.h>
      subroutine wheneq(n,array,inc,target,index,nval)
c
c $Id: wheneq.F,v 1.2 1995/02/28 22:19:52 ccmproc2 Exp $
c $Author: ccmproc2 $
c
      integer index(*),
     $        array(*),
     $        target
      ina=1
      nval=0
      if(inc .lt. 0) ina=(-inc)*(n-1)+1
      do i=1,n
         if(array(ina) .eq. target) then
           nval=nval+1
           index(nval)=i
         end if
         ina=ina+inc
      enddo
      return
#ifdef LOGENTRY 
$Log: wheneq.F,v $
c Revision 1.2  1995/02/28  22:19:52  ccmproc2
c Test CVS Administration with a trivial change to wheneq.
c
c Revision 1.1.1.1  1995/02/09  23:26:17  ccm2
c Start Omega Model. Split into spectral/sld modules
c
c Revision 1.1  1994/09/16  20:03:54  ccm2
c First revision of plx22/util routines
c 
#endif
      end

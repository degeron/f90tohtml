      subroutine whenne(n,array,inc,target,index,nval)
c
c $Id: whenne.F,v 1.1.1.1 1995/02/09 23:26:18 ccm2 Exp $
c $Author: ccm2 $
c
      implicit none
      integer n,inc
      integer array(*),target
      integer index(*),nval
c
      integer i,ina
c
      ina=1
      nval=0
      if(inc.lt.0)ina=(-inc)*(n-1)+1
      do i=1,n
         if (array(ina).ne.target) then
            nval=nval+1
            index(nval)=i
         end if
         ina = ina+inc
      end do
      return
#ifdef LOGENTRY 
$Log: whenne.F,v $
c Revision 1.1.1.1  1995/02/09  23:26:18  ccm2
c Start Omega Model. Split into spectral/sld modules
c
c Revision 1.1  1994/09/16  21:10:31  rosinski
c This is the first part of plx23.
c 
#endif
      end

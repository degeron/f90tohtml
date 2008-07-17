#include <params.h>
c----------
      subroutine whenfgt(n,array,inc,target,index,nval)
c
c $Id: whenfgt.F,v 1.1.1.1 1995/02/09 23:26:17 ccm2 Exp $
c $Author: ccm2 $
c
	dimension index(*), array(*)
	ina=1
	nval=0
	if(inc .lt. 0) ina=(-inc)*(n-1)+1
	do 100 i=1,n
	    if(array(ina) .gt. target) then
	    nval=nval+1
	    index(nval)=i
	    end if
	    ina=ina+inc
 100    continue
      return
#ifdef LOGENTRY 
$Log: whenfgt.F,v $
c Revision 1.1.1.1  1995/02/09  23:26:17  ccm2
c Start Omega Model. Split into spectral/sld modules
c
c Revision 1.1  1994/09/16  20:03:55  ccm2
c First revision of plx22/util routines
c 
#endif
      end           

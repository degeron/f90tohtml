#include <params.h>
c---------
      subroutine whenflt(n,array,inc,target,index,nval)
c
c $Id: whenflt.F,v 1.1.1.1 1995/02/09 23:26:17 ccm2 Exp $
c $Author: ccm2 $
c
	dimension index(*), array(*)
	ina=1
	nval=0
	if(inc .lt. 0) ina=(-inc)*(n-1)+1
	do 100 i=1,n
	    if(array(ina) .lt. target) then
	    nval=nval+1
	    index(nval)=i
	    end if
	    ina=ina+inc
 100    continue
      return
#ifdef LOGENTRY 
$Log: whenflt.F,v $
c Revision 1.1.1.1  1995/02/09  23:26:17  ccm2
c Start Omega Model. Split into spectral/sld modules
c
c Revision 1.1  1994/09/16  20:03:57  ccm2
c First revision of plx22/util routines
c 
#endif
      end           

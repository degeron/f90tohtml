#include <params.h>
      subroutine radoz2(o3vmr   ,pint    ,plol    ,plos    )
C-----------------------------------------------------------------------
C
C Computes the path length integrals to the model interfaces given the 
C ozone volume mixing ratio
C
C---------------------------Code history--------------------------------
C
C Original version:     CCM1
C Standardized:         J. Rosinski, June 1992
C Reviewed:             J. Kiehl, B. Briegleb, August 1992
C Mixing ratio version: Bruce Biegleb, September 1992
C
C-----------------------------------------------------------------------
c
c $Id: radoz2.F,v 1.1 1997/11/03 22:21:34 zender Exp zender $
c $Author: zender $
c
#include <implicit.h>
C-----------------------------------------------------------------------
#include <prgrid.h>
C------------------------------Commons----------------------------------
#include <comozp.h>
C------------------------------Input arguments--------------------------
C
      real o3vmr(plond,plev)   ! ozone volume mixing ratio
      real pint(plond,plevp)   ! Model interface pressures
C
C----------------------------Output arguments---------------------------
C
      real plol(plond,plevp),  ! Ozone prs weighted path length (cm)
     $     plos(plond,plevp)   ! Ozone path length (cm)
C
C---------------------------Local workspace-----------------------------
C
      integer   i,             ! longitude index
     $          k              ! level index
C
C-----------------------------------------------------------------------
C
C Evaluate the ozone path length integrals to interfaces; 
C factors of .1 and .01 to convert pressures from cgs to mks:
C
      do i=1,plon
c++csz
c         plos(i,1) = 0.1*0.5*cplos*o3vmr(i,1)*pint(i,1)
c         plol(i,1) = 0.01*0.25*cplol*o3vmr(i,1)*pint(i,1)*pint(i,1)
         plos(i,1) = 0.1*cplos*o3vmr(i,1)*pint(i,1)
         plol(i,1) = 0.01*cplol*o3vmr(i,1)*pint(i,1)*pint(i,1)
c--csz
      end do
      do k=2,plevp
         do i=1,plon
            plos(i,k) = plos(i,k-1) + 0.1*cplos*o3vmr(i,k-1)*
     $                  (pint(i,k) - pint(i,k-1))
            plol(i,k) = plol(i,k-1) + 0.01*cplol*o3vmr(i,k-1)*
     $                  (pint(i,k)*pint(i,k) - pint(i,k-1)*pint(i,k-1))
         end do
      end do
C
      return
#ifdef LOGENTRY 
$Log: radoz2.F,v $
Revision 1.1  1997/11/03 22:21:34  zender
Initial revision

c Revision 1.1.1.1  1995/02/09  23:27:03  ccm2
c Start Omega Model. Split into spectral/sld modules
c
c Revision 1.2  1994/09/16  21:08:46  rosinski
c This is the first part of plx23.
c 
#endif
      end

#include <params.h>
      subroutine trcmix(pmid, clat, coslat, n2o, ch4, cfc11, cfc12)
c-------------------------------------------------------------
c Specify zonal mean mass mixing ratios of CH4, N2O, CFC11 and
c CFC12
c          Code: J.T.Kiehl November 21, 1994
C------------------------------Parameters-------------------------------
c
c $Id: trcmix.F,v 1.2 1995/02/17 21:28:58 jhack Exp $
c $Author: jhack $
c
#include <implicit.h>
#include <pmgrid.h>
c------------------------------input------------------------------------
      real pmid(plond,plev),       ! model pressures
     $     clat,                   ! current latitude in radians
     $     coslat                  ! cosine of latitude
c------------------------------output-----------------------------------
      real n2o(plond,plev),        ! nitrous oxide mass mixing ratio
     $     ch4(plond,plev),        ! methane mass mixing ratio
     $     cfc11(plond,plev),      ! cfc11 mass mixing ratio
     $     cfc12(plond,plev)       ! cfc12 mass mixing ratio
c------------------------------local------------------------------------
      integer i,                   ! longitude loop index
     $        k                    ! level index
      real dlat                    ! latitude in degrees
      real xn2o,                   ! pressure scale height for n2o
     $     xch4,                   ! pressure scale height for ch4
     $     xcfc11,                 ! pressure scale height for cfc11
     $     xcfc12                  ! pressure scale height for cfc12
      real ch40,                   ! tropospheric mass mixing ratio for ch4
     $     n2o0,                   ! tropospheric mass mixing ratio for n2o
     $     cfc110,                 ! tropospheric mass mixing ratio for cfc11
     $     cfc120                  ! tropospheric mass mixing ratio for cfc12
      real ptrop,                  ! pressure level of tropopause
     $     pratio                  ! pressure divided by ptrop
c
c tropospheric mass mixing ratios
      ch40 = 0.55241 * 1.714e-6
      n2o0 = 1.51913 * 0.311e-6
      cfc110 = 4.69548 * 0.280e-9
      cfc120 = 4.14307 * 0.503e-9
c set stratospheric scale height factor for gases
      dlat = abs(57.2958 * clat)
      if(dlat.le.45.0) then
        xn2o = 0.3478 + 0.00116 * dlat
        xch4 = 0.2353
        xcfc11 = 0.7273 + 0.00606 * dlat
        xcfc12 = 0.4000 + 0.00222 * dlat
      else
        xn2o = 0.4000 + 0.013333 * (dlat - 45)
        xch4 = 0.2353 + 0.0225489 * (dlat - 45)
        xcfc11 = 1.00 + 0.013333 * (dlat - 45)
        xcfc12 = 0.50 + 0.024444 * (dlat - 45)
      end if
c pressure of tropopause
      ptrop = 250.0e2 - 150.0e2*coslat**2.0
c
      do k = 1,plev
         do i = 1,plon
            if(pmid(i,k).ge.ptrop) then
              ch4(i,k) = ch40
              n2o(i,k) = n2o0
              cfc11(i,k) = cfc110
              cfc12(i,k) = cfc120
            else
              pratio = pmid(i,k)/ptrop
              ch4(i,k) = ch40 * (pratio)**xch4
              n2o(i,k) = n2o0 * (pratio)**xn2o
              cfc11(i,k) = cfc110 * (pratio)**xcfc11
              cfc12(i,k) = cfc120 * (pratio)**xcfc12
            end if
         end do
      end do
      return
#ifdef LOGENTRY
$Log: trcmix.F,v $
c Revision 1.2  1995/02/17  21:28:58  jhack
c Next rev of omega physics, ice, clouds, liquid water, convection, etc.
c
#endif
      end

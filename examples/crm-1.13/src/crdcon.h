c
c $Id: crdcon.h,v 1.1.1.1 1995/02/09 23:26:46 ccm2 Exp $
c $Author: ccm2 $
c
C
C Radiation constants
C
      common/crdcon/gravit  ,rga     ,cpair   ,epsilo  ,sslp    ,
     $              stebol  ,rgsslp  ,co2vmr  ,dpfo3   ,dpfco2  ,
     $              dayspy  ,pie
C
      real gravit,    ! Acceleration of gravity
     $     rga,       ! 1./gravit
     $     cpair,     ! Specific heat of dry air
     $     epsilo,    ! Ratio of mol. wght of H2O to dry air
     $     sslp,      ! Standard sea-level pressure
     $     stebol,    ! Stefan-Boltzmann's constant
     $     rgsslp,    ! 0.5/(gravit*sslp)
     $     co2vmr,    ! CO2 volume mixing ratio
     $     dpfo3,     ! Voigt correction factor for O3
     $     dpfco2,    ! Voigt correction factor for CO2
     $     dayspy,    ! Number of days per 1 year
     $     pie        ! 3.14.....
C
#ifdef LOGENTRY 
$Log: crdcon.h,v $
 * Revision 1.1.1.1  1995/02/09  23:26:46  ccm2
 * Start Omega Model. Split into spectral/sld modules
 *
 * Revision 1.1.1.1  1994/04/21  23:55:13  ccm2
 * First cvs version of plx22
 * 
#endif

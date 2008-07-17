c
c $Id: compbl.h,v 1.1.1.1 1995/02/09 23:26:43 ccm2 Exp $
c $Author: ccm2 $
c
C
C Pbl constants
C
      common /compbl/ betam   ,betas   ,betah   ,fak     ,g       ,
     $                onet    ,fakn    ,ricr    ,sffrac  ,vk      ,
     $                ccon    ,binm    ,binh
C
      real betam,  ! constant in wind gradient expression
     $     betas,  ! constant in surface layer gradient expression
     $     betah,  ! constant in temperature gradient expression 
     $     fak,    ! constant in surface temperature excess         
     $     g,      ! gravitational acceleration
     $     onet,   ! 1/3 power in wind gradient expression
     $     fakn,   ! constant in turbulent prandtl number
     $     ricr,   ! critical richardson number
     $     sffrac, ! surface layer fraction of boundary layer
     $     vk,     ! von karman's constant
     $     ccon,   ! fak * sffrac * vk
     $     binm,   ! betam * sffrac
     $     binh    ! betah * sffrac
C
#ifdef LOGENTRY 
$Log: compbl.h,v $
 * Revision 1.1.1.1  1995/02/09  23:26:43  ccm2
 * Start Omega Model. Split into spectral/sld modules
 *
 * Revision 1.1.1.1  1994/04/21  23:55:12  ccm2
 * First cvs version of plx22
 * 
#endif

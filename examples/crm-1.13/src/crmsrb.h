c     $Header: /home/zender/crm/RCS/crmsrb.h,v 1.1 1998/02/19 23:10:36 zender Exp zender $ -*-fortran-*- 

c     Purpose: Common block crmsrb stores surface radiation budget fields for output

c     Usage: crmsrb.h should be declared after params.h
c     The values of the variables in these common blocks are set with routine radcsw()
c     which is called by radctl() which is called by crm().

c     CCM does not declare nspint in prgrid.h (as it probably should) so redefine it here 
      integer bnd_nbr           ! Number of spectral intervals
      parameter(bnd_nbr=18)     ! Number of spectral intervals

c     TOA radiation budget
      common / crmtoa /         ! crmsrb is initialized in radcsw()
     $     alb_SW_TOA,          ! SW albedo at TOA
     $     alb_NIR_TOA,         ! NIR albedo at TOA
     $     alb_vsb_TOA,         ! Visible albedo at TOA
     $     alb_NIR_SW_TOA,      ! NIR to SW albedo ratio at TOA
     $     alb_NIR_vsb_TOA,     ! NIR to visible albedo ratio at TOA
     $     flx_SW_dwn_TOA,      ! SW flux downwelling at TOA
     $     flx_NIR_dwn_TOA,     ! NIR flux downwelling at TOA
     $     flx_vsb_dwn_TOA,     ! Visible flux downwelling at TOA
     $     flx_SW_up_TOA,       ! SW flux upwelling at TOA
     $     flx_NIR_up_TOA,      ! NIR flux upwelling at TOA
     $     flx_vsb_up_TOA       ! Visible flux upwelling at TOA

      real alb_SW_TOA(plond)    ! SW albedo at TOA
      real alb_NIR_TOA(plond)   ! NIR albedo at TOA
      real alb_vsb_TOA(plond)   ! Visible albedo at TOA
      real alb_NIR_SW_TOA(plond) ! NIR to SW albedo ratio at TOA
      real alb_NIR_vsb_TOA(plond) ! NIR to visible albedo ratio at TOA
      real flx_SW_dwn_TOA(plond) ! SW flux downwelling at TOA
      real flx_NIR_dwn_TOA(plond) ! NIR flux downwelling at TOA
      real flx_vsb_dwn_TOA(plond) ! Visible flux downwelling at TOA
      real flx_SW_up_TOA(plond) ! SW flux upwelling at TOA
      real flx_NIR_up_TOA(plond) ! NIR flux upwelling at TOA
      real flx_vsb_up_TOA(plond) ! Visible flux upwelling at TOA
      
c     Surface radiation budget
      common / crmsrb /         ! crmsrb is initialized in radcsw()
     $     dff_drc_SW_sfc,      ! Diffuse/direct SW downwelling flux ratio at surface
     $     dff_drc_vsb_sfc,     ! Diffuse/direct visible downwelling flux ratio at surface
     $     dff_drc_NIR_sfc,     ! Diffuse/direct NIR downwelling flux ratio at surface
     $     flx_SW_dwn_sfc,      ! SW flux downwelling at surface
     $     flx_NIR_dwn_sfc,     ! NIR flux downwelling at surface
     $     flx_vsb_dwn_sfc,     ! Visible flux downwelling at surface
     $     flx_SW_dwn_drc_sfc,  ! SW flux downwelling at surface in direct beam
     $     flx_NIR_dwn_drc_sfc, ! NIR flux downwelling at surface in direct beam
     $     flx_vsb_dwn_drc_sfc, ! Visible flux downwelling at surface in direct beam
     $     flx_SW_dwn_dff_sfc,  ! SW flux downwelling at surface in diffuse beam
     $     flx_NIR_dwn_dff_sfc, ! NIR flux downwelling at surface in diffuse beam
     $     flx_vsb_dwn_dff_sfc, ! Visible flux downwelling at surface in diffuse beam
     $     odxc_ttl             ! Optical depth extinction column total
      

      real dff_drc_SW_sfc(plond) ! Diffuse/direct SW downwelling flux ratio at surface
      real dff_drc_vsb_sfc(plond) ! Diffuse/direct visible downwelling flux ratio at surface
      real dff_drc_NIR_sfc(plond) ! Diffuse/direct NIR downwelling flux ratio at surface
      real flx_SW_dwn_sfc(plond) ! SW flux downwelling at surface
      real flx_NIR_dwn_sfc(plond) ! NIR flux downwelling at surface
      real flx_vsb_dwn_sfc(plond) ! Visible flux downwelling at surface
      real flx_SW_dwn_drc_sfc(plond) ! SW flux downwelling at surface in direct beam
      real flx_NIR_dwn_drc_sfc(plond) ! NIR flux downwelling at surface in direct beam
      real flx_vsb_dwn_drc_sfc(plond) ! Visible flux downwelling at surface in direct beam
      real flx_SW_dwn_dff_sfc(plond) ! SW flux downwelling at surface in diffuse beam
      real flx_NIR_dwn_dff_sfc(plond) ! NIR flux downwelling at surface in diffuse beam
      real flx_vsb_dwn_dff_sfc(plond) ! Visible flux downwelling at surface in diffuse beam
      real odxc_ttl(plond,bnd_nbr) ! Optical depth extinction column total



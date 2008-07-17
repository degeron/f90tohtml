c     $Header: /home/zender/crm/RCS/netcdf.F,v 1.4 1998/02/17 05:56:25 zender Exp zender $

c     Purpose: provide netCDF output for the CCM Column Radiation Model (CRM).

c     netCDF always generates warning messages in FORTRAN because of the
c     include 'netcdf.inc' line, but don't worry about these.

c     When DOUBLE token is defined, this routine assumes all floating point 
c     input variables have been promoted from real*4 to real*8 by the compiler
c     (e.g., by using -r8 -i4 compiler switch on Suns). 
c     The following #define's then cause the default code to produce a valid,
c     albeit double precision, netCDF output file.
c     NB: This preprocessor block MUST come after #include <netcdf.inc>
#ifdef DOUBLE
#define netCDF_float_type_name NF_DOUBLE
#define netCDF_put_float_function_name nf_put_var_double 
#else
#define netCDF_float_type_name NF_FLOAT
#define netCDF_put_float_function_name nf_put_var_real
#endif

#include <params.h>
      subroutine netcdf(
     $     cld,
     $     clwp,
     $     coszrs,
     $     effcld,
     $     fice,
c     
     $     flns,
     $     flnsc,
     $     flnt,
     $     flntc,
     $     flwds,
c     
     $     fsns,
     $     fsnsc,
     $     fsnt,
     $     fsntc,
     $     ioro,
c     
     $     loctim,
     $     lwup,
     $     pintm1,
     $     pmidm1,
     $     ps,
     $     o3mmr,
c     
     $     o3vmr,
     $     qm1,
     $     qrl,
     $     qrs,
     $     rei,
c     
     $     rel,
     $     sndpth,
     $     solin,
     $     srfrad,
     $     tg,
c     
     $     tm1,
     $     ts)

      implicit none
#ifdef RS6K
      implicit automatic (a-z)
#endif
c     Parameters
#include <prgrid.h>
c     Commons
c     Arguments
c     Input arguments
      integer ioro              ! land/ocean/sea ice flag

c     Sun-Earth Geometry
      real clat                 ! Current latitude (radians)
      real coszrs               ! cosine solar zenith angle
      real loctim               ! local time of solar computation

c     Column thermodynamic profile
      real cld(plevp)           ! fractional cloud cover
      real clwp(plev)           ! cloud liquid water path
      real effcld(plevp)        ! effective fractional cloud cover
      real o3mmr(plev)          ! Ozone mass mixing ratio
      real o3vmr(plev)          ! Ozone volume mixing ratio
      real pintm1(plevp)        ! model interface pressures
      real pmidm1(plev)         ! model level pressures
      real ps                   ! surface pressure
      real qm1(plev)            ! model level specific humidity
      real sndpth               ! snow depth (liquid water equivalent)
      real tg                   ! surface (skin) temperature
      real tm1(plev)            ! model level temperatures
      real ts                   ! surface air temperature

c     Cloud microphysics
      real fice(plev)           ! fractional amount of ice
      real rei(plev)            ! ice particle size
      real rel(plev)            ! liquid effective drop size (microns)

c     Shortwave
      real fsns                 ! surface absorbed solar flux
      real fsnsc                ! clr sky surface abs solar flux
      real fsnt                 ! total column absorbed solar flux
      real fsntc                ! clr sky total column abs solar flux
      real qrs(plev)            ! solar heating rate
      real solin                ! solar incident flux
      real srfrad               ! srf radiative heat flux

c     Longwave
      real flns                 ! srf longwave cooling (up-dwn) flux
      real flnsc                ! clr sky lw flx at srf (up-dwn)
      real flnt                 ! net outgoing lw flx at model top
      real flntc                ! clr sky lw flx at model top
      real flwds                ! Surface down longwave flux
      real lwup                 ! Surface up longwave flux from coupler
      real qrl(plev)            ! longwave cooling rate

c     Local workspace
c     Externals

c     Local variables
#include <netcdf.inc>

      character fl_out*80

      integer nc_id             ! file handle
      integer rcd               ! return success code

      integer plev_dim_id       ! dimension id for plev
      integer plevp_dim_id      ! dimension id for plevp

#ifdef CRM_SRB
#include <crmsrb.h>
      integer bnd_dim_id        ! Dimension ID for bnd

      integer dff_drc_SW_sfc_id
      integer dff_drc_NIR_sfc_id
      integer dff_drc_vsb_sfc_id
      integer flx_SW_dwn_sfc_id
      integer flx_NIR_dwn_sfc_id
      integer flx_vsb_dwn_sfc_id
      integer flx_SW_dwn_drc_sfc_id
      integer flx_NIR_dwn_drc_sfc_id
      integer flx_vsb_dwn_drc_sfc_id
      integer flx_SW_dwn_dff_sfc_id
      integer flx_NIR_dwn_dff_sfc_id
      integer flx_vsb_dwn_dff_sfc_id
      integer odxc_ttl_id
#endif

      integer clat_id   
      integer cld_id   
      integer clwp_id
      integer coszrs_id
      integer effcld_id
      integer fice_id
      integer flns_id
      integer flnsc_id
      integer flnt_id
      integer flntc_id
      integer flwds_id
      integer fsns_id
      integer fsnsc_id
      integer fsnt_id
      integer fsntc_id
      integer ioro_id
      integer loctim_id
      integer lwup_id
      integer o3mmr_id
      integer o3vmr_id
      integer pintm1_id
      integer plev_id
      integer plevp_id
      integer pmidm1_id
      integer ps_id
      integer qm1_id
      integer qrl_id
      integer qrs_id
      integer rei_id
      integer rel_id
      integer sndpth_id
      integer solin_id
      integer srfrad_id
      integer tg_id
      integer tm1_id
      integer ts_id

c     Main Code
      fl_out='crm.nc'
      rcd=nf_create(fl_out,NF_CLOBBER,nc_id)

c     Define dimensions
      rcd=nf_def_dim(nc_id,'plev',plev,plev_dim_id)
      rcd=nf_def_dim(nc_id,'plevp',plevp,plevp_dim_id)

c     Define scalars
      rcd=nf_def_var(nc_id,'clat',netCDF_float_type_name,0,0,clat_id)
      rcd=nf_def_var(nc_id,'coszrs',netCDF_float_type_name,0,0,coszrs_id)
      rcd=nf_def_var(nc_id,'flns',netCDF_float_type_name,0,0,flns_id)
      rcd=nf_def_var(nc_id,'flnsc',netCDF_float_type_name,0,0,flnsc_id)
      rcd=nf_def_var(nc_id,'flnt',netCDF_float_type_name,0,0,flnt_id)
      rcd=nf_def_var(nc_id,'flntc',netCDF_float_type_name,0,0,flntc_id)
      rcd=nf_def_var(nc_id,'flwds',netCDF_float_type_name,0,0,flwds_id)
      rcd=nf_def_var(nc_id,'fsns',netCDF_float_type_name,0,0,fsns_id)
      rcd=nf_def_var(nc_id,'fsnsc',netCDF_float_type_name,0,0,fsnsc_id)
      rcd=nf_def_var(nc_id,'fsnt',netCDF_float_type_name,0,0,fsnt_id)
      rcd=nf_def_var(nc_id,'fsntc',netCDF_float_type_name,0,0,fsntc_id)
      rcd=nf_def_var(nc_id,'ioro',NF_INT,0,0,ioro_id)
      rcd=nf_def_var(nc_id,'loctim',netCDF_float_type_name,0,0,loctim_id)
      rcd=nf_def_var(nc_id,'lwup',netCDF_float_type_name,0,0,lwup_id)
      rcd=nf_def_var(nc_id,'ps',netCDF_float_type_name,0,0,ps_id)
      rcd=nf_def_var(nc_id,'sndpth',netCDF_float_type_name,0,0,sndpth_id)
      rcd=nf_def_var(nc_id,'solin',netCDF_float_type_name,0,0,solin_id)
      rcd=nf_def_var(nc_id,'srfrad',netCDF_float_type_name,0,0,srfrad_id)
      rcd=nf_def_var(nc_id,'tg',netCDF_float_type_name,0,0,tg_id)
      rcd=nf_def_var(nc_id,'ts',netCDF_float_type_name,0,0,ts_id)

c     Define plev arrays
      rcd=nf_def_var(nc_id,'clwp',netCDF_float_type_name,1,plev_dim_id,clwp_id)
      rcd=nf_def_var(nc_id,'fice',netCDF_float_type_name,1,plev_dim_id,fice_id)
      rcd=nf_def_var(nc_id,'o3mmr',netCDF_float_type_name,1,plev_dim_id,o3mmr_id)
      rcd=nf_def_var(nc_id,'o3vmr',netCDF_float_type_name,1,plev_dim_id,o3vmr_id)
      rcd=nf_def_var(nc_id,'plev',netCDF_float_type_name,1,plev_dim_id,plev_id)
      rcd=nf_def_var(nc_id,'pmid',netCDF_float_type_name,1,plev_dim_id,pmidm1_id)
      rcd=nf_def_var(nc_id,'h2ommr',netCDF_float_type_name,1,plev_dim_id,qm1_id)
      rcd=nf_def_var(nc_id,'qrl',netCDF_float_type_name,1,plev_dim_id,qrl_id)
      rcd=nf_def_var(nc_id,'qrs',netCDF_float_type_name,1,plev_dim_id,qrs_id)
      rcd=nf_def_var(nc_id,'rei',netCDF_float_type_name,1,plev_dim_id,rei_id)
      rcd=nf_def_var(nc_id,'rel',netCDF_float_type_name,1,plev_dim_id,rel_id)
      rcd=nf_def_var(nc_id,'t',netCDF_float_type_name,1,plev_dim_id,tm1_id)

c     Define plevp arrays
      rcd=nf_def_var(nc_id,'cld',netCDF_float_type_name,1,plevp_dim_id,cld_id)
      rcd=nf_def_var(nc_id,'effcld',netCDF_float_type_name,1,plevp_dim_id,effcld_id)
      rcd=nf_def_var(nc_id,'pint',netCDF_float_type_name,1,plevp_dim_id,pintm1_id)
      rcd=nf_def_var(nc_id,'plevp',netCDF_float_type_name,1,plevp_dim_id,plevp_id)

c     Add units
      rcd=nf_put_att_text(nc_id,clat_id,'units',6,'radian')
      rcd=nf_put_att_text(nc_id,cld_id,'units',8,'fraction')
      rcd=nf_put_att_text(nc_id,clwp_id,'units',12,'gram meter-2')
      rcd=nf_put_att_text(nc_id,coszrs_id,'units',8,'fraction')
      rcd=nf_put_att_text(nc_id,effcld_id,'units',8,'fraction')
      rcd=nf_put_att_text(nc_id,fice_id,'units',8,'fraction')
      rcd=nf_put_att_text(nc_id,flns_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,flnsc_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,flnt_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,flntc_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,flwds_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,fsns_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,fsnsc_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,fsnt_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,fsntc_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,ioro_id,'units',7,'integer')
      rcd=nf_put_att_text(nc_id,loctim_id,'units',8,'fraction')
      rcd=nf_put_att_text(nc_id,lwup_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,o3mmr_id,'units',8,'fraction')
      rcd=nf_put_att_text(nc_id,o3vmr_id,'units',8,'fraction')
      rcd=nf_put_att_text(nc_id,pintm1_id,'units',6,'pascal')
      rcd=nf_put_att_text(nc_id,plev_id,'units',6,'pascal')
      rcd=nf_put_att_text(nc_id,plevp_id,'units',6,'pascal')
      rcd=nf_put_att_text(nc_id,pmidm1_id,'units',6,'pascal')
      rcd=nf_put_att_text(nc_id,ps_id,'units',6,'pascal')
      rcd=nf_put_att_text(nc_id,qm1_id,'units',8,'fraction')
      rcd=nf_put_att_text(nc_id,qrl_id,'units',15,'kelvin second-1')
      rcd=nf_put_att_text(nc_id,qrs_id,'units',15,'kelvin second-1')
      rcd=nf_put_att_text(nc_id,rei_id,'units',6,'micron')
      rcd=nf_put_att_text(nc_id,rel_id,'units',6,'micron')
      rcd=nf_put_att_text(nc_id,sndpth_id,'units',5,'meter')
      rcd=nf_put_att_text(nc_id,solin_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,srfrad_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,tg_id,'units',6,'kelvin')
      rcd=nf_put_att_text(nc_id,tm1_id,'units',6,'kelvin')
      rcd=nf_put_att_text(nc_id,ts_id,'units',6,'kelvin')

c     Add English text descriptions
      rcd=nf_put_att_text(nc_id,clat_id,'long_name',8,'Latitude')
      rcd=nf_put_att_text(nc_id,cld_id,'long_name',14,'Cloud fraction')
      rcd=nf_put_att_text(nc_id,clwp_id,'long_name',24,'In-cloud condensate path')
      rcd=nf_put_att_text(nc_id,coszrs_id,'long_name',25,'Cosine solar zenith angle')
      rcd=nf_put_att_text(nc_id,effcld_id,'long_name',52,'Effective cloud amount (emissivity x cloud fraction)')
      rcd=nf_put_att_text(nc_id,fice_id,'long_name',12,'Ice fraction')
      rcd=nf_put_att_text(nc_id,flns_id,'long_name',37,'Net longwave flux at surface')
      rcd=nf_put_att_text(nc_id,flnsc_id,'long_name',40,'Net longwave flux at surface')
      rcd=nf_put_att_text(nc_id,flnt_id,'long_name',33,'Net longwave flux at TOA')
      rcd=nf_put_att_text(nc_id,flntc_id,'long_name',35,'Net longwave flux at TOA')
      rcd=nf_put_att_text(nc_id,flwds_id,'long_name',36,'Longwave downwelling flux at surface')
      rcd=nf_put_att_text(nc_id,fsns_id,'long_name',38,'Net shortwave flux at surface')
      rcd=nf_put_att_text(nc_id,fsnsc_id,'long_name',40,'Net shortwave flux at surface')
      rcd=nf_put_att_text(nc_id,fsnt_id,'long_name',34,'Net shortwave flux at TOA')
      rcd=nf_put_att_text(nc_id,fsntc_id,'long_name',30,'Net shortwave flux at TOA')
      rcd=nf_put_att_text(nc_id,ioro_id,'long_name',14,'Orography flag')
      rcd=nf_put_att_text(nc_id,loctim_id,'long_name',10,'Local time')
      rcd=nf_put_att_text(nc_id,lwup_id,'long_name',38,'Longwave up flux from coupler (if any)')
      rcd=nf_put_att_text(nc_id,o3mmr_id,'long_name',27,'Ozone dry mass-mixing ratio')
      rcd=nf_put_att_text(nc_id,o3vmr_id,'long_name',29,'Ozone dry volume-mixing ratio')
      rcd=nf_put_att_text(nc_id,pintm1_id,'long_name',28,'Pressure at layer interfaces')
      rcd=nf_put_att_text(nc_id,plev_id,'long_name',16,'Number of levels')
      rcd=nf_put_att_text(nc_id,plevp_id,'long_name',26,'Number of level interfaces')
      rcd=nf_put_att_text(nc_id,pmidm1_id,'long_name',27,'Pressure at layer midpoints')
      rcd=nf_put_att_text(nc_id,ps_id,'long_name',16,'Surface pressure')
      rcd=nf_put_att_text(nc_id,qm1_id,'long_name',33,'Water vapor dry mass-mixing ratio')
      rcd=nf_put_att_text(nc_id,qrl_id,'long_name',21,'Longwave heating rate')
      rcd=nf_put_att_text(nc_id,qrs_id,'long_name',22,'Shortwave heating rate')
      rcd=nf_put_att_text(nc_id,rei_id,'long_name',32,'Effective radius of ice crystals')
      rcd=nf_put_att_text(nc_id,rel_id,'long_name',35,'Effective radius of liquid droplets')
      rcd=nf_put_att_text(nc_id,sndpth_id,'long_name',10,'Snow depth')
      rcd=nf_put_att_text(nc_id,solin_id,'long_name',33,'Downwelling shortwave flux at TOA')
      rcd=nf_put_att_text(nc_id,srfrad_id,'long_name',33,'Total radiative flux into surface')
      rcd=nf_put_att_text(nc_id,tg_id,'long_name',18,'Ground temperature')
      rcd=nf_put_att_text(nc_id,tm1_id,'long_name',11,'Temperature')
      rcd=nf_put_att_text(nc_id,ts_id,'long_name',16,'Skin temperature')

c     End define mode
      rcd=nf_enddef(nc_id)

c     Write data
      rcd=nf_put_var_int(nc_id,ioro_id,ioro)
      rcd=netCDF_put_float_function_name(nc_id,clat_id,clat)
      rcd=netCDF_put_float_function_name(nc_id,cld_id,cld)
      rcd=netCDF_put_float_function_name(nc_id,clwp_id,clwp)
      rcd=netCDF_put_float_function_name(nc_id,coszrs_id,coszrs)
      rcd=netCDF_put_float_function_name(nc_id,effcld_id,effcld)
      rcd=netCDF_put_float_function_name(nc_id,fice_id,fice)
      rcd=netCDF_put_float_function_name(nc_id,flns_id,flns)
      rcd=netCDF_put_float_function_name(nc_id,flnsc_id,flnsc)
      rcd=netCDF_put_float_function_name(nc_id,flnt_id,flnt)
      rcd=netCDF_put_float_function_name(nc_id,flntc_id,flntc)
      rcd=netCDF_put_float_function_name(nc_id,flwds_id,flwds)
      rcd=netCDF_put_float_function_name(nc_id,fsns_id,fsns)
      rcd=netCDF_put_float_function_name(nc_id,fsnsc_id,fsnsc)
      rcd=netCDF_put_float_function_name(nc_id,fsnt_id,fsnt)
      rcd=netCDF_put_float_function_name(nc_id,fsntc_id,fsntc)
      rcd=netCDF_put_float_function_name(nc_id,loctim_id,loctim)
      rcd=netCDF_put_float_function_name(nc_id,lwup_id,lwup)
      rcd=netCDF_put_float_function_name(nc_id,o3mmr_id,o3mmr)
      rcd=netCDF_put_float_function_name(nc_id,o3vmr_id,o3vmr)
      rcd=netCDF_put_float_function_name(nc_id,pintm1_id,pintm1)
      rcd=netCDF_put_float_function_name(nc_id,plev_id,pmidm1)
      rcd=netCDF_put_float_function_name(nc_id,plevp_id,pintm1)
      rcd=netCDF_put_float_function_name(nc_id,pmidm1_id,pmidm1)
      rcd=netCDF_put_float_function_name(nc_id,ps_id,ps)
      rcd=netCDF_put_float_function_name(nc_id,qm1_id,qm1)
      rcd=netCDF_put_float_function_name(nc_id,qrl_id,qrl)
      rcd=netCDF_put_float_function_name(nc_id,qrs_id,qrs)
      rcd=netCDF_put_float_function_name(nc_id,rei_id,rei)
      rcd=netCDF_put_float_function_name(nc_id,rel_id,rel)
      rcd=netCDF_put_float_function_name(nc_id,sndpth_id,sndpth)
      rcd=netCDF_put_float_function_name(nc_id,solin_id,solin)
      rcd=netCDF_put_float_function_name(nc_id,srfrad_id,srfrad)
      rcd=netCDF_put_float_function_name(nc_id,tg_id,tg)
      rcd=netCDF_put_float_function_name(nc_id,tm1_id,tm1)
      rcd=netCDF_put_float_function_name(nc_id,ts_id,ts)

      rcd=nf_close(nc_id)
      write(6,'(2a)') 'Wrote results to ',fl_out

#ifdef CRM_SRB
      rcd=nf_open(fl_out,NF_WRITE,nc_id)
      if (rcd.ne.NF_NOERR) call nc_err_exit(rcd,fl_out)

c     Put output file in define mode 
      rcd=nf_redef(nc_id)
      if (rcd.ne.NF_NOERR) call nc_err_exit(rcd,fl_out)

c     Define dimension IDs
      rcd=nf_def_dim(nc_id,'bnd',bnd_nbr,bnd_dim_id)
      if (rcd.ne.NF_NOERR) call nc_err_exit(rcd,fl_out)

c     Define scalars
      rcd=nf_def_var(nc_id,'dff_drc_SW_sfc',netCDF_float_type_name,0,0,dff_drc_SW_sfc_id)
      rcd=nf_def_var(nc_id,'dff_drc_NIR_sfc',netCDF_float_type_name,0,0,dff_drc_NIR_sfc_id)
      rcd=nf_def_var(nc_id,'dff_drc_vsb_sfc',netCDF_float_type_name,0,0,dff_drc_vsb_sfc_id)
      rcd=nf_def_var(nc_id,'flx_SW_dwn_sfc',netCDF_float_type_name,0,0,flx_SW_dwn_sfc_id)
      rcd=nf_def_var(nc_id,'flx_NIR_dwn_sfc',netCDF_float_type_name,0,0,flx_NIR_dwn_sfc_id)
      rcd=nf_def_var(nc_id,'flx_vsb_dwn_sfc',netCDF_float_type_name,0,0,flx_vsb_dwn_sfc_id)
      rcd=nf_def_var(nc_id,'flx_SW_dwn_drc_sfc',netCDF_float_type_name,0,0,flx_SW_dwn_drc_sfc_id)
      rcd=nf_def_var(nc_id,'flx_NIR_dwn_drc_sfc',netCDF_float_type_name,0,0,flx_NIR_dwn_drc_sfc_id)
      rcd=nf_def_var(nc_id,'flx_vsb_dwn_drc_sfc',netCDF_float_type_name,0,0,flx_vsb_dwn_drc_sfc_id)
      rcd=nf_def_var(nc_id,'flx_SW_dwn_dff_sfc',netCDF_float_type_name,0,0,flx_SW_dwn_dff_sfc_id)
      rcd=nf_def_var(nc_id,'flx_NIR_dwn_dff_sfc',netCDF_float_type_name,0,0,flx_NIR_dwn_dff_sfc_id)
      rcd=nf_def_var(nc_id,'flx_vsb_dwn_dff_sfc',netCDF_float_type_name,0,0,flx_vsb_dwn_dff_sfc_id)
c     Define bnd arrays
      rcd=nf_def_var(nc_id,'odxc_ttl',netCDF_float_type_name,1,bnd_dim_id,odxc_ttl_id)

c     Add units
      rcd=nf_put_att_text(nc_id,dff_drc_SW_sfc_id,'units',8,'fraction')
      rcd=nf_put_att_text(nc_id,dff_drc_NIR_sfc_id,'units',8,'fraction')
      rcd=nf_put_att_text(nc_id,dff_drc_vsb_sfc_id,'units',8,'fraction')
      rcd=nf_put_att_text(nc_id,flx_SW_dwn_sfc_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,flx_NIR_dwn_sfc_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,flx_vsb_dwn_sfc_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,flx_SW_dwn_drc_sfc_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,flx_NIR_dwn_drc_sfc_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,flx_vsb_dwn_drc_sfc_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,flx_SW_dwn_dff_sfc_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,flx_NIR_dwn_dff_sfc_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,flx_vsb_dwn_dff_sfc_id,'units',12,'watt meter-2')
      rcd=nf_put_att_text(nc_id,odxc_ttl_id,'units',8,'fraction')

c     Add English text descriptions
      rcd=nf_put_att_text(nc_id,dff_drc_SW_sfc_id,'long_name',51,'Diffuse/direct SW downwelling flux ratio at surface')
      rcd=nf_put_att_text(nc_id,dff_drc_NIR_sfc_id,'long_name',52,'Diffuse/direct NIR downwelling flux ratio at surface')
      rcd=nf_put_att_text(nc_id,dff_drc_vsb_sfc_id,'long_name',56,'Diffuse/direct visible downwelling flux ratio at surface')
      rcd=nf_put_att_text(nc_id,flx_SW_dwn_sfc_id,'long_name',30,'SW flux downwelling at surface')
      rcd=nf_put_att_text(nc_id,flx_NIR_dwn_sfc_id,'long_name',31,'NIR flux downwelling at surface')
      rcd=nf_put_att_text(nc_id,flx_vsb_dwn_sfc_id,'long_name',35,'Visible flux downwelling at surface')
      rcd=nf_put_att_text(nc_id,flx_SW_dwn_drc_sfc_id,'long_name',45,'SW flux downwelling at surface in direct beam')
      rcd=nf_put_att_text(nc_id,flx_NIR_dwn_drc_sfc_id,'long_name',46,'NIR flux downwelling at surface in direct beam')
      rcd=nf_put_att_text(nc_id,flx_vsb_dwn_drc_sfc_id,'long_name',50,'Visible flux downwelling at surface in direct beam')
      rcd=nf_put_att_text(nc_id,flx_SW_dwn_dff_sfc_id,'long_name',46,'SW flux downwelling at surface in diffuse beam')
      rcd=nf_put_att_text(nc_id,flx_NIR_dwn_dff_sfc_id,'long_name',47,'NIR flux downwelling at surface in diffuse beam')
      rcd=nf_put_att_text(nc_id,flx_vsb_dwn_dff_sfc_id,'long_name',51,'Visible flux downwelling at surface in diffuse beam')
      rcd=nf_put_att_text(nc_id,odxc_ttl_id,'long_name',37,'Optical depth extinction column total')

c     End define mode
      rcd=nf_enddef(nc_id)

c     Write data
      rcd=netCDF_put_float_function_name(nc_id,dff_drc_SW_sfc_id,dff_drc_SW_sfc)
      rcd=netCDF_put_float_function_name(nc_id,dff_drc_NIR_sfc_id,dff_drc_NIR_sfc)
      rcd=netCDF_put_float_function_name(nc_id,dff_drc_vsb_sfc_id,dff_drc_vsb_sfc)
      rcd=netCDF_put_float_function_name(nc_id,flx_SW_dwn_sfc_id,flx_SW_dwn_sfc)
      rcd=netCDF_put_float_function_name(nc_id,flx_NIR_dwn_sfc_id,flx_NIR_dwn_sfc)
      rcd=netCDF_put_float_function_name(nc_id,flx_vsb_dwn_sfc_id,flx_vsb_dwn_sfc)
      rcd=netCDF_put_float_function_name(nc_id,flx_SW_dwn_drc_sfc_id,flx_SW_dwn_drc_sfc)
      rcd=netCDF_put_float_function_name(nc_id,flx_NIR_dwn_drc_sfc_id,flx_NIR_dwn_drc_sfc)
      rcd=netCDF_put_float_function_name(nc_id,flx_vsb_dwn_drc_sfc_id,flx_vsb_dwn_drc_sfc)
      rcd=netCDF_put_float_function_name(nc_id,flx_SW_dwn_dff_sfc_id,flx_SW_dwn_dff_sfc)
      rcd=netCDF_put_float_function_name(nc_id,flx_NIR_dwn_dff_sfc_id,flx_NIR_dwn_dff_sfc)
      rcd=netCDF_put_float_function_name(nc_id,flx_vsb_dwn_dff_sfc_id,flx_vsb_dwn_dff_sfc)
      rcd=netCDF_put_float_function_name(nc_id,odxc_ttl_id,odxc_ttl)

      rcd=nf_close(nc_id)
      write(6,'(2a)') 'Added SRB results to ',fl_out
#endif

      return
      end

      subroutine nc_err_exit(rcd,err_msg_sng)
c     Routine to print netCDF error message, routine name, and exit
      implicit none
c     Commons
#include <netcdf.inc>
c     Local Workspace
      character*(*) err_msg_sng ! Message to print before exiting
      integer rcd               ! Return code
      integer err_msg_sng_len   ! Length of custom message

c     Main Code
      err_msg_sng_len=len(err_msg_sng)
      write (6,'(a,a2,a)') err_msg_sng(1:err_msg_sng_len),', ',nf_strerror(rcd)
      stop 
      end                       ! end nc_err_exit()

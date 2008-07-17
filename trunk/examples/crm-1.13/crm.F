c     $Header: /home/zender/crm/RCS/crm.F,v 1.12 1998/02/19 21:58:51 zender Exp zender $

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c-----------------------------NOTICE------------------------------------
c
c            NCAR COMMUNITY CLIMATE MODEL, VERSION 3.0
c            COPYRIGHT (C) 1996
c            UNIVERSITY CORPORATION FOR ATMOSPHERIC RESEARCH
c            ALL RIGHTS RESERVED
c
c               ------------ ----- --- ---------- ------
c  ********** | distribution terms and conditions notice | ************
c               ------------ ----- --- ---------- ------
c
c (c) copyright 1996 university corporation for atmospheric research/
c national center for atmospheric research/
c climate and global dynamics division
c
c this software, the community climate model (ccm), version ccm3, was
c developed by the climate and global dynamics division (cgd) climate
c modeling section (cms) of the national center for atmospheric research
c (ncar), which is operated by the university corporation for
c atmospheric research (ucar) and sponsored by the national science
c foundation (nsf).
c
c access and use of this software shall impose the following obligations
c and understandings on the user.  the user is granted the right,
c without any fee or cost, to use, copy, modify, alter, enhance and
c distribute this software, and any derivative works thereof, and its
c supporting documentation for any purpose whatsoever, except commercial
c sales, provided that this entire notice appears in all copies of the
c software, derivative works and supporting documentation.  further, the
c user agrees to credit ucar/ncar/cgd in any publications that result
c from the use of this software or in any software package that includes
c this software.  the names ucar/ncar/cgd, however, may not be used in
c any advertising or publicity to endorse or promote any products or
c commercial entity unless specific written permission is obtained from
c ucar/ncar/cgd.
c
c the ccm3 materials are made available with the understanding that
c ucar/ncar/cgd is not obligated to provide (and will not provide) the
c user with any support, consulting, training, or assistance of any kind
c with regard to the use, operation and performance of this software, nor
c to provide the user with any updates, revisions, new versions, or "bug
c fixes."
c
c this software is provided by ucar/ncar/cgd "as is" and any express or
c implied warranties, including but not limited to, the implied
c warranties of merchantability and fitness for a particular purpose are
c disclaimed.  in no event shall ucar/ncar/cgd be liable for any
c special, indirect or consequential damages or any damages whatsoever,
c including but not limited to claims associated with the loss of data
c or profits, which may result from an action in contract, negligence or
c other tortious claim that arises out of or in connection with the
c access, use or performance of this software.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c     CCM3 Column Radiation Model 
c     Jeffrey Kiehl, Bruce Briegleb, and Charlie Zender  
c     May 1996

c     All routines except crm(), getdat(), radctl(), and netcdf() and four dummy
c     routines (readric(), writeric(), outfld(), and radozn()) are #included 
c     straight from CCM Omega v.5 code. This code is identical to CCM3 code
c     but does not invoke the Land Surface Model for albedo computation. 
c     The purpose of the (non-dummy) routines is:

c     crm(): the main() routine. This routine takes the place of tphysbc()
c     in the ccm3 calling tree. It places the required calls to the cloud
c     routines cldefr() and cldems() directly, rather than invoking cldint().

c     getdat(): reads the stdin file to parse the user-specified column
c     profile. overwrites the co2vmr previsouly set by radini() with the
c     user specified value. computes o3vmr (instead of in radozn()).
c     sets the calday variable in comtim.h used in zenith() and radinp().

c     radctl(): main radiation driving routine, same as ccm3 version except:
c     receives additional input variable o3vmr, and passes out
c     additional diagnostic radiation quantities and eccf usually local to radctl().
c     does cgs-->mks conversion for all fluxes.

c     netcdf(): optional convenience routine to output a netCDF file
c     named crm.nc containing most of the data.

c     radcsw(): altered to compute SRB quantities and pass them into crmsrb.h

c     The files that need to be either #included, specified in the compilation
c     statement, or linked from a ccm3 library are...

c     aermix.F     fmrgrid.F    radclr.F     radinp.F     trcabn.F     whenfgt.F
c     albland.F    intmax.F     radclw.F     radoz2.F     trcems.F     whenflt.F
c     albocean.F   isrchfgt.F                radtpl.F     trcmix.F     whenne.F
c     blkdat.F     isrchfle.F   radded.F     resetr.F     trcplk.F     zenith.F
c     cldefr.F     myhandler.F  radems.F     torgrid.F    trcpth.F
c     cldems.F     radabs.F     radini.F     trcab.F      wheneq.F

#include <params.h>

#include <aermix.F>
#include <albland.F>
#include <albocean.F>
#include <blkdat.F>
#include <cldefr.F>
#include <cldems.F>
#include <fmrgrid.F>
#include <radabs.F>
#include <radclr.F>
#include <radclw.F>
#include <radcsw.F>
#include <radded.F>
#include <radems.F>
#include <radini.F>
#include <radinp.F>
#include <radoz2.F>
#include <radtpl.F>
#include <resetr.F>
#include <torgrid.F>
#include <trcab.F>
#include <trcabn.F>
#include <trcems.F>
#include <trcmix.F>
#include <trcplk.F>
#include <trcpth.F>
#include <zenith.F>
#ifdef NETCDF
#include <netcdf.F>
#endif /* end if netCDF output will be enabled */ 
#ifdef SUN
#include <myhandler.F>
#endif /* end if machine is SUN */ 
#ifndef CRAY
#include <intmax.F>
#include <isrchfgt.F>
#include <isrchfle.F>
#include <wheneq.F>
#include <whenfgt.F>
#include <whenflt.F>
#include <whenne.F>
#endif /* end if machine is SUN or RS6K */ 

      program crm

#include <implicit.h>
c     Parameters
#include <prgrid.h>
c     Commons
#include <comtim.h>
#include <comctl.h>
#ifdef CRM_SRB
#include <crmsrb.h>
#endif
c     Arguments
c     Fields specified by the user in getdat()
      integer ioro(plond)       ! land/ocean/sea ice flag

      real clat                 ! Current latitude (radians)
      real cld(plond,plevp)     ! fractional cloud cover
      real clwp(plond,plev)     ! cloud liquid water path
      real coslat               ! cosine latitude

c     NB: o3mmr and o3vmr should be dimensioned (plond,plevr) if a different 
c     size radiation grid is used. Clashes between prgrid.h and ptrrgrid.h
c     (they both define plngbuf) prevent us from dimensioning anything by
c     plevr in this top level crm() routine.
      real o3mmr(plond,plev)    ! Ozone volume mixing ratio
      real o3vmr(plond,plev)    ! Ozone volume mixing ratio
      real pilnm1(plond,plevp)  ! natural log of pintm1
      real pintm1(plond,plevp)  ! model interface pressures
      real pmidm1(plond,plev)   ! model level pressures
      real pmlnm1(plond,plev)   ! natural log of pmidm1
      real ps(plond)            ! surface pressure
      real qm1(plond,plev)      ! model level specific humidity
      real sndpth(plond)        ! snow depth (liquid water equivalent)
      real tg(plond)            ! surface (skin) temperature
      real tm1(plond,plev)      ! model level temperatures
      real ts(plond)            ! surface air temperature

c     Fields computed from user input
      real effcld(plond,plevp)  ! effective cloud=cld*emis
      real emis(plond,plev)     ! cloud emissivity
      real fice(plond,plev)     ! fractional amount of ice
      real rei(plond,plev)      ! ice particle size
      real rel(plond,plev)      ! liquid effective drop size (microns)
      real coszrs(plond)        ! cosine solar zenith angle
      real eccf                 ! earth/sun distance factor 
      real loctim(plond)        ! local time of solar computation
      real srfrad(plond)        ! srf radiative heat flux
      real asdir(plond)         ! albedo: shortwave, direct
      real asdif(plond)         ! albedo: shortwave, diffuse
      real aldir(plond)         ! albedo: longwave, direct
      real aldif(plond)         ! albedo: longwave, diffuse

c     Output longwave arguments from radctl()
      real flwds(plond)         ! Surface down longwave flux
      real lwup(plond)          ! Surface up longwave flux from coupler
      real qrl(plond,plev)      ! Longwave cooling rate

c     Output shortwave arguments from radctl()
      real fsns(plond)          ! Surface absorbed solar flux
      real qrs(plond,plev)      ! Solar heating rate
      real soll(plond)          ! Downward solar rad onto surface (lw direct)
      real solld(plond)         ! Downward solar rad onto surface (lw diffuse)
      real sols(plond)          ! Downward solar rad onto surface (sw direct)
      real solsd(plond)         ! Downward solar rad onto surface (sw diffuse)

c     Additional CRM diagnostic output from radctl()
      real flns(plond)          ! srf longwave cooling (up-dwn) flux
      real flnsc(plond)         ! clr sky lw flx at srf (up-dwn)
      real flnt(plond)          ! net outgoing lw flx at model top
      real flntc(plond)         ! clr sky lw flx at model top
      real fsnsc(plond)         ! clr sky surface abs solar flux
      real fsnt(plond)          ! total column absorbed solar flux
      real fsntc(plond)         ! clr sky total column abs solar flux
      real solin(plond)         ! solar incident flux

c     Local workspace: These variables are not saved.
      real hbuf                 ! history buffer
      real pie                  ! for radiation output

      integer i                 ! longitude index
      integer k                 ! level index
      integer lat               ! latitude row index 

c     Fundamental constants needed by radini()
      real cpairx               ! heat capacity dry air at constant prs (J/kg/K)
      real epsilox              ! ratio mean mol weight h2o to dry air
      real gravx                ! gravitational acceleration (m/s**2)
      real stebolx              ! Sefan-Boltzmann constant (W/m**2/K**4)

#if ( defined SUN )
      integer ieee_handler
      integer myhandler
      integer rcd
      external myhandler
#endif
c     Externals
      external albland
      external albocean
      external blkdat
      external cldefr
      external cldems
      external getdat
      external radctl
      external radini
      external zenith
#ifdef NETCDF
      external netcdf
#endif

c     Main Code
#if ( defined SUN )
c     First set up ieee exception traps for debugging purposes. 
c     Note that setting these traps will cause an informational/
c     diagnostic IEEE message to appear at the end of the program unless
c     they are cleared before exiting. The message will describe which
c     traps are set at the end of the program.
      rcd=ieee_handler( 'set', 'common', myhandler )
c      rcd=ieee_handler( 'set', 'all', myhandler )
c      rcd=ieee_handler( 'set', 'division', myhandler )
c      rcd=ieee_handler( 'set', 'underflow', myhandler )
c      rcd=ieee_handler( 'set', 'overflow', myhandler )
c      rcd=ieee_handler( 'set', 'invalid', myhandler )
      if (rcd.ne.0) write (6,'(a)') 'IEEE trapping not supported here'
#endif

      write(6,'(a)') 'Begin CCM3 Column Radiation Model'

c     Set latitude index to 1
      lat=1

c     Set parameters in common block comtim.h: nstep,dosw,dolw,doabsems
      nstep  =  0
      dosw  =  .true.
      dolw  =  .true.
      doabsems = .true.

c     Set parameters in common block comctl.h: anncyc,dodiavg,iradsw,iradlw,iradae
      anncyc = .true.
      dodiavg = .false.
      iradsw = 1
      iradlw = 1
      iradae = 1

c     Set parameters required for call to radini():
      gravx   =   9.80616
      cpairx  =   1.00464e3
      epsilox =   0.622
      stebolx =   5.67e-8

c     Given these four constants in MKS, radini() will define their CGS
c     equivalents, as well as setting many radiation parameters stored
c     in common blocks. radini() must be called before getdat(), because
c     the co2 mixing ratio set (by the user) in getdat() should overwrite
c     the default CCM3 co2 mixing ratio set by radini().
      call radini(gravx,cpairx,epsilox,stebolx)

c     getdat() also sets calday (used in zenith() and radinp()).
      call getdat(
     $     clat,
     $     cld,
     $     clwp,
     $     coslat,
     $     ioro,
     $     loctim,
     $     o3mmr,
     $     o3vmr,
     $     pilnm1,
     $     pintm1,
     $     pmidm1,
     $     pmlnm1,
     $     ps,
     $     qm1, 
     $     sndpth,
     $     tg,
     $     tm1,
     $     ts)     

c     Get coszrs: needed as input to albland(), albocean(), radctl()
      call zenith (calday  ,dodiavg ,clat    ,coszrs  )

c     Find the albedo for land points
      call albland(lat     ,ioro    ,sndpth  ,coszrs  ,asdir   ,
     $     aldir   ,asdif   ,aldif   )

c     Find the albedo for ocean/sea-ice points
      call albocean(lat     ,ioro    ,sndpth  ,coszrs  ,asdir   ,
     $     aldir   ,asdif   ,aldif   )

c     Cloud particle size and fraction of ice
      call cldefr(ioro, tm1, rel, rei, fice, ps, pmidm1)

c     Cloud emissivity
      call cldems(clwp, fice, rei, emis)

c     Effective cloud cover
      do k=1,plev
         do i=1,plon
            effcld(i,k) = cld(i,k)*emis(i,k)
         end do
      end do

c     Cloud cover at surface interface always zero (for safety's sake)
      do i=1,plon
         effcld(i,plevp) = 0.
         cld(i,plevp)    = 0.
      end do

c     Main radiation driving routine. 
c     NB: All fluxes returned from radctl() have already been converted to MKS.
      call radctl(hbuf    ,clat    ,coslat  ,lat     ,ts      ,
     $     pmidm1  ,pintm1  ,pmlnm1  ,pilnm1  ,tm1     ,
     $     qm1     ,cld     ,effcld  ,clwp    ,coszrs  ,
     $     asdir   ,asdif   ,aldir   ,aldif   ,fsns    ,
     $     qrs     ,qrl     ,flwds   ,lwup    ,rel     ,
     $     rei     ,fice    ,sols    ,soll    ,solsd   ,
     $     solld   ,
c++csz
     $     fsnt,fsntc,fsnsc,flnt,flns,flntc,flnsc,solin, ! output 
     $     eccf, ! output
     $     o3vmr) ! input
c--csz

      do i=1,plon
         srfrad(i)=fsns(i)+flwds(i)
      end do

c     Write out final results
      write (6,'(a)') 'CCM3 CRM Results:'
      write (6,'(a)') 'Conventions:'
      write (6,*) 'Shortwave fluxes are positive downward'
      write (6,*) 'Longwave fluxes are positive upward'
      write (6,*) 'Net Radiative fluxes are positive downward (into the system)'
      write (6,*) 'Fluxes defined to be zero are not reported (e.g., LW down flx TOA)'
      write (6,'(a)') 'Abbreviations, Acronyms and Definitions:'
      write (6,*) 'LW   = Longwave'
      write (6,*) 'LWCF = Longwave Cloud Forcing'
      write (6,*) 'NCF  = Net Cloud Forcing = SWCF+LWCF'
      write (6,*) 'NIR  = Near Infrared (0.7 < lambda < 5.0 microns)'
      write (6,*) 'NRF  = Net Radiative Flux: sum of SW and LW fluxes'
      write (6,*) 'SW   = Shortwave'
      write (6,*) 'SWCF = Shortwave Cloud Forcing'
      write (6,*) 'TOA  = Top of Atmosphere'
      write (6,*) 'Vis  = Visible (0.2 < lambda < 0.7 microns)'
      write (6,*) 'atm  = Atmosphere'
      write (6,*) 'clr  = Clear sky (diagnostic computation with no clouds)'
      write (6,*) 'dff  = Diffuse flux'
      write (6,*) 'drc  = Direct flux'
      write (6,*) 'frc  = Fraction'
      write (6,*) 'net  = net flux = downwelling minus upwelling flux'
      write (6,*) 'sfc  = Surface level'

      write (6,*) ''
      write (6,'(a)') 'Sun-Earth Geometry:'
      pie  = 4.*atan(1.)
      do i=1,1
         write (6,*) 'Day of year (noon Jan 1 = 1.5) = ',calday
         write (6,*) 'Earth-sun distance             = ',eccf,' AU'
         write (6,*) 'Earth latitude                 = ',180.*clat/pie,' degrees'

         write (6,*) 'Cosine solar zenith angle      = ',coszrs(i)
         write (6,*) 'Local solar hour               = ',loctim(i)
         write (6,*) ''

         write (6,'(a)') 'Shortwave (SW) results ( < 5.0 microns):'
         if (solin(i).le.0.) then
            write (6,*) 'SW albedo               = Ill-defined'
            write (6,*) 'SW albedo (clr)         = Ill-defined'
         else
            write (6,*) 'SW albedo               = ',(solin(i)-fsnt(i))/solin(i)
            write (6,*) 'SW albedo (clr)         = ',(solin(i)-fsntc(i))/solin(i)
         endif
         write (6,*) 'SW down flux TOA        = ',solin(i),' W m-2' 
         write (6,*) 'SW up flux TOA          = ',solin(i)-fsnt(i),' W m-2' 
         write (6,*) 'SW up flux TOA (clr)    = ',solin(i)-fsntc(i),' W m-2' 
         write (6,*) 'SW net flux TOA         = ',fsnt(i),' W m-2' 
         write (6,*) 'SW net flux TOA (clr)   = ',fsntc(i),' W m-2' 
         write (6,*) 'SW flux abs atm         = ',fsnt(i)-fsns(i),' W m-2' 
         write (6,*) 'SW flux abs atm (clr)   = ',fsntc(i)-fsnsc(i),' W m-2' 
         write (6,*) 'SW down flux sfc        = ',sols(i)+soll(i)+solsd(i)+solld(i),' W m-2' 
c         write (6,*) 'SW down flux sfc (clr)  = ',,' W m-2' 
         write (6,*) 'SW up flux sfc          = ',sols(i)+soll(i)+solsd(i)+solld(i)-fsns(i),' W m-2' 
c         write (6,*) 'SW up flux sfc  (clr)   = ',,' W m-2' 
         write (6,*) 'SW net flux sfc         = ',fsns(i),' W m-2' 
         write (6,*) 'SW net flux sfc  (clr)  = ',fsnsc(i),' W m-2' 
         write (6,*) 'SW cloud forcing TOA    = ',fsnt(i)-fsntc(i),' W m-2' 
         write (6,*) 'SW cloud forcing sfc    = ',fsns(i)-fsnsc(i),' W m-2'
         if (fsnt(i)-fsntc(i).eq.0.) then
            write (6,*) 'SWCF(sfc)/SWCF(TOA)     = Ill-defined'
         else
            write (6,*) 'SWCF(sfc)/SWCF(TOA)     = ',(fsns(i)-fsnsc(i))/(fsnt(i)-fsntc(i))
         endif                  ! endif
         write (6,*) ''

         write (6,'(a)') 'Longwave (LW) results ( > 5.0 microns):'
         write (6,*) 'LW up flux TOA          = ',flnt(i),' W m-2' 
         write (6,*) 'LW up flux TOA (clr)    = ',flntc(i),' W m-2' 
         write (6,*) 'LW up flux sfc          = ',flns(i)+flwds(i),' W m-2' 
         write (6,*) 'LW down flux sfc        = ',flwds(i),' W m-2' 
c         write (6,*) 'LW down flux sfc (clr)  = ',flwds(i),' W m-2' 
         write (6,*) 'LW net flux sfc         = ',flns(i),' W m-2' 
         write (6,*) 'LW net flux sfc (clr)   = ',flnsc(i),' W m-2' 
         write (6,*) 'LW cloud forcing TOA    = ',flntc(i)-flnt(i),' W m-2' 
         write (6,*) 'LW cloud forcing sfc    = ',flnsc(i)-flns(i),' W m-2' 
         write (6,*) ''

         write (6,'(a)') 'Net Radiative Flux results (NRF=SW+LW):'
         write (6,*) 'NRF up flux TOA         = ',solin(i)-fsnt(i)+flnt(i),' W m-2' 
         write (6,*) 'NRF down flux TOA       = ',solin(i),' W m-2' 
c         write (6,*) 'NRF up flux TOA (clr)  = ',flntc(i),' W m-2' 
         write (6,*) 'NRF net flux TOA        = ',fsnt(i)-flnt(i),' W m-2' 
         write (6,*) 'NRF net flux TOA (clr)  = ',fsntc(i)-flntc(i),' W m-2' 
         write (6,*) 'NRF up flux sfc         = ',sols(i)+soll(i)+solsd(i)+solld(i)-fsns(i)+flns(i)+flwds(i),' W m-2' 
         write (6,*) 'NRF down flux sfc       = ',sols(i)+soll(i)+solsd(i)+solld(i)+flwds(i),' W m-2' 
c         write (6,*) 'NRF down flux sfc (clr)  = ',flwds(i),' W m-2' 
         write (6,*) 'NRF net flux sfc        = ',fsns(i)-flns(i),' W m-2' 
         write (6,*) 'NRF net flux sfc (clr)  = ',fsnsc(i)-flnsc(i),' W m-2' 
         write (6,*) 'NRF cloud forcing TOA   = ',fsnt(i)-fsntc(i)+flntc(i)-flnt(i),' W m-2' 
         write (6,*) 'NRF cloud forcing sfc   = ',fsns(i)-fsnsc(i)+flnsc(i)-flns(i),' W m-2' 
         write (6,*) ''

c     The following defines the SRB in terms of the "true" (unscaled) beam.
         write (6,'(a)') 'Solar surface radiation budget components:'
         write (6,*) 'SW down flux sfc        = ',flx_SW_dwn_sfc,' W m-2'
         write (6,*) 'SW down flux drc sfc    = ',flx_SW_dwn_drc_sfc,' W m-2'
         write (6,*) 'SW down flux dff sfc    = ',flx_SW_dwn_dff_sfc,' W m-2'
         if (sols(i)+soll(i).le.0.) then
            write (6,*) 'SW down flux dff/drc    = Ill-defined'
         else
            write (6,*) 'SW down flux dff/drc    = ',dff_drc_SW_sfc
         endif
         write (6,*) 'Vis down flux sfc       = ',flx_vsb_dwn_sfc,' W m-2'
         write (6,*) 'Vis down flux drc sfc   = ',flx_vsb_dwn_drc_sfc,' W m-2'
         write (6,*) 'Vis down flux dff sfc   = ',flx_vsb_dwn_dff_sfc,' W m-2'
         if (sols(i).le.0.) then
            write (6,*) 'Vis down flux dff/drc   = Ill-defined'
         else
            write (6,*) 'Vis down flux dff/drc   = ',dff_drc_vsb_sfc
         endif
         write (6,*) 'NIR down flux sfc       = ',flx_NIR_dwn_sfc,' W m-2'
         write (6,*) 'NIR down flux drc sfc   = ',flx_NIR_dwn_drc_sfc,' W m-2'
         write (6,*) 'NIR down flux dff sfc   = ',flx_NIR_dwn_dff_sfc,' W m-2'
         if (soll(i).le.0.) then
            write (6,*) 'NIR down flux dff/drc   = Ill-defined'
         else
            write (6,*) 'NIR down flux dff/drc   = ',dff_drc_NIR_sfc
         endif
         write (6,*) ''

c     The following defines the SRB in terms of the scaled beam.
c     These may be useful to some investigators so we leave them here.
c     Do not use these number unless you are sure you know what they mean.
c         write (6,*) 'SW down flux sfc        = ',sols(i)+soll(i)+solsd(i)+solld(i),' W m-2'
c         write (6,*) 'SW down flux drc sfc    = ',sols(i)+soll(i),' W m-2'
c         write (6,*) 'SW down flux dff sfc    = ',solsd(i)+solld(i),' W m-2'
c         if (sols(i)+soll(i).le.0.) then
c            write (6,*) 'SW down flux dff/drc    = Ill-defined'
c         else
c            write (6,*) 'SW down flux dff/drc    = ',(solsd(i)+solld(i))/(sols(i)+soll(i))
c         endif
c         write (6,*) 'Vis down flux sfc       = ',sols(i)+solsd(i),' W m-2'
c         write (6,*) 'Vis down flux drc sfc   = ',sols(i),' W m-2'
c         write (6,*) 'Vis down flux dff sfc   = ',solsd(i),' W m-2'
c         if (sols(i).le.0.) then
c            write (6,*) 'Vis down flux dff/drc   = Ill-defined'
c         else
c            write (6,*) 'Vis down flux dff/drc   = ',solsd(i)/sols(i)
c         endif
c         write (6,*) 'NIR down flux sfc       = ',soll(i)+solld(i),' W m-2'
c         write (6,*) 'NIR down flux drc sfc   = ',soll(i),' W m-2'
c         write (6,*) 'NIR down flux dff sfc   = ',solld(i),' W m-2'
c         if (soll(i).le.0.) then
c            write (6,*) 'NIR down flux dff/drc   = Ill-defined'
c         else
c            write (6,*) 'NIR down flux dff/drc   = ',solld(i)/soll(i)
c         endif

         write (6,'(a)') 'Heating rates:'
         write (6,*) 'Level  Pressure           SW           LW          Net'
         write (6,*) '    #        mb      K day-1      K day-1      K day-1'
         do k=1,plev
            write (6,'(i4,4x,f8.3,1x,3(f12.7,1x))') 
     $           k,pmidm1(i,k)/100.,qrs(i,k)*86400.,qrl(i,k)*86400.,
     $           86400.*(qrs(i,k)+qrl(i,k))
         enddo                  ! end loop over levels
         write (6,*) ''

         write (6,'(a)') 'Cloud microphysics:'
         write (6,*) 'Level  Pressure      r_e liq      r_e ice      Ice frc'
         write (6,*) '    #        mb       micron       micron          frc'
         do k=1,plev
            write (6,'(i4,4x,f8.3,1x,3(f12.7,1x))') 
     $           k,pmidm1(i,k)/100.,rel(i,k),rei(i,k),fice(i,k)
         enddo                  ! end loop over levels

      enddo                     ! end loop over longitude

#ifdef NETCDF
c     Write results to a netCDF file
      call netcdf(
     $     cld(1,1),
     $     clwp(1,1),
     $     coszrs(1),
     $     effcld(1,1),
     $     fice(1,1),
c     
     $     flns(1),
     $     flnsc(1),
     $     flnt(1),
     $     flntc(1),
     $     flwds(1),
c     
     $     fsns(1),
     $     fsnsc(1),
     $     fsnt(1),
     $     fsntc(1),
     $     ioro(1),
c
     $     loctim(1),
     $     lwup(1),
     $     pintm1(1,1),
     $     pmidm1(1,1),
     $     ps(1),
     $     o3mmr(1,1),
c
     $     o3vmr(1,1),
     $     qm1(1,1),
     $     qrl(1,1),
     $     qrs(1,1),
     $     rei(1,1),
c
     $     rel(1,1),
     $     sndpth(1),
     $     solin(1),
     $     srfrad(1),
     $     tg(1),
c
     $     tm1(1,1),
     $     ts(1))
#endif

c     Exiting the program without disabling the traps will cause an
c     IEEE note to appear in stderr even if no exceptions were detected
#if ( defined SUN )
      rcd=ieee_handler('clear','common',myhandler)
#endif

      write (6,'(a)') 'End CCM3 CRM'
      stop
      end

      subroutine getdat(
     $     clat,
     $     cld,
     $     clwp,
     $     coslat,
     $     ioro,
     $     loctim,
     $     o3mmr,
     $     o3vmr,
     $     pilnm1,
     $     pintm1,
     $     pmidm1,
     $     pmlnm1,
     $     ps,
     $     qm1, 
     $     sndpth,
     $     tg,
     $     tm1,
     $     ts)     
c Purpose: Interface routine for column model that initializes certain constants and reads external data 

c O3 mass mixing ratios are read in, but the model also requires the path lengths; they are computed here.
c The cloud longwave emissivity must be computed from the cloud input (fraction and liquid water path), this is done here.

#include <implicit.h>
c     Parameters
#include <prgrid.h>
c     Commons
#include <comtim.h>
#include <crdalb.h>
#include <crdcon.h>

c     Output arguments
      integer ioro(plond)       ! land surface flag

      real clat                 ! model latitude in radians
      real cld(plond,plevp)     ! cloud fraction
      real clwp(plond,plev)     ! cloud liquid water path (g/m**2)
      real coslat               ! cosine latitude
      real loctim(plond)        ! local time of solar computation
      real o3mmr(plond,plev)    ! o3 mass mixing ratio
      real o3vmr(plond,plev)    ! o3 mass mixing ratio
      real pilnm1(plond,plevp)  ! ln(pintm1)
      real pintm1(plond,plevp)  ! pressure at model interfaces 
      real pmidm1(plond,plev)   ! pressure at model mid-levels 
      real pmlnm1(plond,plev)   ! ln(pmidm1)
      real ps(plond)            ! model surface pressure field
      real qm1(plond,plev)      ! moisture field
      real sndpth(plond)        ! snow depth (liquid water equivalent)
      real tg(plond)            ! surface (skin) temperature
      real tm1(plond,plev)      ! atmospheric temperature
      real ts(plond)            ! surface (air)  temperature

c     Local workspace
      integer dbg_lvl           ! Debugging level

      real dayyr(plond)         ! day of year
      real rlat(plond)          ! latitude input

      real co2mix               ! co2 volume mixing ratio read in

      integer i                 ! longitude index
      integer k                 ! level  index
      integer lev(plev)         ! level input

      character*80 label

      real amd                  ! effective molecular weight of dry air (g/mol)
      real amo                  ! molecular weight of ozone (g/mol)
      real vmmr                 ! ozone volume mixing ratio
      data amd   /  28.9644   /
      data amo   /  48.0000   /

c     Main Code
c     Initialize some variables
      dbg_lvl=0

c     Read input data
      do 100 i=1,1

         read (5,101)  label
 101     format(a80)
         if (dbg_lvl.gt.0) write (6,*)   label

         read (5,101)  label
         if (dbg_lvl.gt.0) write (6,*)   label

         read (5,101)  label
         if (dbg_lvl.gt.0) write (6,*)   label

         read (5,*)    dayyr(i)
         if (dbg_lvl.gt.0) write (6,*) 'day of year (1..365)  = ',dayyr(i)

         read (5,*)    rlat(i)
         if (dbg_lvl.gt.0) write (6,*) 'latitude (-90 to +90) = ',rlat(i)

         read (5,101)  label
         if (dbg_lvl.gt.0) write (6,*)   label

         do 200 k=1,plev
            read (5,*) lev(k),pmidm1(i,k),tm1(i,k),qm1(i,k),o3mmr(i,k)
     +           ,cld(i,k),clwp(i,k)
            if (dbg_lvl.gt.0) write (6,99) k   ,pmidm1(i,k),tm1(i,k),qm1(i,k),o3mmr(i,k)
     +           ,cld(i,k),clwp(i,k)
 99         format(1x,i3,1x,6(1pe10.3,1x))
            if( cld(i,k) .gt. 0.99999 ) cld(i,k)=.99999
 200     continue

         read (5,*)     ps(i)
         if (dbg_lvl.gt.0) write (6,*) 'srf pressure = ',ps(i)

c     Convert pressures from mb to pascals and define interface pressures:
         ps(i)=ps(i)*100.
         do 125 k=1,plev

            pmidm1(i,k)=pmidm1(i,k) * 100.
            pmlnm1(i,k)=alog(pmidm1(i,k))

 125     continue
         do 150 k=1,plevp

            if( k .eq. 1 ) then
               pintm1(i,k)=pmidm1(i,k) / 2.0
            else if ( k .gt. 1 .and. k .le. plev ) then
               pintm1(i,k)=0.5 * (pmidm1(i,k-1) + pmidm1(i,k))
            else if ( k .eq. plevp ) then
               pintm1(i,k)=ps(i)
            endif
            pilnm1(i,k)=alog(pintm1(i,k))

 150     continue

         read (5,*)       ts(i)
         if (dbg_lvl.gt.0) write (6,*) 'surface air temperature  = ',ts(i)

         read (5,*)       tg(i)
         if (dbg_lvl.gt.0) write (6,*) 'surface skin temperature = ',tg(i)

         read (5,*)       ioro(i)
         if (dbg_lvl.gt.0) write (6,*) 'surface type (oro) flag  = ',ioro(i)

         read (5,*)       rghnss(i,1)
         if (dbg_lvl.gt.0) write (6,*) 'surface roughness        = ',rghnss(i,1)

         read (5,*)       sndpth(i)
         if (dbg_lvl.gt.0) write (6,*) 'snow depth (liq wat)     = ',sndpth(i)

         read (5,*)       albvss(i,1)
         if (dbg_lvl.gt.0) write (6,*) 'albvss     = ',albvss(i,1)

         read (5,*)       albvsw(i,1)
         if (dbg_lvl.gt.0) write (6,*) 'albvsw     = ',albvsw(i,1)

         read (5,*)       albnis(i,1)
         if (dbg_lvl.gt.0) write (6,*) 'albnis     = ',albnis(i,1)

         read (5,*)       albniw(i,1)
         if (dbg_lvl.gt.0) write (6,*) 'albniw     = ',albniw(i,1)

         read (5,*)       frctst(i,1)
         if (dbg_lvl.gt.0) write (6,*) 'frctst     = ',frctst(i,1)

         read (5,*)       co2mix
         if (dbg_lvl.gt.0) write (6,*) 'co2vmr     = ',co2mix

c     NB: co2vmr is also set in radini() so this user value should override 
c     that setting.
         co2vmr = co2mix

 100  continue

      calday=dayyr(1)
      loctim(1)=(calday-aint(calday))*24.
      pie   =4.*atan(1.)
      clat  =rlat(1)*(pie/180.)
      coslat=cos(clat)

c     Convert ozone mass mixing ratio to ozone volume mixing ratio:
      vmmr=amo/amd
      do k=1,plev
         do i=1,plon
c     o3mmr(i,k)=vmmr*o3vmr(i,k)
            o3vmr(i,k)=o3mmr(i,k)/vmmr
         end do
      end do

      if (dbg_lvl.gt.0) write (6,*) 'End of profile data input'

      return
      end
      subroutine writeric(nabem,absems,lngbuf,nrow)
c
c........ dummy routine for write of ab/em data
c
      return 
      end
      subroutine readric(nabem,absems,lngbuf,nrow)
c
c........ dummy routine for read of ab/em data
c
      return 
      end
      subroutine outfld(name,tmp ,plond,lat,hbuf)
c
c........ dummy routine for history tape write
c
      return 
      end
      subroutine radozn(lat     ,pmid    ,o3vmr   )
c
c     Do nothing routine called from radctl. o3vmr is coming from
c     getdat() instead.
c
      return
      end
c
      subroutine radctl(hbuf    ,clat    ,coslat  ,lat     ,ts      ,
     $                  pmid    ,pint    ,pmln    ,piln    ,t       ,
     $                  h2ommr  ,cld     ,effcld  ,clwp    ,coszrs  ,
     $                  albs    ,albsd   ,albl    ,albld   ,fsns    ,
     $                  qrs     ,qrl     ,flwds   ,lwup    ,rel     ,
     $                  rei     ,fice    ,sols    ,soll    ,solsd   ,
     $                  solld   ,
c++csz
     $     fsnt,fsntc,fsnsc,flnt,flns,flntc,flnsc,solin, ! output
     $     eccf, ! output
     $     o3vmr) ! input
c--csz
C-----------------------------------------------------------------------
C
C Driver for radiation computation.
C
C Radiation uses cgs units, so conversions must be done from
C model fields to radiation fields.
C
C Calling sequence:
C
C     radinp      Converts units of model fields and computes ozone
C                 mixing ratio for solar scheme
C
C     radcsw      Performs solar computation
C       radalb    Computes surface albedos
C       radded    Computes delta-Eddington solution
C       radclr    Computes diagnostic clear sky fluxes
C
C     radclw      Performs longwave computation
C
C       radtpl    Computes path quantities
C       radems    Computes emissivity
C       radabs    Computes absorptivity
C
C---------------------------Code history--------------------------------
C
C Original version:  CCM1
C Standardized:      J. Rosinski, June 1992
C Reviewed:          J. Kiehl, B. Briegleb, August 1992
C
C Modified:          B. Briegleb, March 1995 to add aerosol
C                    to shortwave code
C
C-----------------------------------------------------------------------
c
c $Id: crm.F,v 1.12 1998/02/19 21:58:51 zender Exp zender $
c $Author: zender $
c
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <pmgrid.h>
#include <ptrrgrid.h>
#include <pagrid.h>
C------------------------------Commons----------------------------------
#include <comctl.h>
C-----------------------------------------------------------------------
#include <comhst.h>
C-----------------------------------------------------------------------
#include <comtim.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real hbuf(*)              ! history tape buffer,for calls to outfld
      integer lat               ! Latitude row index
      real ts(plond),           ! Surface (skin) temperature
     $     pmid(plond,plev),    ! Model level pressures
     $     pint(plond,plevp),   ! Model interface pressures
     $     pmln(plond,plev),    ! Natural log of pmid
     $     rel(plond,plev),     ! liquid cloud particle effective radius
     $     rei(plond,plev),     ! ice effective drop size (microns)
     $     fice(plond,plev),    ! fractional ice content within cloud
     $     piln(plond,plevp),   ! Natural log of pint
     $     t(plond,plev),       ! Model level temperatures
     $     h2ommr(plond,plev),  ! Model level specific humidity
     $     cld(plond,plevp),    ! Fractional cloud cover
     $     effcld(plond,plevp), ! Effective fractional cloud cover
     $     clwp(plond,plev)     ! Cloud liquid water path
      real coszrs(plond),       ! Cosine solar zenith angle
     $     albs(plond),
     $     albsd(plond),
     $     albl(plond),
     $     albld(plond)
      real clat,                  ! current latitude(radians)
     $     coslat                 ! cosine latitude
C
C Output solar arguments
C
      real fsns(plond),         ! Surface absorbed solar flux
     $     sols(plond),         ! Downward solar rad onto surface (sw direct)
     $     soll(plond),         ! Downward solar rad onto surface (lw direct)
     $     solsd(plond),        ! Downward solar rad onto surface (sw diffuse)
     $     solld(plond),        ! Downward solar rad onto surface (lw diffuse)
     $     qrs(plond,plev)      ! Solar heating rate
C
C Output longwave arguments
C
      real qrl(plond,plev),     ! Longwave cooling rate
     $     flwds(plond),        ! Surface down longwave flux
     $     lwup(plond)          ! Surface up longwave flux from coupler
C
C---------------------------Local variables-----------------------------
C
      integer i                 ! index
      real solin(plond),        ! Solar incident flux
     $     fsnt(plond),         ! Net column abs solar flux at model top
     $     fsntc(plond),        ! Clear sky total column abs solar flux
     $     fsnsc(plond)         ! Clear sky surface abs solar flux
      real flnt(plond),         ! Net outgoing lw flux at model top
     $     flns(plond),         ! Srf longwave cooling (up-down) flux
     $     flntc(plond),        ! Clear sky lw flux at model top
     $     flnsc(plond)         ! Clear sky lw flux at srf (up-down)
      real pbr(plond,plevr),    ! Model mid-level pressures (dynes/cm2)
     $     pnm(plond,plevrp),   ! Model interface pressures (dynes/cm2)
     $     o3vmr(plond,plevr),  ! Ozone volume mixing ratio
     $     o3mmr(plond,plevr),  ! Ozone mass mixing ratio
     $     plco2(plond,plevrp), ! Prs weighted CO2 path
     $     plh2o(plond,plevrp), ! Prs weighted H2O path
     $     tclrsf(plond,plevrp),! Total clear sky fraction, level to space
     $     eccf                 ! Earth/sun distance factor
      real tmp(plond)           ! Temporary for outfld
      real n2o(plond,plev),        ! nitrous oxide mass mixing ratio
     $     ch4(plond,plev),        ! methane mass mixing ratio
     $     cfc11(plond,plev),      ! cfc11 mass mixing ratio
     $     cfc12(plond,plev)       ! cfc12 mass mixing ratio
      real aermmr(plond,plevr), ! level aerosol mass mixing ratio
     $     rh(plond,plevr)      ! level relative humidity (fraction)
C
C Declare local arrays to which model input arrays are interpolated here.
C Current default is none since radiation grid = model grid.
C
C Externals.
C
      external radinp,          ! Computes latitude dependent radiation input
     $         aermix,          ! Specifies aerosol mass mixing ratio
     $         radcsw,          ! Computes solar radiation
     $         radozn,          ! Computes ozone volume mixing ratio
     $         radclw,          ! Computes longwave radiation
     $         torgrid,         ! Interpolate model variables to radiation grid
     $         fmrgrid          ! Interpolate radiation variables to model grid
C--------------------------------------------------------------------------
C
C Interpolate model input arrays to radiation vertical grid.  Currently this 
C is a do-nothing routine because radiation grid = model grid.
C
      call torgrid(pmid    ,pint    ,pmln    ,piln    ,t       ,
     $             h2ommr  ,cld     ,effcld  ,clwp    ,
     $             pmid    ,pint    ,pmln    ,piln    ,t       ,
     $             h2ommr  ,cld     ,effcld  ,clwp    )
C
C Interpolate ozone volume mixing ratio to model levels
C
c++csz
c
c     This is a do nothing routine in the CRM.
c     Instead of interpolating the o3vmr from the time-interpolated
c     values, we pass compute o3vmr in getdat() and pass it directly
c     into radctl(). o3mmr will be computed in radinp().
c
      call radozn(lat     ,pmid    ,o3vmr   )
c--csz
C
C Set latitude dependent radiation input
C
      call radinp(pmid    ,pint    ,h2ommr  ,cld     ,o3vmr   ,
     $            pbr     ,pnm     ,plco2   ,plh2o   ,tclrsf  ,
     $            eccf    ,o3mmr   )
C
C Solar radiation computation
C
      if (dosw) then
C
C Specify aerosol mass mixing ratio
C
         call aermix(pnm     ,pbr     ,h2ommr  ,t       ,aermmr  ,
     $               rh      )
C
         call radcsw(pnm     ,h2ommr  ,cld     ,clwp    ,o3mmr   ,
     $               eccf    ,coszrs  ,albs    ,albsd   ,albl    ,
     $               albld   ,solin   ,qrs     ,fsns    ,fsnt    ,
     $               fsnsc   ,fsntc   ,rel     ,rei     ,fice    ,
     $               sols    ,soll    ,solsd   ,solld   ,aermmr  ,
     $               rh      )
C
C Convert units of shortwave fields needed by rest of model from CGS to MKS
C
         do i=1,plon
            solin(i) = solin(i)*1.e-3
            fsnt(i)  = fsnt(i) *1.e-3
            fsns(i)  = fsns(i) *1.e-3
            fsntc(i) = fsntc(i)*1.e-3
            fsnsc(i) = fsnsc(i)*1.e-3
         end do
C
C Calculate/outfld albedo and clear sky albedo
C
         if (ninavg(1).eq.'Q') then
            do i=1,plon
               if (solin(i).gt.0.) then
                  tmp(i) = (solin(i) - fsnt(i)) / solin(i)
               else
                  tmp(i) = 0.
               end if
            end do
            call outfld('ALB     ',tmp ,plond,lat,hbuf)
C
            do i=1,plon
               if (solin(i).gt.0.) then
                  tmp(i) = (solin(i) - fsntc(i)) / solin(i)
               else
                  tmp(i) = 0.
               end if
            end do
            call outfld('ALBCLR  ',tmp ,plond,lat,hbuf)
         end if
C
C Dump shortwave radiation information to history tape buffer (diagnostics)
C
         call outfld('SOLIN   ',solin ,plond,lat,hbuf)
         call outfld('FSNT    ',fsnt  ,plond,lat,hbuf)
         call outfld('FSNS    ',fsns  ,plond,lat,hbuf)
         call outfld('FSNTC   ',fsntc ,plond,lat,hbuf)
         call outfld('FSNSC   ',fsnsc ,plond,lat,hbuf)
      end if
C
C Longwave radiation computation
C
      if (dolw) then
c
c Specify trace gas mixing ratios    
c
         call trcmix(pmid, clat, coslat, n2o, ch4, cfc11, cfc12)
c
         call radclw(lat     ,ts      ,t       ,h2ommr  ,o3vmr   ,
     $               pbr     ,pnm     ,pmln    ,piln    ,plco2   ,
     $               plh2o   ,n2o     ,ch4     ,cfc11   ,cfc12   ,
     $               effcld  ,tclrsf  ,qrl     ,flns    ,flnt    ,
     $               flnsc   ,flntc   ,flwds   ,lwup    )
C
C Convert units of longwave fields needed by rest of model from CGS to MKS
C
         do i=1,plon
            flnt(i)  = flnt(i)*1.e-3
            flns(i)  = flns(i)*1.e-3
            flntc(i) = flntc(i)*1.e-3
            flnsc(i) = flnsc(i)*1.e-3
            flwds(i) = flwds(i)*1.e-3
         end do
C
C Dump longwave radiation information to history tape buffer (diagnostics)
C
         call outfld('FLNT    ',flnt  ,plond,lat,hbuf)
         call outfld('FLNTC   ',flntc ,plond,lat,hbuf)
         call outfld('FLNS    ',flns  ,plond,lat,hbuf)
         call outfld('FLNSC   ',flnsc ,plond,lat,hbuf)
      end if
C
C Interpolate radiation output arrays to model vertical grid.  Currently this 
C is a do-nothing routine because radiation grid = model grid.
C
      call fmrgrid(qrs     ,qrl     ,
     $             qrs     ,qrl     )
C
      return
#ifdef LOGENTRY 
c Revision 1.4  1995/03/17  18:54:10  ccm2
c add globally uniform background aerosol.
c
c Revision 1.2  1995/02/17  21:28:36  jhack
c Next rev of omega physics, ice, clouds, liquid water, convection, etc.
c
c Revision 1.2  1994/09/16  21:08:43  rosinski
c This is the first part of plx23.
c 
#endif
      end

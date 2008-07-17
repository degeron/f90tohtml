#include <params.h>
      subroutine radcsw(pint    ,h2ommr  ,cld     ,clwp    ,o3mmr   ,
     $                  eccf    ,coszrs  ,albs    ,albsd   ,albl    ,
     $                  albld   ,solin   ,qrs     ,fsns    ,fsnt    ,
     $                  fsnsc   ,fsntc   ,rel     ,rei     ,fice    ,
     $                  sols    ,soll    ,solsd   ,solld   ,aermmr  ,
     $                  rh      )
C-----------------------------------------------------------------------
C
C Solar radiation code
C
C Computes incident solar flux, solar heating rate, surface absorbed
C solar flux, and total column absorbed solar flux
C
C Uses the delta-eddington method
C
C Divides solar spectrum into 18 intervals from 0.2-5.0 micro-meters.
C solar flux fractions specified for each interval. allows for
C seasonally and diurnally varying solar input.  Includes molecular,
C cloud, and surface scattering, along with h2o,o3,co2,o2,cloud, and
C surface absorption. Computes delta-eddington reflections and
C transmissions assuming homogeneously mixed layers. Adds the layers 
C assuming scattering between layers to be isotropic, and distinguishes 
C direct solar beam from scattered radiation.
C
C Longitude loops are broken into 1 or 2 sections, so that only daylight
C (i.e. coszrs > 0) computations are done.
C
C Note that an extra layer above the model top layer is added.
C
C cgs units are used.
C
C Special diagnostic calc of the clear sky surface and total column
C absorbed flux is also done; this calculation does not effect the rest
C of the model, but is included for cloud forcing diagnostics.
C
C For more details , see Briegleb, Bruce P., 1992: Delta-Eddington
C Approximation for Solar Radiation in the NCAR Community Climate Model,
C Journal of Geophysical Research, Vol 97, D7, pp7603-7612).
C
C---------------------------Code history--------------------------------
C
C Modified March 1995 to add aerosols
C Original version:  B. Briegleb
C Standardized:      J. Rosinski, June 1992
C Reviewed:          J. Kiehl, B. Briegleb, August 1992
C
C-----------------------------------------------------------------------
c
c $Id: radcsw.F,v 1.1 1998/02/19 23:09:51 zender Exp zender $
c $Author: zender $
c
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <prgrid.h>
C-----------------------------------------------------------------------
      real scon                ! Solar constant
      parameter (scon = 1.367e6)
C------------------------------Commons----------------------------------
#include <crdcon.h>
      real rel(plond,plev),    ! liquid effective drop size (microns)
     $     rei(plond,plev),    ! ice effective drop size (microns)
     $     fice(plond,plev)    ! fractional ice content within cloud
C------------------------------Arguments--------------------------------
c++csz
#ifdef CRM_SRB
#include <crmsrb.h>
c     Local variables for SRB computations
      real flx_spc_dwn_drc_sfc(plond) ! Flux in single spectral band downwelling direct beam surface
      real xpn_arg              ! Argument to exponential
#endif 
c--csz

C
C Input arguments
C
      real pint(plond,plevp),  ! Interface pressure
     $     h2ommr(plond,plev), ! Specific humidity (h2o mass mix ratio)
     $     cld(plond,plevp),   ! Fractional cloud cover
     $     clwp(plond,plev),   ! Layer liquid water path
     $     o3mmr(plond,plev),  ! Ozone mass mixing ratio
     $     eccf,               ! Eccentricity factor(1./earth-sun dist. squared)
     $     coszrs(plond)       ! Cosine solar zenith angle
      real albs(plond),        ! 0.2-0.7 micro-meter srfc alb to direct rad
     $     albl(plond),        ! 0.7-5.0 micro-meter srfc alb to direct rad
     $     albsd(plond),       ! 0.2-0.7 micro-meter srfc alb to diffuse rad
     $     albld(plond)        ! 0.7-5.0 micro-meter srfc alb to diffuse rad
      real aermmr(plond,plev), ! level aerosol mass mixing ratio
     $     rh(plond,plev)      ! level relative humidity (fraction)
C
C
C Output arguments
C
      real solin(plond),       ! Incident solar flux
     $     qrs(plond,plev),    ! Solar heating rate
     $     fsns(plond),        ! Surface absorbed solar flux
     $     fsnt(plond),        ! Total column absorbed solar flux
     $     fsnsc(plond),       ! Clear sky surface absorbed solar flux
     $     fsntc(plond),       ! Clear sky total column absorbed solar flx
     $     sols(plond),        ! direct solar rad incident on surface (< 0.7)
     $     soll(plond),        ! direct solar rad incident on surface (>= 0.7)
     $     solsd(plond),       ! diffuse solar rad incident on surface (< 0.7)
     $     solld(plond)        ! diffuse solar rad incident on surface (>= 0.7)

C
C------------------------------Externals--------------------------------
C
      integer   isrchfgt,      ! Search for first array element > 0
     $          isrchfle       ! Search for first array element < 0
      external    radded,      ! Computes delta-eddington solution
     $            radclr,      ! Computes clear sky delta-edd solution
     $          isrchfgt,      ! Search for first array element > 0
     $          isrchfle       ! Search for first array element < 0
C
C---------------------------Local variables-----------------------------
C
      integer       ns,        ! Spectral loop index
     $               i,        ! Longitude loop index
     $               k,        ! Level loop index
     $               n,        ! Loop index for daylight
     $           nloop,        ! Number of daylight loops
     $           is(2),        ! Daytime start indices
     $           ie(2),        ! Daytime end indices
     $          indxsl         ! Index for cloud particle properties
C
C A. Slingo's data for cloud particle radiative properties (from 'A GCM
C Parameterization for the Shortwave Properties of Water Clouds' JAS
C vol. 46 may 1989 pp 1419-1427)
C
      real abarl(4), ! A coefficient for extinction optical depth
     $     bbarl(4), ! B coefficiant for extinction optical depth
     $     cbarl(4), ! C coefficiant for single particle scat albedo
     $     dbarl(4), ! D coefficiant for single particle scat albedo
     $     ebarl(4), ! E coefficiant for asymmetry parameter
     $     fbarl(4)  ! F coefficiant for asymmetry parameter
      save abarl, bbarl, cbarl, dbarl, ebarl, fbarl
C
      data abarl/ 2.817e-02, 2.682e-02,2.264e-02,1.281e-02/
      data bbarl/ 1.305    , 1.346    ,1.454    ,1.641    /
      data cbarl/-5.62e-08 ,-6.94e-06 ,4.64e-04 ,0.201    /
      data dbarl/ 1.63e-07 , 2.35e-05 ,1.24e-03 ,7.56e-03 /
      data ebarl/ 0.829    , 0.794    ,0.754    ,0.826    /
      data fbarl/ 2.482e-03, 4.226e-03,6.560e-03,4.353e-03/
C
      real abarli,   ! A coefficiant for current spectral interval
     $     bbarli,   ! B coefficiant for current spectral interval
     $     cbarli,   ! C coefficiant for current spectral interval
     $     dbarli,   ! D coefficiant for current spectral interval
     $     ebarli,   ! E coefficiant for current spectral interval
     $     fbarli    ! F coefficiant for current spectral interval
C
C Caution... A. Slingo recommends no less than 4.0 micro-meters nor
C greater than 20 micro-meters
C
C
c    ice water coefficients (Ebert and Curry,1992, JGR, 97, 3831-3836)
c
      real abari(4), ! a coefficient for extinction optical depth
     $     bbari(4), ! b coefficiant for extinction optical depth
     $     cbari(4), ! c coefficiant for single particle scat albedo
     $     dbari(4), ! d coefficiant for single particle scat albedo
     $     ebari(4), ! e coefficiant for asymmetry parameter
     $     fbari(4)  ! f coefficiant for asymmetry parameter
      save abari, bbari, cbari, dbari, ebari, fbari
c
      data abari/ 3.448e-03, 3.448e-03,3.448e-03,3.448e-03/
      data bbari/ 2.431    , 2.431    ,2.431    ,2.431    /
      data cbari/ 1.00e-05 , 1.10e-04 ,1.861e-02,.46658   /
      data dbari/ 0.0      , 1.405e-05,8.328e-04,2.05e-05 /
      data ebari/ 0.7661   , 0.7730   ,0.794    ,0.9595   /
      data fbari/ 5.851e-04, 5.665e-04,7.267e-04,1.076e-04/
C
      real abarii,   ! A coefficiant for current spectral interval
     $     bbarii,   ! B coefficiant for current spectral interval
     $     cbarii,   ! C coefficiant for current spectral interval
     $     dbarii,   ! D coefficiant for current spectral interval
     $     ebarii,   ! E coefficiant for current spectral interval
     $     fbarii    ! F coefficiant for current spectral interval
C
C      real cldefr ! Universal cloud effective radius in micro-meters
C      data cldefr / 10.0 /
C
      real delta           ! Pressure (atmospheres) for stratospheric h2o limit
      data delta  /  1.70e-3 /
C
      real o2mmr           ! O2 mass mixing ratio:
      save delta, o2mmr
      data o2mmr / .23143 /
C
C CO2 info:
C
      real mmwair,         ! Mean molecular weight of air
     $     mmwco2,         ! Mean molecular weight of co2
     $     co2mmr          ! Co2 mass mixing ratio
      save mmwair, mmwco2
      data mmwair / 28.9644 /
      data mmwco2 / 44.0000 /
C
      real albdir(plond),  ! Current spc intrvl srf alb to direct rad
     $     albdif(plond)   ! Current spc intrvl srf alb to diffuse rad
C
      integer nspint  ! Num of spectral intervals across solar spectrum
      parameter ( nspint = 18 )
C
C Next series depends on spectral interval
C
      real frcsol(nspint),  ! Fraction of solar flux in each spectral interval
     $     wavmin(nspint),  ! Min wavelength (micro-meters) of interval
     $     wavmax(nspint),  ! Max wavelength (micro-meters) of interval
     $     raytau(nspint),  ! Rayleigh scattering optical depth
     $     abh2o(nspint),   ! Absorption coefficiant for h2o (cm2/g)
     $     abo3 (nspint),   ! Absorption coefficiant for o3  (cm2/g)
     $     abco2(nspint),   ! Absorption coefficiant for co2 (cm2/g)
     $     abo2 (nspint),   ! Absorption coefficiant for o2  (cm2/g)
     $     ph2o(nspint),    ! Weight of h2o in spectral interval
     $     pco2(nspint),    ! Weight of co2 in spectral interval
     $     po2 (nspint)     ! Weight of o2  in spectral interval
      save frcsol ,wavmin ,wavmax ,raytau ,abh2o ,abo3 ,
     $     abco2  ,abo2   ,ph2o   ,pco2  ,po2
C
      data frcsol / .001488, .001389, .001290, .001686, .002877,
     $              .003869, .026336, .426131, .526861, .526861,
     $              .526861, .526861, .526861, .526861, .526861,
     $              .006239, .001834, .001834/
C
      data wavmin / .200,  .245,  .265,  .275,  .285,
     $              .295,  .305,  .350,  .700,  .701,
     $              .701,  .701,  .701,  .702,  .702,
     $             2.630, 4.160, 4.160/
      data wavmax / .245,  .265,  .275,  .285,  .295,
     $              .305,  .350,  .700, 5.000, 5.000,
     $             5.000, 5.000, 5.000, 5.000, 5.000,
     $             2.860, 4.550, 4.550/
C
      data raytau / 4.020, 2.180, 1.700, 1.450, 1.250,
     $              1.085, 0.730, 0.135, 0.020, .0001,
     $              .0001, .0001, .0001, .0001, .0001,
     $              .0001, .0001, .0001/
C
C Absorption coefficiants
C
      data abh2o /    .000,     .000,    .000,    .000,    .000,
     $                .000,     .000,    .000,    .002,    .035,
     $                .377,    1.950,   9.400,  44.600, 190.000,
     $                .000,     .000,    .000/
C
      data abo3  /
     $ 5.370e+04, 13.080e+04,  9.292e+04, 4.530e+04, 1.616e+04,
     $ 4.441e+03,  1.775e+02,  2.101e+01,      .000,      .000,
     $  .000    ,   .000    ,   .000    ,      .000,      .000,
     $  .000    ,   .000    ,   .000    /
C
      data abco2  /    .000,     .000,    .000,    .000,    .000,
     $                 .000,     .000,    .000,    .000,    .000,
     $                 .000,     .000,    .000,    .000,    .000,
     $                 .094,     .196,   1.963/
C
      data abo2  /    .000,     .000,    .000,    .000,    .000,
     $                .000,     .000,1.11e-05,6.69e-05,    .000,
     $                .000,     .000,    .000,    .000,    .000,
     $                .000,    .000,    .000/
C
C Spectral interval weights
C
      data ph2o  /    .000,     .000,    .000,    .000,    .000,
     $                .000,     .000,    .000,    .505,    .210,
     $                .120,     .070,    .048,    .029,    .018,
     $                .000,     .000,    .000/
      data pco2  /    .000,     .000,    .000,    .000,    .000,
     $                .000,     .000,    .000,    .000,    .000,
     $                .000,     .000,    .000,    .000,    .000,
     $               1.000,     .640,    .360/
      data po2   /    .000,     .000,    .000,    .000,    .000,
     $                .000,     .000,   1.000,   1.000,    .000,
     $                .000,     .000,    .000,    .000,    .000,
     $                .000,     .000,    .000/
C
C Diagnostic and accumulation arrays; note that sfltot, fswup, and
C fswdn are not used in the computation,but are retained for future use.
C
      real solflx(plond),         ! Solar flux in current interval
     $     sfltot(plond),         ! Spectrally summed total solar flux
     $     totfld(plond,0:plev),  ! Spectrally summed flux divergence
     $     fswup(plond,0:plevp),  ! Spectrally summed up flux
     $     fswdn(plond,0:plevp)   ! Spectrally summed down flux
C
C Cloud radiative property arrays
C
      real tauxcl(plond,0:plev),  ! water cloud extinction optical depth
     $     tauxci(plond,0:plev),  ! ice cloud extinction optical depth
     $         wcl(plond,0:plev), ! liquid cloud single scattering albedo
     $         gcl(plond,0:plev), ! liquid cloud asymmetry parameter
     $         fcl(plond,0:plev), ! liquid cloud forward scattered fraction
     $         wci(plond,0:plev), ! ice cloud single scattering albedo
     $         gci(plond,0:plev), ! ice cloud asymmetry parameter
     $         fci(plond,0:plev)  ! ice cloud forward scattered fraction
C
C Aerosol radiative property arrays
C
      real tauxar(plond,0:plev),     ! aerosol extinction optical depth
     $         wa(plond,0:plev),     ! aerosol single scattering albedo
     $         ga(plond,0:plev),     ! aerosol assymetry parameter
     $         fa(plond,0:plev)      ! aerosol forward scattered fraction
C
      real tauaer(plond),            ! total column aerosol extinction
     $     waer(plond),              ! aerosol single scattering albedo
     $     gaer(plond),              ! aerosol asymmetry parameter
     $     faer(plond)               ! aerosol forward scattering fraction
C
C Sulphate aerosol properties from August 1992
C
      real ksa(nspint),   ! aerosol spectral mass absorption coeff (m2/g)
     $     wsa(nspint),   ! aerosol spectral single scattering albedo
     $     gsa(nspint)    ! aerosol spectral asymmetry parameter
C
      data ksa /11.1163, 10.5472, 10.2468, 10.0392,  9.8292,
     $           9.6199,  9.0407,  5.3012,  1.9169,  0.3780,
     $           0.3780,  0.3780,  0.3780,  0.5704,  0.5704,
     $           0.5704,  0.5704,  0.5704 /
C
      data wsa / .999999, .999999, .999999, .999999, .999999,
     $           .999999, .999999, .999999, .999991, .989772,
     $           .989772, .989772, .989772, .847061, .847061,
     $           .847061, .847061, .847061 /
C
      data gsa / .719161, .719012, .718453, .717820, .716997,
     $           .715974, .712743, .694889, .618115, .485286,
     $           .485286, .485286, .485286, .295557, .295557,
     $           .295557, .295557, .295557 /
C
C Other variables and arrays needed for aerosol:
C
      real rhfac,              ! multiplication factor for kaer
     $     rhpc,               ! level relative humidity in %
     $     a0,                 ! constant in rh mult factor
     $     a1,                 ! constant in rh mult factor
     $     a2,                 ! constant in rh mult factor
     $     a3                  ! constant in rh mult factor
c
      data a0 / -9.2906106183    /
      data a1 /  0.52570211505   /
      data a2 / -0.0089285760691 /
      data a3 /  5.0877212432e-05/
C
C Various arrays and other constants:
C
      real pflx(plond,0:plevp),   ! Interface press, including extra layer
     $     zenfac(plond),         ! Square root of cos solar zenith angle
     $     sqrco2,                ! Square root of the co2 mass mixg ratio
     $     tmp1,                  ! Temporary constant array
     $     tmp2,                  ! Temporary constant array
     $     pdel,                  ! Pressure difference across layer
     $     path,                  ! Mass path of layer
     $     ptop,                  ! Lower interface pressure of extra layer
     $     ptho2,                 ! Used to compute mass path of o2
     $     ptho3,                 ! Used to compute mass path of o3
     $     pthco2,                ! Used to compute mass path of co2
     $     pthh2o,                ! Used to compute mass path of h2o
     $     h2ostr,                ! Inverse square root h2o mass mixing ratio
     $     wavmid,                ! Spectral interval middle wavelength
     $     trayoslp               ! Rayleigh optical depth/standard pressure
      real tmp1l,                 ! Temporary constant array
     $     tmp2l,                 ! Temporary constant array
     $     tmp3l,                 ! Temporary constant array
     $     tmp1i,                 ! Temporary constant array
     $     tmp2i,                 ! Temporary constant array
     $     tmp3i                  ! Temporary constant array
      real rdenom,                ! Multiple scattering term
     $     psf,                   ! Frac of solar flux in spect interval
     $     gocp                   ! Gravity/cp
C
C Layer absorber amounts; note that 0 refers to the extra layer added
C above the top model layer
C
      real uh2o(plond,0:plev),    ! Layer absorber amount of h2o
     $      uo3(plond,0:plev),    ! Layer absorber amount of  o3
     $     uco2(plond,0:plev),    ! Layer absorber amount of co2
     $      uo2(plond,0:plev),    ! Layer absorber amount of  o2
     $      uaer(plond,0:plev)    ! Layer aerosol amount 
C
C Total column absorber amounts:
C
      real uth2o(plond),          ! Total column  absorber amount of  h2o
     $     uto3(plond),           ! Total column  absorber amount of  o3
     $     utco2(plond),          ! Total column  absorber amount of  co2
     $     uto2(plond),           ! Total column  absorber amount of  o2
     $     utaer(plond)           ! Total column  aerosol
C
C These arrays are defined for plev model layers; 0 refers to the extra
C layer on top:
C
      real rdir(plond,0:plev),    ! Layer reflectivity to direct rad
     $     rdif(plond,0:plev),    ! Layer reflectivity to diffuse rad
     $     tdir(plond,0:plev),    ! Layer transmission to direct rad
     $     tdif(plond,0:plev),    ! Layer transmission to diffuse rad
     $     explay(plond,0:plev),  ! Solar beam exp transmission for layer
     $     flxdiv(plond,0:plev)   ! Flux divergence for layer
C
C These arrays are defined at model interfaces; 0 is the top of the
C extra layer above the model top; plevp is the earth surface:
C
      real rupdir(plond,0:plevp), ! Ref to dir rad for layers below
     $     rupdif(plond,0:plevp), ! Ref to dif rad for layers below
     $     rdndif(plond,0:plevp), ! Ref to dif rad for layers above
     $     exptdn(plond,0:plevp), ! Solar beam exp down transm from top
     $     tottrn(plond,0:plevp), ! Total transmission for layers above
     $     fluxup(plond,0:plevp), ! Up   flux at model interface
     $     fluxdn(plond,0:plevp)  ! Down flux at model interface
C
C-----------------------------------------------------------------------
C
C Initialize output fields:
C
      do i=1, plon
         fsnt(i)  = 0.0
         fsns(i)  = 0.0
         solin(i) = 0.0
         fsnsc(i) = 0.0
         fsntc(i) = 0.0
         sols(i) = 0.0
         soll(i) = 0.0
         solsd(i) = 0.0
         solld(i) = 0.0
      end do
      do k=1, plev
         do i=1, plon
            qrs(i,k) = 0.0
         end do
      end do
C
C Compute starting, ending daytime loop indices:
C
      nloop = 0
      is(1) = isrchfgt(plon,coszrs,1,0.0)
C
C If night everywhere, return:
C
      if(is(1).gt.plon) return
      ie(1) = isrchfle(plon-is(1),coszrs(is(1)+1),1,0.0) + is(1) - 1
      nloop = 1
C
C Possibly 2 daytime loops needed:
C
      if(ie(1).ne.plon) then
         is(2) = isrchfgt(plon-ie(1),coszrs(ie(1)+1),1,0.0) + ie(1)
         if(is(2).le.plon) then
            nloop = 2
            ie(2) = plon
         end if
      end if
C
C Define solar incident radiation and interface pressures:
C
      do n=1,nloop
         do i=is(n),ie(n)
            solin(i) = scon*eccf*coszrs(i)
            pflx(i,0) = 0.
         end do
      end do
      do k=1,plevp
         do n=1,nloop
            do i=is(n),ie(n)
               pflx(i,k) = pint(i,k)
            end do
         end do
      end do
C
C Compute optical paths:
C
      tmp1   = 0.5/(gravit*sslp)
      co2mmr = co2vmr*(mmwco2/mmwair)
      sqrco2 = sqrt(co2mmr)
      do n=1,nloop
         do i=is(n),ie(n)
            ptop      = pflx(i,1)
            ptho2     = o2mmr * ptop / gravit
            ptho3     = o3mmr(i,1) * ptop / gravit
            pthco2    = sqrco2 * (ptop / gravit)
            h2ostr    = sqrt( 1. / h2ommr(i,1) )
            zenfac(i) = sqrt(coszrs(i))
            pthh2o    = ptop**2*tmp1 +
     $                (ptop*rga)*(h2ostr*zenfac(i)*delta)
            uh2o(i,0) = h2ommr(i,1)*pthh2o
            uco2(i,0) = zenfac(i)*pthco2
            uo2 (i,0) = zenfac(i)*ptho2
            uo3 (i,0) = ptho3
            uaer(i,0) = 0.0
         end do
      end do
C
      tmp2 = delta/gravit
      do k=1,plev
         do n=1,nloop
            do i=is(n),ie(n)
               pdel   = pflx(i,k+1) - pflx(i,k)
               path   = pdel / gravit
               ptho2  = o2mmr * path
               ptho3  = o3mmr(i,k) * path
               pthco2 = sqrco2 * path
               h2ostr = sqrt(1.0/h2ommr(i,k))
               pthh2o = (pflx(i,k+1)**2 - pflx(i,k)**2)*tmp1 +
     $                  pdel*h2ostr*zenfac(i)*tmp2
               uh2o(i,k) = h2ommr(i,k)*pthh2o
               uco2(i,k) = zenfac(i)*pthco2
               uo2 (i,k) = zenfac(i)*ptho2
               uo3 (i,k) = ptho3
C
C Adjust aerosol amount by relative humidity factor:
C
               if( rh(i,k) .gt. .90 ) then
                 rhfac = 2.8
               else if (rh(i,k) .lt. .60 ) then
                 rhfac = 1.0
               else
                 rhpc  = 100. * rh(i,k)
                 rhfac = (a0 + a1*rhpc + a2*rhpc**2 + a3*rhpc**3)
               endif
               uaer(i,k) = aermmr(i,k)*rhfac*path
C
            end do
         end do
      end do
C
C Compute column absorber amounts for the clear sky computation:
C
      do n=1,nloop
         do i=is(n),ie(n)
            uth2o(i) = 0.0
            uto3(i)  = 0.0
            utco2(i) = 0.0
            uto2(i)  = 0.0
            utaer(i) = 0.0
         end do
      end do
      do k=1,plev
         do n=1,nloop
            do i=is(n),ie(n)
               uth2o(i) = uth2o(i) + uh2o(i,k)
               uto3(i)  = uto3(i)  + uo3(i,k)
               utco2(i) = utco2(i) + uco2(i,k)
               uto2(i)  = uto2(i)  + uo2(i,k)
               utaer(i) = utaer(i) + uaer(i,k)
            end do
         end do
      end do
C
C Initialize spectrally integrated totals:
C
      do k=0,plev
         do i=1,plon
            totfld(i,k) = 0.0
            fswup (i,k) = 0.0
            fswdn (i,k) = 0.0
         end do
      end do
      do i=1,plon
         sfltot(i)       = 0.0
         fswup (i,plevp) = 0.0
         fswdn (i,plevp) = 0.0
      end do
C
C Set cloud properties for top (0) layer; so long as tauxcl is zero,
C there is no cloud above top of model; the other cloud properties
C are arbitrary:
C
      do n=1,nloop
         do i=is(n),ie(n)
            tauxcl(i,0) = 0.
            wcl(i,0)     = 0.999999
            gcl(i,0)     = 0.85
            fcl(i,0)     = 0.725
C
            tauxci(i,0) = 0.
            wci(i,0)     = 0.999999
            gci(i,0)     = 0.85
            fci(i,0)     = 0.725
C
C Aerosol 
C
            tauxar(i,0) = 0.
            wa(i,0)      = 0.925
            ga(i,0)      = 0.850
            fa(i,0)      = 0.7225
C
         end do
      end do
C
C Begin spectral loop
C
      do 100 ns=1,nspint
C
C Set index for cloud particle properties based on the wavelength,
C according to A. Slingo (1989) equations 1-3:
C Use index 1 (0.25 to 0.69 micrometers) for visible
C Use index 2 (0.69 - 1.19 micrometers) for near-infrared
C Use index 3 (1.19 to 2.38 micrometers) for near-infrared
C Use index 4 (2.38 to 4.00 micrometers) for near-infrared
C
C Note that the minimum wavelength is encoded (with .001, .002, .003)
C in order to specify the index appropriate for the near-infrared
C cloud absorption properties
C
        if(wavmax(ns) .le. 0.7) then
           indxsl = 1
        else if(wavmin(ns) .eq. 0.700) then
           indxsl = 2
        else if(wavmin(ns) .eq. 0.701) then
           indxsl = 3
        else if(wavmin(ns) .eq. 0.702 .or. wavmin(ns) .gt. 2.38) then
           indxsl = 4
        end if
C
C Set cloud extinction optical depth, single scatter albedo,
C asymmetry parameter, and forward scattered fraction:
C
        abarli = abarl(indxsl)
        bbarli = bbarl(indxsl)
        cbarli = cbarl(indxsl)
        dbarli = dbarl(indxsl)
        ebarli = ebarl(indxsl)
        fbarli = fbarl(indxsl)
c
        abarii = abari(indxsl)
        bbarii = bbari(indxsl)
        cbarii = cbari(indxsl)
        dbarii = dbari(indxsl)
        ebarii = ebari(indxsl)
        fbarii = fbari(indxsl)
        do k=1,plev

           do n=1,nloop
              do i=is(n),ie(n)
c   liquid
                 tmp1l = abarli + bbarli/rel(i,k)
                 tmp2l = 1. - cbarli - dbarli*rel(i,k)
                 tmp3l = fbarli*rel(i,k)
c   ice
                 tmp1i = abarii + bbarii/rei(i,k)
                 tmp2i = 1. - cbarii - dbarii*rei(i,k)
                 tmp3i = fbarii*rei(i,k)
C
C Cloud fraction incorporated into cloud extinction optical depth
C
                 tauxcl(i,k) = clwp(i,k)*tmp1l*(1.-fice(i,k))
     $                         *cld(i,k)*sqrt(cld(i,k))
                 tauxci(i,k) = clwp(i,k)*tmp1i*fice(i,k)
     $                         *cld(i,k)*sqrt(cld(i,k))
C
C Do not let single scatter albedo be 1; delta-eddington solution
C for non-conservative case:
C
                 wcl(i,k) = amin1(tmp2l,.999999)
                 gcl(i,k) = ebarli + tmp3l
                 fcl(i,k) = gcl(i,k)*gcl(i,k)
C
                 wci(i,k) = amin1(tmp2i,.999999)
                 gci(i,k) = ebarii + tmp3i
                 fci(i,k) = gci(i,k)*gci(i,k)
C
C Set aerosol properties:
C
                 tauxar(i,k) = 1.e4 * ksa(ns) * uaer(i,k)
C
                 wa(i,k)     = wsa(ns)
                 ga(i,k)     = gsa(ns)
                 fa(i,k)     = gsa(ns)*gsa(ns)
C
                 waer(i)     = wa(i,k)
                 gaer(i)     = ga(i,k)
                 faer(i)     = fa(i,k)
C
              end do
           end do
        end do
C
C Set reflectivities for surface based on mid-point wavelength
C
        wavmid = 0.5*(wavmin(ns) + wavmax(ns))
C
C Wavelength less  than 0.7 micro-meter
C
        if(wavmid .lt. 0.7 ) then
           do n=1,nloop
              do i=is(n),ie(n)
                 albdir(i) = albs(i)
                 albdif(i) = albsd(i)
              end do
           end do
C
C Wavelength greater than 0.7 micro-meter
C
        else
           do n=1,nloop
              do i=is(n),ie(n)
                 albdir(i) = albl(i)
                 albdif(i) = albld(i)
              end do
           end do
        end if
        trayoslp = raytau(ns)/sslp
C
C Layer input properties now completely specified; compute the
C delta-Eddington solution reflectivities and transmissivities
C for each layer, starting from the top and working downwards:
C
        call radded(coszrs   ,trayoslp,pflx   ,abh2o(ns),abo3(ns),
     $              abco2(ns),abo2(ns),uh2o   ,uo3      ,uco2    ,
     $              uo2      ,tauxcl  ,wcl    ,gcl      ,fcl     ,
     $              tauxci   ,wci     ,gci    ,fci      ,tauxar  ,
     $              wa       ,ga      ,fa     ,nloop    ,is      ,
     $              ie       ,rdir    ,rdif   ,tdir     ,tdif    ,
     $              explay   ,exptdn  ,rdndif ,tottrn   )
C
C Compute reflectivity to direct and diffuse radiation for layers below
C by adding succesive layers starting from the surface and working
C upwards:
C
        do n=1,nloop
           do i=is(n),ie(n)
              rupdir(i,plevp) = albdir(i)
              rupdif(i,plevp) = albdif(i)
           end do
        end do
        do k=plev,0,-1
           do n=1,nloop
              do i=is(n),ie(n)
                 rdenom = 1./( 1. - rdif(i,k)*rupdif(i,k+1))
                 rupdir(i,k) = rdir(i,k) + tdif(i,k)*
     $                 (rupdir(i,k+1)*explay(i,k) +
     $                  rupdif(i,k+1)*(tdir(i,k)-explay(i,k)))*rdenom
                 rupdif(i,k) = rdif(i,k) +
     $                         rupdif(i,k+1)*tdif(i,k)**2*rdenom
              end do
           end do
        end do
C
C Compute up and down fluxes for each interface, using the added
C atmospheric layer properties at each interface:
C
        do k=0,plevp
           do n=1,nloop
              do i=is(n),ie(n)
                 rdenom = 1./(1. - rdndif(i,k)*rupdif(i,k))
                 fluxup(i,k) = (exptdn(i,k)*rupdir(i,k) +
     $                  (tottrn(i,k)-exptdn(i,k))*rupdif(i,k))*rdenom
                 fluxdn(i,k)=exptdn(i,k) + (tottrn(i,k) - exptdn(i,k) +
     $                  exptdn(i,k)*rupdir(i,k)*rdndif(i,k))*rdenom
              end do
           end do
        end do
C
C Compute flux divergence in each layer using the interface up and down
C fluxes:
C
        do k=0,plev
           do n=1,nloop
              do i=is(n),ie(n)
                 flxdiv(i,k) = (fluxdn(i,k  ) - fluxdn(i,k+1)) +
     $                         (fluxup(i,k+1) - fluxup(i,k  ))
              end do
           end do
        end do
C
C Monochromatic computation completed; accumulate in totals; adjust
C fraction within spectral interval to allow for the possibility of
C sub-divisions within a particular interval:
C
        psf = 1.0
        if(ph2o(ns).ne.0.) psf = psf*ph2o(ns)
        if(pco2(ns).ne.0.) psf = psf*pco2(ns)
        if(po2 (ns).ne.0.) psf = psf*po2 (ns)
        do n=1,nloop
           do i=is(n),ie(n)
              solflx(i)  = solin(i)*frcsol(ns)*psf
              fsnt(i) = fsnt(i) + solflx(i)*(fluxdn(i,1) - fluxup(i,1))
              fsns(i) = fsns(i) + solflx(i)*
     $                  (fluxdn(i,plevp) - fluxup(i,plevp))
              sfltot(i)  = sfltot(i) + solflx(i)
              fswup(i,0) = fswup(i,0) + solflx(i)*fluxup(i,0)
              fswdn(i,0) = fswdn(i,0) + solflx(i)*fluxdn(i,0)
              if (wavmid .lt. 0.7) then
                 sols(i)=sols(i) + exptdn(i,plevp)*solflx(i)*0.001
                 solsd(i)=solsd(i) + (fluxdn(i,plevp) -
     $                    exptdn(i,plevp)) * solflx(i)*0.001 
              else
                 soll(i)=soll(i) + exptdn(i,plevp)*solflx(i)*0.001
                 solld(i)=solld(i) + (fluxdn(i,plevp) -
     $                    exptdn(i,plevp)) * solflx(i)*0.001 
              end if
           end do
        end do
        do k=0,plev
           do n=1,nloop
              do i=is(n),ie(n)
                 totfld(i,k)  = totfld(i,k)  + solflx(i)*flxdiv(i,k)
                 fswup(i,k+1) = fswup(i,k+1) + solflx(i)*fluxup(i,k+1)
                 fswdn(i,k+1) = fswdn(i,k+1) + solflx(i)*fluxdn(i,k+1)
              end do
           end do
        end do
C
C
C Following code is the diagnostic clear sky computation:
C
C Compute delta-Eddington solution reflectivities and transmissivities
C for the entire column; note, for convenience, we use the same
C reflectivity and transmissivity arrays as for the full calculation
C above, where 0 for layer quantities refers to the entire atmospheric
C column, and where 0 for interface quantities refers to top of atmos-
C phere, while 1 refers to the surface:
C
C
C Compute total column aerosol optical depth:
C
        do n=1,nloop
           do i=is(n),ie(n)
             tauaer(i) = 1.e4 * ksa(ns) * utaer(i)
           enddo
        enddo
C
        call radclr(coszrs   ,trayoslp,pflx    ,abh2o(ns),abo3(ns) ,
     $              abco2(ns),abo2(ns),uth2o   ,uto3     ,utco2    ,
     $              uto2     ,tauaer  ,waer    ,gaer     ,faer     ,
     $              nloop    ,is      ,ie      ,rdir     ,rdif     ,
     $              tdir     ,tdif    ,explay  ,exptdn   ,rdndif   ,
     $              tottrn   )
C
C Compute reflectivity to direct and diffuse radiation for entire
C column; 0,1 on layer quantities refers to two effective layers
C overlying surface; 0 on interface quantities refers to top of column;
C 2 on interface quantities refers to the surface:
C
        do n=1,nloop
           do i=is(n),ie(n)
              rupdir(i,2) = albdir(i)
              rupdif(i,2) = albdif(i)
           end do
        end do
C
        do k=1,0,-1
           do n=1,nloop
              do i=is(n),ie(n)
                 rdenom = 1./( 1. - rdif(i,k)*rupdif(i,k+1))
                 rupdir(i,k) = rdir(i,k) + tdif(i,k)*
     $                 (rupdir(i,k+1)*explay(i,k) +
     $                  rupdif(i,k+1)*(tdir(i,k)-explay(i,k)))*rdenom
                 rupdif(i,k) = rdif(i,k) +
     $                        rupdif(i,k+1)*tdif(i,k)**2*rdenom
              end do
           end do
        end do
C
C Compute up and down fluxes for each interface, using the added
C atmospheric layer properties at each interface:
C
        do k=0,2
           do n=1,nloop
              do i=is(n),ie(n)
                 rdenom = 1./(1. - rdndif(i,k)*rupdif(i,k))
                 fluxup(i,k) = (exptdn(i,k)*rupdir(i,k) +
     $                  (tottrn(i,k)-exptdn(i,k))*rupdif(i,k))*rdenom
                 fluxdn(i,k)=exptdn(i,k) + (tottrn(i,k) - exptdn(i,k) +
     $                  exptdn(i,k)*rupdir(i,k)*rdndif(i,k))*rdenom
              end do
           end do
        end do
C
        do n=1,nloop
           do i=is(n),ie(n)
              fsntc(i) = fsntc(i) + solflx(i)*(fluxdn(i,0)-fluxup(i,0))
              fsnsc(i) = fsnsc(i) + solflx(i)*(fluxdn(i,2)-fluxup(i,2))
           end do
        end do
C
C End of clear sky calculation
C
  100 continue              ! End of spectral interval loop
C
C Compute solar heating rate (k/s)
C
      gocp = gravit/cpair
      do k=1,plev
         do n=1,nloop
           do i=is(n),ie(n)
              qrs(i,k) = -gocp*totfld(i,k)/(pint(i,k) - pint(i,k+1))
           end do
        end do
      end do

c++csz
#ifdef CRM_SRB
c     Initialize fluxes
      do i=1,plon
         dff_drc_SW_sfc(i)=-1.e36
         dff_drc_vsb_sfc(i)=-1.e36
         dff_drc_NIR_sfc(i)=-1.e36
         flx_spc_dwn_drc_sfc(i)=0.
         flx_SW_dwn_sfc(i)=0.
         flx_NIR_dwn_sfc(i)=0.
         flx_vsb_dwn_sfc(i)=0.
         flx_SW_dwn_drc_sfc(i)=0.
         flx_NIR_dwn_drc_sfc(i)=0.
         flx_vsb_dwn_drc_sfc(i)=0.
         flx_SW_dwn_dff_sfc(i)=0.
         flx_NIR_dwn_dff_sfc(i)=0.
         flx_vsb_dwn_dff_sfc(i)=0.
      enddo                     ! end loop over i, lon
c     Initialize unscaled extinction optical depths
      do ns=1,nspint
         do i=1,plon
            odxc_ttl(i,ns)=0.
         enddo                  ! end loop over i, lon
      enddo                     ! end loop over ns, spectral interval
c     Compute column optical depths in each spectral interval
      do ns=1,nspint
         do k=1,plev
            do n=1,nloop
               do i=is(n),ie(n)
                  odxc_ttl(i,ns)=odxc_ttl(i,ns)+
     $                 raytau(ns)*(pflx(i,k+1)-pflx(i,k))/sslp+ ! Rayleigh scattering
     $                 abh2o(ns)*uh2o(i,k)+abo3(ns)*uo3(i,k)+ ! H2O, O3
     $                 abco2(ns)*uco2(i,k)+abo2(ns)*uo2(i,k)+ ! CO2, O2
     $                 tauxcl(i,k)+ ! Liquid cloud
     $                 tauxci(i,k)+ ! Ice cloud
     $                 tauxar(i,k) ! Aerosol
               enddo            ! end loop over i, sunny lons in this group
            enddo               ! end loop over n, number of sunny lon groups
         enddo                  ! end loop over k, lev
      enddo                     ! end loop over ns, spectral interval
c     Compute spectral fluxes and accumulate in totals
      do ns=1,nspint
         wavmid=0.5*(wavmin(ns)+wavmax(ns))
         psf=1.0
         if (ph2o(ns).ne.0.) psf=psf*ph2o(ns)
         if (pco2(ns).ne.0.) psf=psf*pco2(ns)
         if (po2 (ns).ne.0.) psf=psf*po2(ns)
         do n=1,nloop
            do i=is(n),ie(n)
               xpn_arg=amin1(odxc_ttl(i,ns)/coszrs(i),25.)
               flx_spc_dwn_drc_sfc(i)=
     $              scon*eccf*coszrs(i)* ! (Solar constant)*(AUs)*(cos(SZA))
     $              frcsol(ns)* ! Fraction of solar flux in spectral interval
     $              psf*        ! Weight of interval for overlapping intervals
     $              0.001*      ! Convert CGS --> MKS
     $              exp(-xpn_arg) ! Bougher's law
               flx_SW_dwn_drc_sfc(i)=flx_SW_dwn_drc_sfc(i)+
     $              flx_spc_dwn_drc_sfc(i)
               if (wavmid.lt.0.7) then
                  flx_vsb_dwn_drc_sfc(i)=flx_vsb_dwn_drc_sfc(i)+
     $                 flx_spc_dwn_drc_sfc(i)
               else
                  flx_NIR_dwn_drc_sfc(i)=flx_NIR_dwn_drc_sfc(i)+
     $                 flx_spc_dwn_drc_sfc(i)
               end if           ! endif NIR
            enddo               ! end loop over i, sunny lons in this group
         enddo                  ! end loop over n, number of sunny lon groups
      enddo                     ! end loop over ns, spectral interval
c     Use knowledge of direct and total downward fluxes to get diffuse fluxes
      do n=1,nloop
         do i=is(n),ie(n)
            flx_SW_dwn_sfc(i)=fswdn(i,plevp)*0.001 ! CGS --> MKS
            flx_vsb_dwn_sfc(i)=sols(i)+solsd(i)
            flx_NIR_dwn_sfc(i)=soll(i)+solld(i)
            flx_SW_dwn_dff_sfc(i)=flx_SW_dwn_sfc(i)-flx_SW_dwn_drc_sfc(i)
            flx_vsb_dwn_dff_sfc(i)=flx_vsb_dwn_sfc(i)-flx_vsb_dwn_drc_sfc(i)
            flx_NIR_dwn_dff_sfc(i)=flx_NIR_dwn_sfc(i)-flx_NIR_dwn_drc_sfc(i)
            if (flx_SW_dwn_drc_sfc(i).gt.0.) dff_drc_SW_sfc(i)=flx_SW_dwn_dff_sfc(i)/flx_SW_dwn_drc_sfc(i)
            if (flx_vsb_dwn_drc_sfc(i).gt.0.) dff_drc_vsb_sfc(i)=flx_vsb_dwn_dff_sfc(i)/flx_vsb_dwn_drc_sfc(i)
            if (flx_NIR_dwn_drc_sfc(i).gt.0.) dff_drc_NIR_sfc(i)=flx_NIR_dwn_dff_sfc(i)/flx_NIR_dwn_drc_sfc(i)
         enddo                  ! end loop over i, sunny lons in this group
      enddo                     ! end loop over n, number of sunny lon groups
#endif
c--csz
C
      return
#ifdef LOGENTRY 
$Log: radcsw.F,v $
Revision 1.1  1998/02/19 23:09:51  zender
Initial revision

c Revision 1.4  1995/03/17  18:54:09  ccm2
c add globally uniform background aerosol.
c
c Revision 1.2  1995/02/17  21:28:31  jhack
c Next rev of omega physics, ice, clouds, liquid water, convection, etc.
c
c Revision 1.2  1994/09/16  21:08:42  rosinski
c This is the first part of plx23.
c 
#endif
      end

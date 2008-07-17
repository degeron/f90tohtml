c
c $Id: crdalb.h,v 1.1.1.1 1995/02/09 23:26:46 ccm2 Exp $
c $Author: ccm2 $
c
C
C Surface albedo data
C
C The albedos are computed for a model grid box by ascribing values to
C 1x1 degree points of a vegetation dataset, then linearly averaging
C for each grid box; ocean and land values are averaged together along
C coastlines; the fraction of every grid box that has strong zenith
C angle dependence is included also (see Briegleb, Bruce P., 1992:
C Delta-Eddington Approximation for Solar Radiation in the NCAR
C Community Climate Model, Journal of Geophysical Research, Vol 97, D7,
C pp7603-7612).
C
      common/crdalb/albvss(plond,plat),albvsw(plond,plat),
     $              albnis(plond,plat),albniw(plond,plat),
     $              frctst(plond,plat)
C
C vis  = 0.2 - 0.7 micro-meters wavelength range
C nir  = 0.7 - 5.0 micro-meters wavelength range
C
C szad = strong zenith angle dependent
C wzad = weak   zenith angle dependent
C
      real albvss, ! Grid box alb for vis over szad surfaces
     $     albvsw, ! Grid box alb for vis over wzad surfaces
     $     albnis, ! Grid box alb for nir over szad surfaces
     $     albniw, ! Grid box alb for nir over wzad surfaces
     $     frctst  ! Fraction of area in grid box with szad surfaces
C
C Surface boundary data
C
C Vegtyp is used to specify the thermal properites of the surface, as
C well as determine the location of permanent land ice points; it is the
C dominant surface type within the model grid box based on the 1x1
C degree resolution vegetation dataset; it is encoded in the following
C manner:
C
C   1        ocean
C   2        sea ice
C   3        permanent land ice
C   4        tropical evergreen forest
C   5        deciduous forest
C   6        grassland/tundra
C   7        desert
C
C Rghnss is the aerodynamic roughness length for the grid box, computed
C by linear averaging of the values ascribed to the 1x1 degree
C resolution vegetation dataset; ocean and land values are averaged
C together along coastlines.
C
C Evapf is the ratio of actual to potential evaporation, and is computed
C from the 1x1 degree resolution vegetation dataset in a manner similar
C to the aerodynamic roughness.
C
C Vevapf allows for variable snow cover, where the underlying
C evaporability factor is modified.
C
C Snwjan and snwjly are mean climatological snow depths (liquid water
C equivalent) used to compute the prescribed daily values of snow cover.
C
      common/crdsrf/vegtyp(plond,plat),rghnss(plond,plat),
     $              evapf (plond,plat),vevapf(plond,plat),
     $              snwjan(plond,plat),snwjly(plond,plat)
C
      real vegtyp, ! Surface thermal type, based on veg type
     $     rghnss, ! Aerodynamic roughness length
     $     evapf , ! Constant surface evaporability
     $     vevapf, ! Variable surface evaporability
     $     snwjan, ! Snow cover (liq water equiv) for January
     $     snwjly  ! Snow cover (liq water equiv) for July
C
#ifdef LOGENTRY 
$Log: crdalb.h,v $
 * Revision 1.1.1.1  1995/02/09  23:26:46  ccm2
 * Start Omega Model. Split into spectral/sld modules
 *
 * Revision 1.1.1.1  1994/04/21  23:55:13  ccm2
 * First cvs version of plx22
 * 
#endif

#include <params.h>
      subroutine torgrid(pmidm   ,pintm   ,pmlnm   ,pilnm   ,tm      ,
     $                   h2ommrm ,cldm    ,effcldm ,clwpm   ,
     $                   pmid    ,pint    ,pmln    ,piln    ,t       ,
     $                   h2ommr  ,cld     ,effcld  ,clwp    )
C-----------------------------------------------------------------------
C
C Interpolate model arrays to radiation vertical grid.
C
C------------------------------Parameters-------------------------------
c
c $Id: torgrid.F,v 1.1.1.1 1995/02/09 23:27:10 ccm2 Exp $
c $Author: ccm2 $
c
#include <pmgrid.h>
#include <ptrrgrid.h>
C-----------------------------------------------------------------------
C
C Input arguments
C
      real pmidm(plond,plev),
     $     pintm(plond,plevp),
     $     pmlnm(plond,plev),
     $     pilnm(plond,plevp),
     $     tm(plond,plev),
     $     h2ommrm(plond,plev),
     $     cldm(plond,plevp),
     $     effcldm(plond,plevp),
     $     clwpm(plond,plev)
C
C Output arguments
C
      real pmid(plond,plevr),
     $     pint(plond,plevrp),
     $     pmln(plond,plevr),
     $     piln(plond,plevrp),
     $     t(plond,plevr),
     $     h2ommr(plond,plevr),
     $     cld(plond,plevrp),
     $     effcld(plond,plevrp),
     $     clwp(plond,plevr)
C
C Code to interpolate goes here.  Do nothing could be coded as a memory
C transfer, but left out for efficiency considerations.
C
      return
#ifdef LOGENTRY 
$Log: torgrid.F,v $
c Revision 1.1.1.1  1995/02/09  23:27:10  ccm2
c Start Omega Model. Split into spectral/sld modules
c
c Revision 1.2  1994/06/10  23:37:25  bath
c bum steer
c
c Revision 1.1.1.1  1994/04/21  23:55:10  ccm2
c First cvs version of plx22
c 
#endif
      end

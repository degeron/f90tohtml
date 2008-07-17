#include <params.h>
      subroutine fmrgrid(qrs     ,qrl     ,
     $                   qrsm    ,qrlm    )
C-----------------------------------------------------------------------
C
C Interpolate model arrays to radiation vertical grid.
C
C------------------------------Parameters-------------------------------
c
c $Id: fmrgrid.F,v 1.1.1.1 1995/02/09 23:26:49 ccm2 Exp $
c $Author: ccm2 $
c
#include <pmgrid.h>
#include <ptrrgrid.h>
C-----------------------------------------------------------------------
C
C Input arguments (radiation grid)
C
      real qrs(plond,plevr),     ! Shortwave heating rate
     $     qrl(plond,plevr)      ! Longwave heating rate
C
C Output arguments (model grid)
C
      real qrsm(plond,plev),     ! Shortwave heating rate
     $     qrlm(plond,plev)      ! Longwave heating rate
C
C Code to interpolate goes here.  Do nothing could be coded as a memory
C transfer, but left out for efficiency considerations.
C
      return
#ifdef LOGENTRY 
$Log: fmrgrid.F,v $
c Revision 1.1.1.1  1995/02/09  23:26:49  ccm2
c Start Omega Model. Split into spectral/sld modules
c
c Revision 1.1.1.1  1994/04/21  23:55:01  ccm2
c First cvs version of plx22
c 
#endif
      end









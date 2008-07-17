c
c $Id: pagrid.h,v 1.3 1995/03/03 17:47:17 bonan Exp $
c $Author: bonan $
c
C
C Model grid point resolution parameters.
C
      integer
     $     plnlv,    ! Length of multilevel field slice
     $     plndlv,   ! Length of multilevel 3-d field slice
     $     pbflnb,   ! Length of buffer 1
     $     pbflna,   ! Length of buffer 2
     $     pbflnm1,  ! Length of buffer m1
     $     pflenb,   ! Length of buffer 1, padded for unblocked I/O
     $     pflena,   ! Length of buffer 2, padded for unblocked I/O
     $     plenalcl, ! Length of buffer 2, needed in SPEGRD
     $     ptifld,   ! Number of fields on time-invariant boundary dataset
     $     ptvsfld,  ! Number of fields on time-variant boundary dataset
     $     ptvofld,  ! Number of fields on ozone dataset
     $     plenhi,   ! Length of integer header record
     $     plenhc,   ! Length of character header record
     $     plenhr,   ! Length of real header record
     $     plexbuf,  ! Length of communication buffer for flux coupling
     $     ptapes,   ! Maximum number of history tapes allowed
     $     pflds     ! Number of fields in master field list
      integer
     $     ptileni,  ! Length of time-invariant integer header
     $     ptilenc,  ! Length of time-invariant character header
     $     ptvoleni, ! Length of ozone integer header
     $     ptvolenc, ! Length of ozone character header
     $     ptvsleni, ! Length of time-variant integer header
     $     ptvslenc  ! Length of time-variant character header
      integer
     $     plenhis,  ! Length of integer header scalars
     $     plenhcs,  ! Length of character header scalars
     $     ptilenis, ! Length of time-invariant integer scalars
     $     ptilencs, ! Length of time-invariant character scalars
     $     ptolenis, ! Length of ozone integer header scalars
     $     ptolencs, ! Length of ozone character header scalars
     $     ptslenis, ! Length of time-variant integer header scalars
     $     ptslencs  ! Length of time-variant character header scalars
C
      parameter(plnlv=plon*plev,plndlv=plond*plev)
C
C In pbflnb, 9 multi-level fields include the plev levels of plol and
C plos. 2 multi-level fields are pcnst-dependent.
C There are plevmx sub-surface temperature fields. (See User's Guide 
C for complete buffer description)
C There are 4 single-level fields to hold albedos
C
      parameter(pbflnb=(7 + 2*pcnst)*plndlv + (15+plevmx+pcnst)*plond,
C
C In pbflna, there are 3 multi-level and 3 single-level fields.
C
     $          pbflna = (3 + 3*plev)*plond,
     $          pbflnm1 = (1 + 2*plev)*plond,
     $          pflenb = ((pbflnb + pbflnm1)/512 + 1)*512,
     $          pflena = (pbflna/512 + 1)*512,
C
C plenalcl is the buffer size as required in SPEGRD.  
C Only pflena is read/written.
C
     $          plenalcl = ((pbflna + 3*plndlv + plond)/512 + 1)*512,
     $          plexbuf = (((1 + 7*plev)*plond)/512+1)*512,
     $          ptapes = 6,
C
C 8 fields in master list are pcnst-dependent 2 fields occur only
C if pcnst > 1
C
#if ( defined COUP_LSM )
     $          pflds=92+8*pcnst+2*(pcnst-1)+plevmx+25)
#else
     $          pflds=92+8*pcnst+2*(pcnst-1)+plevmx)
#endif
      parameter(ptifld = 11, ptvsfld = 1, ptvofld = 2)
C
C There are 37 scalar words in the integer header and 89 scalar words
C in the character header
C
      parameter(plenhis=37,plenhcs=89,
     $          plenhi=plenhis+3*pflds,plenhc=plenhcs+2*pflds,
     $          plenhr=3*(2*plev + 1) + 2*plat,
     $          ptilenis=plenhis, ptilencs=plenhcs,
     $          ptileni=ptilenis+3*ptifld, ptilenc=ptilencs+2*ptifld,
     $          ptolenis=plenhis, ptolencs=plenhcs,
     $          ptvoleni=ptolenis+3*ptvofld,ptvolenc=ptolencs+2*ptvofld,
     $          ptslenis=plenhis, ptslencs=plenhcs,
     $          ptvsleni=ptslenis+3*ptvsfld,ptvslenc=ptslencs+2*ptvsfld)







#ifdef LOGENTRY 
$Log: pagrid.h,v $
 * Revision 1.3  1995/03/03  17:47:17  bonan
 * add land surface model
 *
 * Revision 1.2  1995/02/10  01:09:03  ccm2
 * Add spectral code to omega, create omega0_1
 *
 * Revision 1.2  1994/09/16  21:08:27  rosinski
 * This is the first part of plx23.
 * 
#endif

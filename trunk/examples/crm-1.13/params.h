#ifndef PARAMS_SET
#define PARAMS_SET
c#define SUN 1
c#define RS6K 1
c#define CRAY 1
c#define NETCDF 1
#define CRM_SRB 1
#define PCNST 1
#define PLON 1
#define PLAT 1
#define PLEV 18
#define PLEVR 18
#define PTRM 1
#define PTRN 1
#define PTRK 1
#undef NOT_DEFINED
#if defined NOT_DEFINED
#define COUP_LSM
#define INCOREBUF
#define INCORERAD
#define FORTFFT
#endif
#if ( ! defined CRAY ) && ( ! defined RS6K ) && ( ! defined SUN )
You must define one of CRAY, RS6K, or SUN
Recompile specifying -DSUN or -DCRAY or -DRS6K to the compiler.
#endif
#if ( ! defined NETCDF )
c If you have netCDF installed and would like the column model to write 
c a netCDF output file, then recompile specifying -DNETCDF and -lnetcdf
#endif
#if ( defined INCORE )
'INCORE' has been changed to 'INCOREBUF' and 'INCORERAD'
#endif
#endif

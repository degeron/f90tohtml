# $Header: /home/zender/crm/RCS/Makefile,v 1.2 1996/08/28 14:24:22 zender Exp zender $

# The first LDFLAGS is for typical C programs with netCDF, math, and networking
# The second LDFLAGS enables C/Fortran linking

# NB: This Makefile requires GNU make. AT&T make will not parse it.

ifndef OPTS
 OPTS := O
endif
ifndef PVM_ARCH
 PVM_ARCH := $(shell ./pvmgetarch)
endif
ifneq (,$(findstring SUN,$(PVM_ARCH)))
 CCM_TARG_FLAG := SUN
endif
ifneq (,$(findstring CRAY,$(PVM_ARCH)))
 CCM_TARG_FLAG := CRAY
endif
ifneq (,$(findstring RS6K,$(PVM_ARCH)))
 CCM_TARG_FLAG := RS6K
endif
ifndef MY_OBJ_DIR
 MY_OBJ_DIR := ./src
endif
ifndef MY_INC_DIR
 MY_INC_DIR := . -I./src
endif
ifndef MY_LIB_DIR
 MY_LIB_DIR := .
endif
ifndef MY_BIN_DIR
 MY_BIN_DIR := .
endif

# Redefine the default patterns for C compiling
$(MY_OBJ_DIR)/%.o : %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $(MY_OBJ_DIR)/$(notdir $@)

ifeq ($(PVM_ARCH),CRAY)
C++ := g++
CC := cc
CPPFLAGS := -Wp"-F -D$(CCM_TARG_FLAG) -I$(MY_INC_DIR) -I/usr/local/include"
FC := cf77
LD := ld
#LDFLAGS := -L$(MY_LIB_DIR) -L/usr/local/lib -lnetcdf -lm
#LDFLAGS := -L$(MY_LIB_DIR) -ljeffcdf_0024KW -lm
#LDFLAGS := $(LDFLAGS) -L/lib -lf
LEX := lex
LINT := lint
YACC := yacc
ifeq ($(OPTS),O)
 CFLAGS := -O
endif
ifeq ($(OPTS),D)
 CFLAGS := -g
endif
$(MY_OBJ_DIR)/%.o : %.F
	$(FC) -c $(FFLAGS) $(CPPFLAGS) $<
	mv -f $(notdir $@) $(MY_OBJ_DIR)
$(MY_OBJ_DIR)/%.o : %.f
	$(FC) -c $(FFLAGS) $<
	mv -f $(notdir $@) $(MY_OBJ_DIR)
endif

ifeq ($(PVM_ARCH),LINUX)
C++ := g++
CC := gcc -ansi
# NB: nameser.h needs -Di386, but gcc is sending -Di586 (on pentiums)
CPPFLAGS := -D$(CCM_TARG_FLAG) -I$(MY_INC_DIR) -Di386 -I/usr/local/include
FC := g77
LD := ld
#LDFLAGS := -L$(MY_LIB_DIR) -L/usr/local/lib -lnetcdf -lm
#LDFLAGS := $(LDFLAGS)
LEX := lex
LINT := lint
YACC := yacc
ifeq ($(OPTS),O)
 CFLAGS := -O
endif
ifeq ($(OPTS),D)
 CFLAGS := -g
endif
$(MY_OBJ_DIR)/%.o : %.F
	$(FC) -c $(FFLAGS) $(CPPFLAGS) -o $(MY_OBJ_DIR)/$(notdir $@) $<
$(MY_OBJ_DIR)/%.o : %.f
	$(FC) -c $(FFLAGS) -o $(MY_OBJ_DIR)/$(notdir $@) $<
endif

ifeq ($(PVM_ARCH),RS6K)
C++ := g++
CC := gcc -ansi
CPP := /lib/cpp -P
CPPFLAGS := -D$(CCM_TARG_FLAG) -I$(MY_INC_DIR) -I/usr/local/include
FC := xlf
LD := ld
#LDFLAGS := -L$(MY_LIB_DIR) -L/usr/local/lib -lncaru -lnetcdf -lm
#LDFLAGS := $(LDFLAGS) -lxlf90 -lxlf
LEX := lex
LINT := lint
YACC := yacc
ifeq ($(OPTS),O)
 CFLAGS := -O2
 PREPROCESS.F := $(CPP) $(CPPFLAGS)
 FFLAGS := -O -NS2000 -qfixed=132
endif
ifeq ($(OPTS),D)
 CFLAGS := -g
 PREPROCESS.F := $(CPP) $(CPPFLAGS)
 FFLAGS := -g -NS2000 -qfixed=132
endif
$(MY_OBJ_DIR)/%.o : %.F
	$(CPP) $(CPPFLAGS) $< $(MY_OBJ_DIR)/$(basename $<).f 
	$(FC) -c $(FFLAGS) -o $(MY_OBJ_DIR)/$(notdir $@) $(MY_OBJ_DIR)/$(basename $<).f
$(MY_OBJ_DIR)/%.o : %.f
	$(FC) -c $(FFLAGS) -o $(MY_OBJ_DIR)/$(notdir $@) $<
endif

ifeq ($(PVM_ARCH),SGI5)
C++ := g++
CC := gcc -ansi
CPPFLAGS := -D$(CCM_TARG_FLAG) -I$(MY_INC_DIR) -I/opt/netcdf-2.3.2/include
FC := f77
LD := ld
#LDFLAGS := -L$(MY_LIB_DIR) -L/opt/netcdf-2.3.2/lib -lnetcdf -lm
#LDFLAGS := $(LDFLAGS) -lF77 -lI77 -lU77 -lftn
LEX := lex
LINT := lint
YACC := yacc
ifeq ($(OPTS),O)
 CFLAGS := -O2
 FFLAGS := -O2 -e -Nl99 -extend_source
endif
ifeq ($(OPTS),D)
 CFLAGS := -g
 FFLAGS := -g -e -Nl99 -extend_source
endif
$(MY_OBJ_DIR)/%.o : %.F
	$(FC) -c $(FFLAGS) $(CPPFLAGS) -o $(MY_OBJ_DIR)/$(notdir $@) $<
$(MY_OBJ_DIR)/%.o : %.f
	$(FC) -c $(FFLAGS) -o $(MY_OBJ_DIR)/$(notdir $@) $<
endif

ifeq ($(PVM_ARCH),SUN4)
C++ := g++
CC := acc
CPPFLAGS := -D$(CCM_TARG_FLAG) -I$(MY_INC_DIR) -I/contrib/include
FC := f77
LD := ld
#LDFLAGS := -L$(MY_LIB_DIR) -L/contrib/lib -lnetcdf -lm
#LDFLAGS := $(LDFLAGS) -cg92 -L/opt/SUNWspro/SC3.0/lib/cg92 -lF77 -lM77 -lresolv
LEX := lex
LINT := lint
YACC := yacc
ifeq ($(OPTS),O)
 CFLAGS := -O2
 FFLAGS := -fast -e -Nl99
endif
ifeq ($(OPTS),D)
 CFLAGS := -g
 FFLAGS := -g -e -Nl99
endif
$(MY_OBJ_DIR)/%.o : %.F
	$(FC) -c $(FFLAGS) $(CPPFLAGS) -o $(MY_OBJ_DIR)/$(notdir $@) $<
$(MY_OBJ_DIR)/%.o : %.f
	$(FC) -c $(FFLAGS) -o $(MY_OBJ_DIR)/$(notdir $@) $<
endif

ifeq ($(PVM_ARCH),SUN4SOL2)
C++ := g++
CC := gcc -ansi
CPPFLAGS := -D$(CCM_TARG_FLAG) -I$(MY_INC_DIR) -I/contrib/include
FC := f77
LD := ld
#LDFLAGS := -L$(MY_LIB_DIR) -L/contrib/lib -lnetcdf -lsunmath -lsocket -lnsl -lm
#LDFLAGS := $(LDFLAGS) -lF77 -lM77 -lresolv
LEX := lex
LINT := lint
YACC := yacc
ifeq ($(OPTS),O)
 CFLAGS := -O2
 FFLAGS := -fast -e -Nl99
endif
ifeq ($(OPTS),D)
 CFLAGS := -g
 FFLAGS := -g -e -Nl99
endif
$(MY_OBJ_DIR)/%.o : %.F
	$(FC) -c $(FFLAGS) $(CPPFLAGS) -o $(MY_OBJ_DIR)/$(notdir $@) $<
$(MY_OBJ_DIR)/%.o : %.f
	$(FC) -c $(FFLAGS) -o $(MY_OBJ_DIR)/$(notdir $@) $<
endif

ifeq ($(PVM_ARCH),SUNMP)
C++ := g++
CC := gcc -ansi
CPPFLAGS := -D$(CCM_TARG_FLAG) -I$(MY_INC_DIR) -I/contrib/include
FC := f77
LD := ld
#LDFLAGS := -L$(MY_LIB_DIR) -L/contrib/lib -lnetcdf -lsunmath -lthread -lsocket -lnsl -lm
#LDFLAGS := $(LDFLAGS) -lF77 -lM77 -lresolv
LEX := lex
LINT := lint
YACC := yacc
ifeq ($(OPTS),O)
 CFLAGS := -O2
 FFLAGS := -fast -e -Nl99
endif
ifeq ($(OPTS),D)
 CFLAGS := -g
 FFLAGS := -g -e -Nl99
endif
$(MY_OBJ_DIR)/%.o : %.F
	$(FC) -c $(FFLAGS) $(CPPFLAGS) -o $(MY_OBJ_DIR)/$(notdir $@) $<
$(MY_OBJ_DIR)/%.o : %.f
	$(FC) -c $(FFLAGS) -o $(MY_OBJ_DIR)/$(notdir $@) $<
endif

ifeq ($(PVM_ARCH),LINUX)
C++ := g++
CC := gcc -ansi
CPPFLAGS := -D$(CCM_TARG_FLAG) -I$(MY_INC_DIR) -I/usr/local/include
FC := g77
LD := ld
#LDFLAGS := -L$(MY_LIB_DIR) -L/contrib/lib -lnetcdf -lsunmath -lthread -lsocket -lnsl -lm
#LDFLAGS := $(LDFLAGS) -lF77 -lM77 -lresolv
LEX := flex
LINT := lint
YACC := bison
ifeq ($(OPTS),O)
 CFLAGS := -O2
 FFLAGS := -fast -e -Nl99
endif
ifeq ($(OPTS),D)
 CFLAGS := -g
 FFLAGS := -g -e -Nl99
endif
$(MY_OBJ_DIR)/%.o : %.F
	$(FC) -c $(FFLAGS) $(CPPFLAGS) -o $(MY_OBJ_DIR)/$(notdir $@) $<
$(MY_OBJ_DIR)/%.o : %.f
	$(FC) -c $(FFLAGS) -o $(MY_OBJ_DIR)/$(notdir $@) $<
endif

ifndef $(precision)
 precision := single
endif
ifeq ($(precision),double)
ifeq ($(PVM_ARCH),SUNMP)
 FFLAGS := $(FFLAGS) -r8 -i4
endif
ifeq ($(PVM_ARCH),RS6K)
 FFLAGS := $(FFLAGS) -qREALSIZE=8 -qINTSIZE=4
endif
ifeq ($(PVM_ARCH),SGI5)
 FFLAGS := $(FFLAGS) -r8 -i4
endif
endif

CRM_OBJ := $(MY_OBJ_DIR)/crm.o

CRM_OBJ := $(MY_OBJ_DIR)/crm.o
crm:	$(CRM_OBJ) params.h
	$(FC) -o $(MY_BIN_DIR)/crm $(CRM_OBJ) $(LDFLAGS) 
	chmod 744 $(MY_BIN_DIR)/crm



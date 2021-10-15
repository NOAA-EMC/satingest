SHELL=/bin/sh

# If this makefile is invoked by the command line
#       make -f makefile
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable named CMD.
#
# For a quick method to move that executable to it's expected target
# location ../../exec,  use the command line
#       make -f makefile install
#
# To remove all files produced by this makefile but leave the executable(s) use
# the command line
#       make -f makefile clean
#
# To remove all files produced by this makefile including the executable(s) use
# the command line
#       make -f makefile clobber
#
# To remove the source files and this makefile use the command line
#       make -f makefile void
#
# The parameters SRCS and OBJS should not need to be changed.  If, however,
# you need to add a new module add the name of the source module to the
# SRCS parameter and add the name of the resulting object file to the OBJS
# parameter.  The new modules are not limited to Fortran, but may be C, YACC,
# LEX, or CAL.  An explicit rule will need to be added for PASCAL modules.

SRCS =	to3_datasets.f90 tranomi.f90

OBJS =  to3_datasets.o tranomi.o


# Tunable parameters
#
# FC            Name of the compiling system to use
# LDFLAGS       Flags to the loader
# LIBS          List of libraries
# CMD           Name of the executable
# FFLAGS        Flags to the compiler
# DEBUG         Flags for extra debugging

FC = ftn
LIBS = $(W3NCO_LIB4) $(BUFR_LIB4) -L $(HDF5_ROOT)/lib -lhdf5_fortran \
 -lhdf5 $(Z_LIB)
CMD = bufr_tranomi

#DEBUG =		-g -traceback
FFLAGS = -O2 -convert big_endian -list -auto -assume noold_ldout_format \
 -I $(HDF5_ROOT)/include  $(DEBUG)


# Lines from here on down should not need to be changed.  They are the actual
# rules which make uses to build CMD.

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

.SUFFIXES: .o .f90

.f90.o:
	$(FC) $(FFLAGS) -c $<

.PHONY: clean clobber void install
clean:
	-rm -f $(OBJS) xref.db *.lst *.mod

clobber:	clean
	-rm -f $(CMD)

void:	clobber
	-rm -f $(SRCS) makefile

install:
	mv $(CMD) ../../exec/

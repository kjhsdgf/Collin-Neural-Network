#===============================================================================
# Copyright 2006-2017 Intel Corporation All Rights Reserved.
#
# The source code,  information  and material  ("Material") contained  herein is
# owned by Intel Corporation or its  suppliers or licensors,  and  title to such
# Material remains with Intel  Corporation or its  suppliers or  licensors.  The
# Material  contains  proprietary  information  of  Intel or  its suppliers  and
# licensors.  The Material is protected by  worldwide copyright  laws and treaty
# provisions.  No part  of  the  Material   may  be  used,  copied,  reproduced,
# modified, published,  uploaded, posted, transmitted,  distributed or disclosed
# in any way without Intel's prior express written permission.  No license under
# any patent,  copyright or other  intellectual property rights  in the Material
# is granted to  or  conferred  upon  you,  either   expressly,  by implication,
# inducement,  estoppel  or  otherwise.  Any  license   under such  intellectual
# property rights must be express and approved by Intel in writing.
#
# Unless otherwise agreed by Intel in writing,  you may not remove or alter this
# notice or  any  other  notice   embedded  in  Materials  by  Intel  or Intel's
# suppliers or licensors in any way.
#===============================================================================

## Content:
##      Build standalone library of FFTW3 C wrappers to Intel(R) MKL.
##*****************************************************************************

## Usage examples:
##
## nmake libia32
##        Build the library for 32-bit applications using
##        Intel C++ compiler (as default).
##
## nmake libintel64 compiler=msvs
##        Build the library with Microsoft C++ compiler.

help:
	@type <<
  Usage: nmake {libia32|libintel64} [option...]

  Options:
  compiler=msvs|pgi|intel
        Build the library with a Microsoft C compiler, PGI C compiler, or
        Intel(R) C Compiler.
        Default value: intel

  Additional macros:
  MKLROOT=<path>
        Path to Intel(R) MKL root directory with header files and libraries.
        Default value: ..\..

  INSTALL_DIR=<path>
        Install the library to the specified location, which must exist.
        Default value: . (that is, the current directory)

  INSTALL_LIBNAME=<name>
        Specify the name of the library.
        Default value depends on compiler used.

<<NOKEEP

##-----------------------------------------------------------------------------
## Default values

!include fftw3xc.lst

compiler = intel

!ifndef MKLROOT
MKLROOT = ..\..
!endif

!ifndef INSTALL_DIR
!ifdef install_to
INSTALL_DIR = $(install_to)
!else
INSTALL_DIR = .
!endif
!endif

obj_path = $(INSTALL_DIR)\obj_$(compiler)

!ifndef INSTALL_LIBNAME
!ifdef install_as
INSTALL_LIBNAME = $(install_as)
!else
INSTALL_LIBNAME = fftw3xc_$(compiler).lib
!endif
!endif

_passdown = compiler=$(compiler) MKLROOT="$(MKLROOT:^"=)" \
            INSTALL_DIR="$(INSTALL_DIR:^"=)" INSTALL_LIBNAME=$(INSTALL_LIBNAME)

##-----------------------------------------------------------------------------
## Main targets

libia32 lib32:
	nmake lib _IA=ia32 $(_passdown) tmpdir="$(tmpdir:^"=)_ia32"

libintel64 libem64t:
	nmake lib _IA=intel64 $(_passdown) tmpdir="$(tmpdir:^"=)_intel64"

!ifdef _IA
##-----------------------------------------------------------------------------
## Supporting _macros

!if ("$(compiler)" == "msvs")
CC = cl
!else if ("$(compiler)" == "pgi")
CC = pgcc
!else
CC = icl
!endif

_cflags = $(_cflags_1) $(_cflags_2) $(CFLAGS)
_cppflags = -I"$(MKLROOT:^"=)"\include -I"$(MKLROOT:^"=)"\include\fftw $(CPPFLAGS)

!if ("$(compiler)" != "pgi")
_cflags_1 = /MT
!endif

!if ("$(compiler)" == "intel" && "$(_IA)" == "ia32")
## This option is required by Intel(R) compiler to produce
## Pentium(R) III compatible executables.
_cflags_2 = /arch:ia32
!endif

_objs = $(WRAP:+=.obj)

##-----------------------------------------------------------------------------
## Rules

.SUFFIXES:
.SUFFIXES: .c .obj

lib: clean "$(INSTALL_DIR:^"=)\$(INSTALL_LIBNAME)" cleanup

# NMAKE provides no way to add prefix $(obj_path)\ to $(_objs),
# so we use $(_objs) as pseudotargets
"$(INSTALL_DIR:^"=)\$(INSTALL_LIBNAME)": $(_objs)
	lib /out:$@ "$(obj_path)\*.obj"

# Nmake doesn't allow spaces in {$(obj_path)}.obj, use fake {}.obj instead.
!if ("$(compiler)" == "pgi")
{wrappers}.c{}.obj:
	$(CC) $(_cflags) $(_cppflags) -c -o"$(obj_path)"\$(@F) $<
!else
{wrappers}.c{}.obj:
	$(CC) $(_cflags) $(_cppflags) -c /Fo"$(obj_path)"\ $<
!endif

clean:
	-rd /s/q "$(obj_path)"
	md "$(obj_path)"
	-del /f/q "$(INSTALL_DIR:^"=)\$(INSTALL_LIBNAME)"

cleanup:
	-rd /s/q "$(obj_path)"

##-----------------------------------------------------------------------------
!endif

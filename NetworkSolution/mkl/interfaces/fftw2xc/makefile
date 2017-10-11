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
##      Build standalone library of FFTW2 C wrappers to Intel(R) MKL.
##*****************************************************************************

help:
	@type <<
  Usage: nmake {libia32|libintel64}
               [PRECISION=MKL_DOUBLE|MKL_SINGLE]
               [compiler=compiler_name]

  compiler_name=msvs - using Microsoft C compiler

  Intel(R) C Compiler as default

<<NOKEEP

##-----------------------------------------------------------------------------
## examples of using:
##
## nmake libia32 - build by Intel C++ compiler (as default)
##
## nmake libintel64 compiler=msvs - build by Microsoft C++ compiler
##
##-----------------------------------------------------------------------------

!include fftw2xc.lst

sufobj   = obj
suflib   = lib
obj_path = obj_$(prec)_$(compiler)
LIBR     = lib -out:

!ifndef MKLROOT
MKLROOT = ..\..
!endif

!if ("$(ARCH)" != "")
! ifndef INSTALL_DIR
INSTALL_DIR = $(MKLROOT:^"=)\lib\$(ARCH)
obj_path = obj_$(prec)_$(compiler)
! else
obj_path = "$(INSTALL_DIR:^"=)\obj_$(prec)_$(compiler)"
! endif
!endif

objs = $(WRAP:+=.obj)

!if "$(PLAT)" == "win32"
IA = ia32
!else if "$(PLAT)" == "win32e"
IA = intel64
!else
IA = ERROR
!endif

!if ("$(PRECISION)" != "MKL_SINGLE")
PRECISION = MKL_DOUBLE
prec = double
!else
prec = single
!endif

!if ("$(compiler)" == "msvs")
CC = cl
!else
compiler = intel
CC = icl
!endif

COPTS = $(COPTS)

!ifndef INSTALL_LIBNAME
INSTALL_LIBNAME = fftw2xc_$(prec)_$(compiler).lib
!endif

##-----------------------------------------------------------------------------
## Rules

.SUFFIXES:
.SUFFIXES: .c .obj

libia32 lib32:
	nmake PLAT=win32 lib ARCH=ia32 \
	  compiler=$(compiler) PRECISION=$(PRECISION)

libintel64 libem64t:
	nmake PLAT=win32e lib ARCH=intel64 \
	  compiler=$(compiler) PRECISION=$(PRECISION)

lib: check
	if not exist $(obj_path) md $(obj_path)
	nmake $(objs) library \
	  compiler=$(compiler) PRECISION=$(PRECISION)

check:
!ifndef PLAT
	@echo Undefined platform: PLAT=$(PLAT). Set one of: win32 win32e
!endif
!if "$(IA)" == "ERROR"
	@echo Unknown platform: PLAT=$(PLAT). Set one of: win32 win32e
!endif

{wrappers}.c.obj:
	set include=$(MKLROOT:^"=)\include;$(MKLROOT:^"=)\include\fftw;%%include%%
	$(CC) $(SPEC_OPT) $(COPTS) /c /MT /D$(PRECISION) /Fo$(obj_path)\ $<

library:
	$(LIBR)"$(INSTALL_DIR:^"=)\$(INSTALL_LIBNAME)" $(obj_path)\*.$(sufobj)

clean: check
	-del /f/q "$(INSTALL_DIR:^"=)\$(INSTALL_LIBNAME)"
	-del /f/q $(obj_path)

clean_obj: check
	-del /f/q $(obj_path)

##-----------------------------------------------------------------------------

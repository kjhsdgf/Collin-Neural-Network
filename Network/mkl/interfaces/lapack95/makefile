#===============================================================================
# Copyright 2005-2017 Intel Corporation All Rights Reserved.
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

#  Content:
#      Intel(R) Math Kernel Library LAPACK95 library creation
#******************************************************************************

help:
	@echo Usage: nmake {libia32^|libintel64} install_dir=user_dir
	@echo             [interface=interface_name] [FC=compiler_name]
	@echo Intel(R) Fortran Compiler as default
	@echo install_dir    - library and the .mod files will be built and installed
	@echo                  in subdirectories of user_dir the same way as if
	@echo                  user_dir were the Intel(R) MKL installation directory.
	@echo interface_name - can be lp64 or ilp64 for intel64. Default value is lp64.
	@echo FC             - can be ifort or pgf95. Default value is ifort.

##------------------------------------------------------------------------------
## examples of using:
##
## nmake libia32 install_dir=lib95    - build  by Intel(R) Fortran Compiler  (as default)
##                                    for 32-bit applications, static linking
##
## nmake libintel64 install_dir=lib95 - build  by Intel(R) Fortran Compiler  (as default)
##                                    for Intel(R) 64 applications, static linking
##------------------------------------------------------------------------------

!include lapack95.lst

iface_95 = lapack.f90
iface_77 = lapack_interfaces.f90

!ifndef FC
FC = ifort
!endif

!IFNDEF interface
interface = lp64
!ENDIF

opts0    = /MT /Zl
optsc    = /c $(opts0)
sufobj   = obj
suflib   = lib
sufmod   = mod
src_path = source
LIBR     = lib -out:

!IFNDEF MKLROOT
MKLROOT = ..\..
!ENDIF

obj_path = "$(install_dir)"\lib\$(IA)\obj_lapack95_$(IA)_$(interface)
obj77_path = $(obj_path)\obj77

inc_path = $(MKLROOT)\include

objs = $(src_lapack95:+=.obj)
mod77 = $(iface_77:.f90=.mod)

FOPTS =

!if "$(PLAT)"=="win32"
IA = ia32
mod_path = $(install_dir)\include\ia32
mkl_lapack95_lib      = mkl_lapack95.$(suflib)

!else if "$(PLAT)"=="win32e"
IA = intel64
!  if ("$(interface)"=="ilp64")
!IF "$(FC)"=="pgf95"
FOPTS = -i8 -mp
!ELSE
FOPTS = /4I8 /4Ya
!ENDIF
mod_path = $(install_dir)\include\intel64\ilp64
mkl_lapack95_lib      = mkl_lapack95_ilp64.$(suflib)
!  else
mod_path = $(install_dir)\include\intel64\lp64
mkl_lapack95_lib      = mkl_lapack95_lp64.$(suflib)
!  endif

!else
IA = ERROR
!endif

.SUFFIXES:: .f90 .$(sufobj) .$(suflib) .$(sufmod)

libia32 lib32: checkdir
	nmake PLAT=win32 build interface=$(interface) install_dir="$(install_dir)" MKLROOT="$(MKLROOT:^"=)" FC=$(FC)
libintel64 libem64t: checkdir
	nmake PLAT=win32e build interface=$(interface) install_dir="$(install_dir)" MKLROOT="$(MKLROOT:^"=)" FC=$(FC)

build:	check lib_mod
lib_mod:	"$(install_dir)\lib\$(IA)\$(mkl_lapack95_lib)"

"$(install_dir)\lib\$(IA)\$(mkl_lapack95_lib)":
	if not exist $(obj77_path) md $(obj77_path)
!IF "$(FC)"=="pgf95"
	$(FC) $(FOPTS) -module $(obj77_path) -c -o$(obj_path)\lapack77.$(sufobj) "$(src_path)"\$(iface_77)
!ELSE
	$(FC) $(SPEC_OPT) $(FOPTS) /module:$(obj77_path) $(optsc) /Fo$(obj77_path)\ "$(src_path)"\$(iface_77)
!ENDIF
	nmake $(objs) interface=$(interface) install_dir="$(install_dir)"
	if not exist "$(mod_path)" md "$(mod_path)"
!IF "$(FC)"=="pgf95"
	$(FC) $(FOPTS) -module "$(mod_path)" -c -o$(obj_path)\lapack95.$(sufobj) "$(inc_path)\$(iface_95)"
!ELSE
	$(FC) $(SPEC_OPT) $(FOPTS) /module:"$(mod_path)" $(optsc) /Fo$(obj77_path)\ "$(inc_path)\$(iface_95)"
!ENDIF
	$(LIBR)"$(install_dir)"\lib\$(IA)\$(mkl_lapack95_lib) $(obj_path)\*.$(sufobj)
	-rmdir /s/q $(obj_path)

checkdir:
!ifndef install_dir
	@echo Not given install_dir
	@exit 1
!endif

check:
!ifndef PLAT
	@echo Undefined platform: PLAT=$(PLAT). Set one of: win32 win32e
!endif
!if "$(IA)"=="ERROR"
	@echo Unknown platform: PLAT=$(PLAT). Set one of: win32 win32e
!endif

{$(src_path)}.f90.$(sufobj):
	if not exist $(obj77_path) md $(obj77_path)
!IF "$(FC)"=="pgf95"
	$(FC) $(FOPTS) -module $(obj77_path) -c -o$(obj_path)\$@ $<
!ELSE
	$(FC) $(SPEC_OPT) $(FOPTS) /module:$(obj77_path) $(optsc) /Fo$(obj_path)\ $<
!ENDIF

clean: cleania32 cleanintel64
cleania32: checkdir
	-rmdir /s/q "$(install_dir)"\lib\ia32\obj_lapack95_ia32_lp64
	-del /f/q "$(install_dir)"\lib\ia32\mkl_lapack95.$(suflib)
cleanintel64: checkdir
	-rmdir /s/q "$(install_dir)"\lib\intel64\obj_lapack95_intel64_lp64 "$(install_dir)"\lib\intel64\obj_lapack95_intel64_ilp64
	-del /f/q "$(install_dir)"\lib\intel64\mkl_lapack95_lp64.$(suflib) "$(install_dir)"\lib\intel64\mkl_lapack95_ilp64.$(suflib)

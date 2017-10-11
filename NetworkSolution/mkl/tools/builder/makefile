#===============================================================================
# Copyright 2004-2017 Intel Corporation All Rights Reserved.
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

##  Content:
##      Intel(R) Math Kernel Library (Intel(R) MKL) Custom DLL builder
##
##******************************************************************************

help:
 	@nmake -q print_help PRIHE=YES

print_help:
!IF ("$(PRIHE)"=="YES")
!MESSAGE *
!MESSAGE * Custom DLL builder.
!MESSAGE * Usage: nmake <target> [<options>]
!MESSAGE *   target
!MESSAGE *     libia32    - for IA-32 architecture from static Intel(R) MKL libraries
!MESSAGE *     libintel64 - for Intel(R) 64 architecture from static Intel(R) MKL libraries
!MESSAGE *     dllia32    - for IA-32 architecture from dynamic Intel(R) MKL libraries
!MESSAGE *     dllintel64 - for Intel(R) 64 architecture from dynamic Intel(R) MKL libraries
!MESSAGE *     help       - prints this help
!MESSAGE *   <options>
!MESSAGE *     interface={cdecl|stdcall}
!MESSAGE *         Specifies programming interface for ia32.
!MESSAGE *         Default: cdecl.
!MESSAGE *     interface={lp64|ilp64}
!MESSAGE *         Specifies programming interface for intel64.
!MESSAGE *         Default: lp64.
!MESSAGE *     export=<file_name>
!MESSAGE *         The name of the file that contains the list of entry points
!MESSAGE *         to be included to the DLL.
!MESSAGE *         Default: user_example_list (no extension).
!MESSAGE *     name=<dll_name>
!MESSAGE *         The name of the created libraries.
!MESSAGE *         Default: mkl_custom.dll and mkl_custom.lib.
!MESSAGE *     xerbla=<err_handlr>
!MESSAGE *         The name of the object file that contains the user's error handler.
!MESSAGE *         By default native Intel(R) MKL XERBLA is used.
!MESSAGE *     threading={parallel|sequential}
!MESSAGE *         Specifies whether to use Intel(R) MKL in the threaded or sequential mode.
!MESSAGE *         Default: parallel.
!MESSAGE *     manifest={yes|no|embed}
!MESSAGE *         Manages the creation of MS manifest for Custom DLL.
!MESSAGE *         Default: manifest is not specified.
!MESSAGE *     crt=<c-runtime library name>
!MESSAGE *         Specifies the name of MS C-runtime library used to build the custom DLL.
!MESSAGE *         Default: msvcrt.lib.
!MESSAGE *     MKLROOT=<MKL_directory>
!MESSAGE *         Specifies the location of Intel(R) MKL libraries used to build the custom shared object.
!MESSAGE *         Default: the Intel(R) MKL installation directory.
!MESSAGE *
!ENDIF

##------------------------------------------------------------------------------
## Usage examples:
##
## nmake libia32
##      Creates Intel(R) MKL Custom DLL for IA-32 architecture.
##      Function list is taken from the predefined file user_example_list.
##      The static-parallel Intel(R) MKL libraries are used to build mkl_custom.dll.
##      No special xerbla is used.
##
## nmake dllintel64 export=my_blas_list interface=ilp64 name=my_blas
##      Creates my_blas.dll for Intel(R) 64 architecture.
##      Function list is taken from the user file my_blas_list.
##      The dynamic-parallel-ilp64 Intel(R) MKL libraries are used to build my_blas.dll.
##
##------------------------------------------------------------------------------

!if "$(MKLROOT)"==""
CB_MKLROOT=..\..
!else
CB_MKLROOT="$(MKLROOT)"
!endif

mklia32_libpath=$(CB_MKLROOT)\lib\ia32
mklintel64_libpath=$(CB_MKLROOT)\lib\intel64
compileria32_libpath=$(CB_MKLROOT)\..\compiler\lib\ia32
compilerintel64_libpath=$(CB_MKLROOT)\..\compiler\lib\intel64

!if "$(export)"==""
CB_EXPORT=user_example_list
!else
CB_EXPORT="$(export)"
!endif

!if "$(name)"==""
CB_NAME=mkl_custom
!else
CB_NAME="$(name)"
!endif

IFACE_LIB_32=mkl_intel_c.lib
IFACE_LIB_32_DLL=mkl_intel_c_dll.lib

!if "$(interface)"=="std" || "$(interface)"=="stdcall" || "$(interface)"=="STD" || "$(interface)"=="STDCALL"
IFACE_LIB_32=$(IFACE_LIB_32) mkl_intel_s.lib
IFACE_LIB_32_DLL=$(IFACE_LIB_32_DLL) mkl_intel_s_dll.lib
!endif

IF=lp64
!if "$(interface)"=="ilp64"
IF=ilp64
!endif

!ifndef threading
threading=parallel
!endif

!if "$(threading)"=="sequential"
THREADING_LIB=mkl_sequential.lib
THREADING_LIB_DLL=mkl_sequential_dll.lib
OMP_LIB_INTEL64=
OMP_LIB_IA32=
!else
THREADING_LIB=mkl_intel_thread.lib
THREADING_LIB_DLL=mkl_intel_thread_dll.lib
OMP_LIB_INTEL64=libiomp5md.lib
OMP_LIB_IA32=libiomp5md.lib
!endif

manifest = embed

!if "$(manifest)"=="embed"
MNFST_OPT=/MANIFEST
MT_CMD=mt.exe -nologo -manifest $(CB_NAME).dll.manifest -outputresource:$(CB_NAME).dll;2
!else
!if "$(manifest)"=="yes"
MNFST_OPT=/MANIFEST
MT_CMD=
!else
!if "$(manifest)"=="no"
MNFST_OPT=/MANIFEST:NO
MT_CMD=
!else
MNFST_OPT=/MANIFEST
MT_CMD=mt.exe -nologo -manifest $(CB_NAME).dll.manifest -outputresource:$(CB_NAME).dll;2
!endif
!endif
!endif

!if "$(xerbla)"==""
CB_XERBLA=
!else
CB_XERBLA="$(xerbla)"
!endif

!if "$(crt)"==""
CB_CRT=msvcrt.lib
I_MALLOC_OBJ=i_malloc_dll.obj
!else
CB_CRT="$(crt)"
I_MALLOC_OBJ=i_malloc_dll_static.obj
!endif

NODEFLIBS = /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:libmmt.lib /NODEFAULTLIB:libirc.lib /NODEFAULTLIB:svml_dispmt.lib /NODEFAULTLIB:libdecimal.lib

libintel64 intel64 em64t:
	set Lib=$(mklintel64_libpath);%%Lib%%;$(compilerintel64_libpath)
	@echo EXPORTS > user_def_file.def
	type $(CB_EXPORT) | findstr /V "^[;#]" >> user_def_file.def
	@if exist .\temp_cdll rd /q /s .\temp_cdll
	@md .\temp_cdll
	lib .\lib\mkl_cdll_intel64.lib /EXTRACT:"$(I_MALLOC_OBJ)" /OUT:".\temp_cdll\$(I_MALLOC_OBJ)"
	lib .\lib\mkl_cdll_intel64.lib /EXTRACT:_vml_dll_core.obj /OUT:.\temp_cdll\_vml_dll_core.obj
	link /DLL $(MNFST_OPT) /MACHINE:AMD64 $(NODEFLIBS) /def:user_def_file.def \
	.\temp_cdll\$(I_MALLOC_OBJ) .\temp_cdll\_vml_dll_core.obj $(CB_XERBLA) \
	mkl_intel_$(IF).lib $(THREADING_LIB) mkl_core.lib \
	$(OMP_LIB_INTEL64) \
	$(CB_CRT) kernel32.lib user32.lib /out:$(CB_NAME).dll
	@rd /q /s .\temp_cdll
	@del user_def_file.def
	$(MT_CMD)

libia32 ia32:
	set Lib=$(mklia32_libpath);%%Lib%%;$(compileria32_libpath)
	@echo EXPORTS > user_def_file.def
	type $(CB_EXPORT) | findstr /V "^[;#]" >> user_def_file.def
	@if exist .\temp_cdll rd /q /s .\temp_cdll
	@md .\temp_cdll
	@lib .\lib\mkl_cdll_ia32.lib /EXTRACT:"$(I_MALLOC_OBJ)" /OUT:".\temp_cdll\$(I_MALLOC_OBJ)"
	@lib .\lib\mkl_cdll_ia32.lib /EXTRACT:_vml_dll_core.obj /OUT:.\temp_cdll\_vml_dll_core.obj
	link /DLL $(MNFST_OPT) /MACHINE:IX86 $(NODEFLIBS) /def:user_def_file.def \
	.\temp_cdll\$(I_MALLOC_OBJ) .\temp_cdll\_vml_dll_core.obj $(CB_XERBLA) \
	$(IFACE_LIB_32) $(THREADING_LIB) mkl_core.lib \
	$(OMP_LIB_IA32) \
	$(CB_CRT) kernel32.lib user32.lib /out:$(CB_NAME).dll
	@rd /q /s .\temp_cdll
	@del user_def_file.def
	$(MT_CMD)

dllintel64:
	set Lib=$(mklintel64_libpath);%%Lib%%;$(compilerintel64_libpath)
	@echo EXPORTS > user_def_file.def
	@type $(CB_EXPORT) | findstr /V "^[;#]" >> user_def_file.def
	link /NOLOGO /DLL $(MNFST_OPT) /MACHINE:AMD64 /IGNORE:4001 /def:user_def_file.def \
	$(CB_XERBLA) \
	mkl_intel_$(IF)_dll.lib $(THREADING_LIB_DLL) \
	$(OMP_LIB_INTEL64) \
	$(CB_CRT) kernel32.lib user32.lib /out:$(CB_NAME).dll
	@del user_def_file.def
	@$(MT_CMD)

dllia32:
	set Lib=$(mklia32_libpath);%%Lib%%;$(compileria32_libpath)
	@echo EXPORTS > user_def_file.def
	type $(CB_EXPORT) | findstr /V "^[;#]" >> user_def_file.def
	link /DLL $(MNFST_OPT) /MACHINE:IX86 /IGNORE:4001 /def:user_def_file.def \
	$(CB_XERBLA) \
	$(IFACE_LIB_32_DLL) $(THREADING_LIB_DLL) \
	$(OMP_LIB_IA32) \
	$(CB_CRT) kernel32.lib user32.lib /out:$(CB_NAME).dll
	@del user_def_file.def
	$(MT_CMD)

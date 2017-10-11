@echo off
rem ============================================================================
rem Copyright 2003-2017 Intel Corporation All Rights Reserved.
rem
rem The source code,  information and material ("Material")  contained herein is
rem owned by Intel Corporation or its suppliers or licensors,  and title to such
rem Material remains with Intel Corporation or its suppliers  or licensors.  The
rem Material contains  proprietary  information  of Intel  or its  suppliers and
rem licensors.  The Material is protected by worldwide copyright laws and treaty
rem provisions.  No part  of the  Material   may be  used,  copied,  reproduced,
rem modified,  published,   uploaded,   posted,   transmitted,   distributed  or
rem disclosed in any way without Intel's prior  express written  permission.  No
rem license under any patent,  copyright  or other intellectual  property rights
rem in the Material is granted to or conferred upon you,  either  expressly,  by
rem implication,  inducement,  estoppel  or otherwise.  Any  license  under such
rem intellectual  property  rights must  be  express and  approved  by  Intel in
rem writing.
rem
rem Unless otherwise  agreed by  Intel in writing,  you may not  remove or alter
rem this notice or  any other notice  embedded in Materials by  Intel or Intel's
rem suppliers or licensors in any way.
rem ============================================================================

setlocal

:: Cache some environment variables.
set CPRO_PATH=%~dp0
call :GetFullPath "%CPRO_PATH%\..\.." CPRO_PATH
set MKLROOT=%CPRO_PATH%\mkl
set REDIST=%CPRO_PATH%\redist

set SCRIPT_NAME=%~nx0
set MOD_NAME=mod

:: Set the default arguments
set MKL_TARGET_ARCH=
set MKL_TARGET_ARCH_PATH=
set MKL_MIC_ARCH_PATH=intel64_win_mic
set MKL_LP64_ILP64=lp64
set MKL_MOD=

:ParseArgs
:: Parse the incoming arguments
if /i "%1"==""        goto Build
if /i "%1"=="ia32"       (set MKL_TARGET_ARCH=ia32)    & (set MKL_TARGET_ARCH_PATH=ia32_win)    & shift & goto ParseArgs
if /i "%1"=="intel64"    (set MKL_TARGET_ARCH=intel64) & (set MKL_TARGET_ARCH_PATH=intel64_win) & (set MKL_TARGET_LINUX_ARCH_PATH=intel64_lin)  & shift & goto ParseArgs
if /i "%1"=="none"                                       shift & goto ParseArgs
if /i "%1"=="vs2012"                                     shift & goto ParseArgs
if /i "%1"=="vs2013"                                     shift & goto ParseArgs
if /i "%1"=="vs2015"                                     shift & goto ParseArgs
if /i "%1"=="vs2017"                                     shift & goto ParseArgs
if /i "%1"=="lp64"       (set MKL_LP64_ILP64=lp64)     & shift & goto ParseArgs
if /i "%1"=="ilp64"      (set MKL_LP64_ILP64=ilp64)    & shift & goto ParseArgs
if /i "%1"=="%MOD_NAME%" (set MKL_MOD=%MOD_NAME%)      & shift & goto ParseArgs
call :Error %SCRIPT_NAME% %MOD_NAME% %1& goto Abort

:Build

:: target achitecture is mandatory
if /i "%MKL_TARGET_ARCH%"=="" call :Syntax %SCRIPT_NAME% %MOD_NAME% & goto Abort

:: main actions
set "LIB=%MKLROOT%\lib\%MKL_TARGET_ARCH_PATH%;%CPRO_PATH%\compiler\lib\%MKL_TARGET_ARCH_PATH%;%LIB%"

if exist "%REDIST%\%MKL_TARGET_ARCH_PATH%\compiler" (
    set "PATH=%REDIST%\%MKL_TARGET_ARCH_PATH%\compiler;%PATH%"
)

if exist "%REDIST%\%MKL_TARGET_ARCH_PATH%" (
    set "PATH=%REDIST%\%MKL_TARGET_ARCH_PATH%\mkl;%PATH%"
)

if /i "%MKL_MOD%"=="%MOD_NAME%" (
    if /i "%MKL_TARGET_ARCH%"=="intel64" (
        set "INCLUDE=%MKLROOT%\include\%MKL_TARGET_ARCH%\%MKL_LP64_ILP64%;%INCLUDE%"
    ) else (
        set "INCLUDE=%MKLROOT%\include\%MKL_TARGET_ARCH%;%INCLUDE%"
    )
)
set "INCLUDE=%MKLROOT%\include;%INCLUDE%"
set "CPATH=%MKLROOT%\include;%CPATH%"

if /i "%MKL_TARGET_ARCH%"=="intel64" (
    if exist "%MKLROOT%\lib\%MKL_MIC_ARCH_PATH%" (
        set "MIC_LD_LIBRARY_PATH=%MKLROOT%\lib\%MKL_MIC_ARCH_PATH%;%CPRO_PATH%\compiler\lib\%MKL_MIC_ARCH_PATH%;%MIC_LD_LIBRARY_PATH%"
        set "MIC_LIBRARY_PATH=%MKLROOT%\lib\%MKL_MIC_ARCH_PATH%;%CPRO_PATH%\compiler\lib\%MKL_MIC_ARCH_PATH%;%MIC_LIBRARY_PATH%"
    )
    if exist "%MKLROOT%\..\..\linux\mkl\lib\%MKL_TARGET_LINUX_ARCH_PATH%" (
        set "LD_LIBRARY_PATH=%MKLROOT%\..\..\linux\mkl\lib\%MKL_TARGET_LINUX_ARCH_PATH%;%LD_LIBRARY_PATH%"
    )
)


set "TBBLIBROOT=%CPRO_PATH%\tbb\lib"
if not defined TBBROOT (
    if exist "%TBBLIBROOT%" (
        set "LIB=%TBBLIBROOT%\%MKL_TARGET_ARCH_PATH%\vc_mt;%LIB%"
        if exist "%REDIST%\%MKL_TARGET_ARCH_PATH%\tbb" (
            set "PATH=%REDIST%\%MKL_TARGET_ARCH_PATH%\tbb\vc_mt;%PATH%"
        )

        if /i "%MKL_TARGET_ARCH%"=="intel64" (
            if exist "%TBBLIBROOT%\%MKL_MIC_ARCH_PATH%" (
                set "MIC_LIBRARY_PATH=%TBBLIBROOT%\%MKL_MIC_ARCH_PATH%;%MIC_LIBRARY_PATH%"
                set "MIC_LD_LIBRARY_PATH=%TBBLIBROOT%\%MKL_MIC_ARCH_PATH%;%MIC_LD_LIBRARY_PATH%"
            )
        )
    )

    if /i "%MKL_TARGET_ARCH%"=="intel64" (
        for %%G in (4.7 4.4 4.1) do (
            if exist "%MKLROOT%\..\..\linux\tbb\lib\%MKL_TARGET_LINUX_ARCH_PATH%\gcc%%G" (
                set "LD_LIBRARY_PATH=%MKLROOT%\..\..\linux\tbb\lib\%MKL_TARGET_LINUX_ARCH_PATH%\gcc%%G;%LD_LIBRARY_PATH%"
                goto EndTBBPath
            )
        )
    )
)
:EndTBBPath

:: pattern
::
:: setlocal
:: set LOCAL_VAR=val
:: set VAR=%LOCAL_VAR%
:: endlocal& ^
:: set VAR=%VAR%
::
:: allows to set value of VAR=val as global and do spoil LOCAL_VAR outside the pattern (if any such variable exists)
endlocal& ^
set MKLROOT=%MKLROOT%& ^
set PATH=%PATH%& ^
set LIB=%LIB%& ^
set INCLUDE=%INCLUDE%& ^
set CPATH=%CPATH%& ^
if /i "%MKL_TARGET_ARCH%"=="intel64" set LD_LIBRARY_PATH=%LD_LIBRARY_PATH%& ^
if /i "%MKL_TARGET_ARCH%"=="intel64" set MIC_LD_LIBRARY_PATH=%MIC_LD_LIBRARY_PATH%& ^
if /i "%MKL_TARGET_ARCH%"=="intel64" set MIC_LIBRARY_PATH=%MIC_LIBRARY_PATH% 

goto End

:Error
echo.
echo Invalid command line argument: %3

:Syntax
echo.
echo Syntax:
echo    %1 ^<arch^> ^[MKL_interface^] ^[Visual_Studio^] ^[%2^]
echo.
echo    ^<arch^> must be one of the following
echo        ia32         : Setup for IA-32 architecture
echo        intel64      : Setup for Intel(R) 64 architecture
echo.
echo    %2% (optional) - set path to Intel(R) MKL F95 modules
echo.
echo    MKL_interface (optional) - Intel(R) MKL programming interface for intel64
echo.                              Not applicable without %MOD_NAME%
echo        lp64         : 4 bytes integer (default)
echo        ilp64        : 8 bytes integer
echo.
echo    Visual_Studio (optional) - argument passed from compilersvars.
echo.                              Does Not affect this script.
echo        none  
echo        vs2012
echo        vs2013
echo        vs2015
echo        vs2017
exit /B 1

:GetFullPath
set %2=%~f1
goto End

:End
exit /B 0

:Abort
exit /B 1

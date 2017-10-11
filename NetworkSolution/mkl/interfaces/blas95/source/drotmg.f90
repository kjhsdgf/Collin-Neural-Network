!===============================================================================
! Copyright 2005-2017 Intel Corporation All Rights Reserved.
!
! The source code,  information  and material  ("Material") contained  herein is
! owned by Intel Corporation or its  suppliers or licensors,  and  title to such
! Material remains with Intel  Corporation or its  suppliers or  licensors.  The
! Material  contains  proprietary  information  of  Intel or  its suppliers  and
! licensors.  The Material is protected by  worldwide copyright  laws and treaty
! provisions.  No part  of  the  Material   may  be  used,  copied,  reproduced,
! modified, published,  uploaded, posted, transmitted,  distributed or disclosed
! in any way without Intel's prior express written permission.  No license under
! any patent,  copyright or other  intellectual property rights  in the Material
! is granted to  or  conferred  upon  you,  either   expressly,  by implication,
! inducement,  estoppel  or  otherwise.  Any  license   under such  intellectual
! property rights must be express and approved by Intel in writing.
!
! Unless otherwise agreed by Intel in writing,  you may not remove or alter this
! notice or  any  other  notice   embedded  in  Materials  by  Intel  or Intel's
! suppliers or licensors in any way.
!===============================================================================

!  Content:
!      F95 interface for BLAS routines
!*******************************************************************************
! This file was generated automatically!
!*******************************************************************************

PURE SUBROUTINE DROTMG_F95(D1,D2,X1,Y1,PARAM)
    ! Fortran77 call:
    ! DROTMG(D1,D2,X1,Y1,PARAM)
    ! <<< Use statements >>>
    USE F77_BLAS, ONLY: F77_ROTMG
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    REAL(WP), INTENT(INOUT) :: D1
    REAL(WP), INTENT(INOUT) :: D2
    REAL(WP), INTENT(INOUT) :: X1
    REAL(WP), INTENT(IN) :: Y1
    ! <<< Array arguments >>>
    REAL(WP), INTENT(OUT) :: PARAM(5)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'ROTMG'
    ! <<< Local scalars >>>
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    ! <<< Call blas77 routine >>>
    CALL F77_ROTMG(D1,D2,X1,Y1,PARAM)
END SUBROUTINE DROTMG_F95

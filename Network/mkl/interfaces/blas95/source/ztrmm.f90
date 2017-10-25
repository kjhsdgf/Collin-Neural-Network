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

PURE SUBROUTINE ZTRMM_F95(A,B,SIDE,UPLO,TRANSA,DIAG,ALPHA)
    ! Fortran77 call:
    ! ZTRMM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
    ! SIDE='L','R'; default: 'L'
    ! UPLO='U','L'; default: 'U'
    ! TRANSA='N','C','T'; default: 'N'
    ! DIAG='N','U'; default: 'N'
    ! Default ALPHA=1
    ! <<< Use statements >>>
    USE F77_BLAS, ONLY: F77_TRMM
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANSA
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIAG
    COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(IN) :: A(:,:)
    COMPLEX(WP), INTENT(INOUT) :: B(:,:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=4), PARAMETER :: SRNAME = 'TRMM'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_SIDE
    CHARACTER(LEN=1) :: O_UPLO
    CHARACTER(LEN=1) :: O_TRANSA
    CHARACTER(LEN=1) :: O_DIAG
    COMPLEX(WP) :: O_ALPHA
    INTEGER :: M
    INTEGER :: N
    INTEGER :: LDA
    INTEGER :: LDB
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(ALPHA)) THEN
        O_ALPHA = ALPHA
    ELSE
        O_ALPHA = 1
    ENDIF
    IF(PRESENT(DIAG)) THEN
        O_DIAG = DIAG
    ELSE
        O_DIAG = 'N'
    ENDIF
    IF(PRESENT(SIDE)) THEN
        O_SIDE = SIDE
    ELSE
        O_SIDE = 'L'
    ENDIF
    IF(PRESENT(TRANSA)) THEN
        O_TRANSA = TRANSA
    ELSE
        O_TRANSA = 'N'
    ENDIF
    IF(PRESENT(UPLO)) THEN
        O_UPLO = UPLO
    ELSE
        O_UPLO = 'U'
    ENDIF
    LDA = MAX(1,SIZE(A,1))
    LDB = MAX(1,SIZE(B,1))
    M = SIZE(B,1)
    N = SIZE(B,2)
    ! <<< Call blas77 routine >>>
    CALL F77_TRMM(O_SIDE,O_UPLO,O_TRANSA,O_DIAG,M,N,O_ALPHA,A,LDA,B,LDB)
END SUBROUTINE ZTRMM_F95

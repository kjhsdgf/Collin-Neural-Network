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

PURE SUBROUTINE DSYRK_F95(A,C,UPLO,TRANS,ALPHA,BETA)
    ! Fortran77 call:
    ! DSYRK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
    ! UPLO='U','L'; default: 'U'
    ! TRANS='N','C','T'; default: 'N'
    ! Default ALPHA=1
    ! Default BETA=0
    ! <<< Use statements >>>
    USE F77_BLAS, ONLY: F77_SYRK
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
    REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
    REAL(WP), INTENT(IN), OPTIONAL :: BETA
    ! <<< Array arguments >>>
    REAL(WP), INTENT(IN) :: A(:,:)
    REAL(WP), INTENT(INOUT) :: C(:,:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=4), PARAMETER :: SRNAME = 'SYRK'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_UPLO
    CHARACTER(LEN=1) :: O_TRANS
    REAL(WP) :: O_ALPHA
    REAL(WP) :: O_BETA
    INTEGER :: N
    INTEGER :: K
    INTEGER :: LDA
    INTEGER :: LDC
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(ALPHA)) THEN
        O_ALPHA = ALPHA
    ELSE
        O_ALPHA = 1
    ENDIF
    IF(PRESENT(BETA)) THEN
        O_BETA = BETA
    ELSE
        O_BETA = 0
    ENDIF
    IF(PRESENT(TRANS)) THEN
        O_TRANS = TRANS
    ELSE
        O_TRANS = 'N'
    ENDIF
    IF(PRESENT(UPLO)) THEN
        O_UPLO = UPLO
    ELSE
        O_UPLO = 'U'
    ENDIF
    IF((O_TRANS.EQ.'N'.OR.O_TRANS.EQ.'n')) THEN
        K = SIZE(A,2)
    ELSE
        K = SIZE(A,1)
    ENDIF
    LDA = MAX(1,SIZE(A,1))
    LDC = MAX(1,SIZE(C,1))
    N = SIZE(C,2)
    ! <<< Call blas77 routine >>>
    CALL F77_SYRK(O_UPLO,O_TRANS,N,K,O_ALPHA,A,LDA,O_BETA,C,LDC)
END SUBROUTINE DSYRK_F95

!===============================================================================
! Copyright 2010-2017 Intel Corporation All Rights Reserved.
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

PURE SUBROUTINE CGEM2VC_F95(A,X1,X2,Y1,Y2,ALPHA,BETA)
    ! Fortran77 call:
    ! CGEM2VC(M,N,ALPHA,A,LDA,X1,INCX1,X2,INCX2,BETA,Y1,INCY1,Y2,INCY2)
    ! Default ALPHA=1
    ! Default BETA=0
    ! <<< Use statements >>>
    USE F77_BLAS, ONLY: F77_GEM2V
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
    COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(IN) :: A(:,:)
    COMPLEX(WP), INTENT(IN) :: X1(:)
    COMPLEX(WP), INTENT(IN) :: X2(:)
    COMPLEX(WP), INTENT(INOUT) :: Y1(:)
    COMPLEX(WP), INTENT(INOUT) :: Y2(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'GEM2V'
    ! <<< Local scalars >>>
    COMPLEX(WP) :: O_ALPHA
    COMPLEX(WP) :: O_BETA
    INTEGER :: INCX1
    INTEGER :: INCX2
    INTEGER :: INCY1
    INTEGER :: INCY2
    INTEGER :: M
    INTEGER :: N
    INTEGER :: LDA
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
    INCX1 = 1
    INCX2 = 1
    INCY1 = 1
    INCY2 = 1
    LDA = MAX(1,SIZE(A,1))
    M = SIZE(A,1)
    N = SIZE(A,2)
    ! <<< Call blas77 routine >>>
    CALL F77_GEM2V(M,N,O_ALPHA,A,LDA,X1,INCX1,X2,INCX2,O_BETA,Y1,INCY1, &
     &                                                         Y2,INCY2)
END SUBROUTINE CGEM2VC_F95

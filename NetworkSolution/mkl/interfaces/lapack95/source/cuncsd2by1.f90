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
!      F95 interface for LAPACK routines
!*******************************************************************************
! This file was generated automatically!
!*******************************************************************************

PURE SUBROUTINE CUNCSD2BY1_F95(X11,X21,THETA,U1,U2,V1T,JOBU1,           &
     &                             JOBU2,JOBV1T,INFO)
    ! Fortran77 call:
    ! CUNCSD2BY1(JOBU1,JOBU2,JOBV1T,M,P,Q,X11,LDX11,
    !   X21,LDX21,THETA,U1,LDU1,U2,LDU2,V1T,LDV1T,
    !   WORK,LWORK,RWORK,LRWORK,IWORK,INFO)
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_UNCSD2BY1, F77_XERBLA
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBU1
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBU2
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBV1T
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(INOUT) :: X11(:,:)
    COMPLEX(WP), INTENT(INOUT) :: X21(:,:)
    REAL(WP), INTENT(OUT) :: THETA(:)
    COMPLEX(WP), INTENT(OUT) :: U1(:,:)
    COMPLEX(WP), INTENT(OUT) :: U2(:,:)
    COMPLEX(WP), INTENT(OUT) :: V1T(:,:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=9), PARAMETER :: SRNAME = 'UNCSD2BY1'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_JOBU1
    CHARACTER(LEN=1) :: O_JOBU2
    CHARACTER(LEN=1) :: O_JOBV1T
    INTEGER :: O_INFO
    INTEGER :: M
    INTEGER :: P
    INTEGER :: Q
    INTEGER :: LDU1
    INTEGER :: LDU2
    INTEGER :: LDV1T
    INTEGER :: LDX11
    INTEGER :: LDX21
    INTEGER :: LWORK
    INTEGER :: LRWORK
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    COMPLEX(WP), POINTER :: WORK(:)
    REAL(WP), POINTER :: RWORK(:)
    INTEGER, POINTER :: IWORK(:)
    ! <<< Arrays to request optimal sizes >>>
    REAL(WP) :: S_RWORK(1)
    COMPLEX(WP) :: S_WORK(1)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    !    Not inited: 1 scalars (special=2)
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(JOBU1)) THEN
        O_JOBU1 = JOBU1
    ELSE
        O_JOBU1 = 'Y'
    ENDIF
    IF(PRESENT(JOBU2)) THEN
        O_JOBU2 = JOBU2
    ELSE
        O_JOBU2 = 'Y'
    ENDIF
    IF(PRESENT(JOBV1T)) THEN
        O_JOBV1T = JOBV1T
    ELSE
        O_JOBV1T = 'Y'
    ENDIF
    LDU1 = MAX(1,SIZE(U1,1))
    LDU2 = MAX(1,SIZE(U2,1))
    LDV1T = MAX(1,SIZE(V1T,1))
    LDX11 = MAX(1,SIZE(X11,1))
    LDX21 = MAX(1,SIZE(X21,1))
    M = SIZE(U1,2) + SIZE(U2,2)
    P = SIZE(U1,2)
    Q = SIZE(V1T,2)
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    ALLOCATE(IWORK(M-MIN(P,MIN(M-P,MIN(Q,M-Q)))), STAT=L_STAT_ALLOC)
    ! <<< Request work array(s) size >>>
    LRWORK = -1
    LWORK = -1
    CALL F77_UNCSD2BY1(O_JOBU1,O_JOBU2,O_JOBV1T,M,                      &
     &P,Q,X11,LDX11,X21,LDX21,THETA,U1,LDU1,U2,LDU2,                    &
     &     V1T,LDV1T,S_WORK,LWORK,S_RWORK,LRWORK,IWORK,O_INFO)
    ! <<< Exit if error: bad parameters >>>
    IF(O_INFO /= 0) THEN
        GOTO 200
    ENDIF
    LRWORK = S_RWORK(1)
    LWORK = S_WORK(1)
    ! <<< Allocate work arrays with requested sizes >>>
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(RWORK(LRWORK), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(WORK(LWORK), STAT=L_STAT_ALLOC)
    ENDIF
    ! Error while build wrapper: CUNCSD2BY1
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_UNCSD2BY1(O_JOBU1,O_JOBU2,O_JOBV1T,                    &
     &  M,P,Q,X11,LDX11,X21,LDX21,THETA,U1,                             &
     &  LDU1,U2,LDU2,V1T,LDV1T,WORK,LWORK,RWORK,LRWORK,IWORK, O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Deallocate work arrays with requested sizes >>>
    DEALLOCATE(RWORK, STAT=L_STAT_DEALLOC)
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
200    CONTINUE
    ! <<< Deallocate local and work arrays >>>
    DEALLOCATE(IWORK, STAT=L_STAT_DEALLOC)
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE CUNCSD2BY1_F95

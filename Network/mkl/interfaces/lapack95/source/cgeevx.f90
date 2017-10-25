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

PURE SUBROUTINE CGEEVX_F95(A,W,VL,VR,BALANC,ILO,IHI,SCALE,ABNRM,RCONDE, &
     &                                                      RCONDV,INFO)
    ! Fortran77 call:
    ! CGEEVX(BALANC,JOBVL,JOBVR,SENSE,N,A,LDA,W,VL,LDVL,VR,LDVR,ILO,IHI,
    !   SCALE,ABNRM,RCONDE,RCONDV,WORK,LWORK,RWORK,INFO)
    ! BALANC='N','B','P','S'; default: 'N'
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_GEEVX, F77_XERBLA
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: BALANC
    INTEGER, INTENT(OUT), OPTIONAL :: ILO
    INTEGER, INTENT(OUT), OPTIONAL :: IHI
    REAL(WP), INTENT(OUT), OPTIONAL :: ABNRM
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(INOUT) :: A(:,:)
    COMPLEX(WP), INTENT(OUT) :: W(:)
    COMPLEX(WP), INTENT(OUT), OPTIONAL, TARGET :: VL(:,:)
    COMPLEX(WP), INTENT(OUT), OPTIONAL, TARGET :: VR(:,:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: SCALE(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: RCONDE(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: RCONDV(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'GEEVX'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_BALANC
    INTEGER :: O_ILO
    INTEGER :: O_IHI
    REAL(WP) :: O_ABNRM
    INTEGER :: O_INFO
    CHARACTER(LEN=1) :: JOBVL
    CHARACTER(LEN=1) :: JOBVR
    CHARACTER(LEN=1) :: SENSE
    INTEGER :: N
    INTEGER :: LDA
    INTEGER :: LDVL
    INTEGER :: LDVR
    INTEGER :: LWORK
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    COMPLEX(WP), POINTER :: O_VL(:,:)
    COMPLEX(WP), POINTER :: O_VR(:,:)
    REAL(WP), POINTER :: O_SCALE(:)
    REAL(WP), POINTER :: O_RCONDE(:)
    REAL(WP), POINTER :: O_RCONDV(:)
    COMPLEX(WP), POINTER :: WORK(:)
    REAL(WP), POINTER :: RWORK(:)
    ! <<< Arrays to request optimal sizes >>>
    COMPLEX(WP) :: S_WORK(1)
    ! <<< Stubs to "allocate" optional arrays >>>
    REAL(WP), TARGET :: L_A1_REAL(1)
    COMPLEX(WP), TARGET :: L_A2_COMP(1,1)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(BALANC)) THEN
        O_BALANC = BALANC
    ELSE
        O_BALANC = 'N'
    ENDIF
    IF(PRESENT(VL).OR.PRESENT(RCONDE)) THEN
        JOBVL = 'V'
    ELSE
        JOBVL = 'N'
    ENDIF
    IF(PRESENT(VR).OR.PRESENT(RCONDE)) THEN
        JOBVR = 'V'
    ELSE
        JOBVR = 'N'
    ENDIF
    LDA = MAX(1,SIZE(A,1))
    N = SIZE(A,2)
    IF(PRESENT(RCONDE).AND.PRESENT(RCONDV)) THEN
        SENSE = 'B'
    ELSEIF(PRESENT(RCONDE)) THEN
        SENSE = 'E'
    ELSEIF(PRESENT(RCONDV)) THEN
        SENSE = 'V'
    ELSE
        SENSE = 'N'
    ENDIF
    IF(PRESENT(VL)) THEN
        LDVL = MAX(1,SIZE(VL,1))
    ELSEIF(PRESENT(RCONDE)) THEN
        LDVL = N
    ELSE
        LDVL = 1
    ENDIF
    IF(PRESENT(VR)) THEN
        LDVR = MAX(1,SIZE(VR,1))
    ELSEIF(PRESENT(RCONDE)) THEN
        LDVR = N
    ELSE
        LDVR = 1
    ENDIF
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    IF(.NOT.PRESENT(RCONDE)) THEN
        IF(PRESENT(VL)) THEN
            O_VL => VL
        ELSE
            O_VL => L_A2_COMP
        ENDIF
    ELSE
        IF(PRESENT(VL)) THEN
            O_VL => VL
        ELSE
            ALLOCATE(O_VL(N,N), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(.NOT.PRESENT(RCONDE)) THEN
        IF(PRESENT(VR)) THEN
            O_VR => VR
        ELSE
            O_VR => L_A2_COMP
        ENDIF
    ELSE
        IF(L_STAT_ALLOC==0) THEN
            IF(PRESENT(VR)) THEN
                O_VR => VR
            ELSE
                ALLOCATE(O_VR(N,N), STAT=L_STAT_ALLOC)
            ENDIF
        ENDIF
    ENDIF
    IF(PRESENT(RCONDE)) THEN
        O_RCONDE => RCONDE
    ELSE
        O_RCONDE => L_A1_REAL
    ENDIF
    IF(PRESENT(RCONDV)) THEN
        O_RCONDV => RCONDV
    ELSE
        O_RCONDV => L_A1_REAL
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(SCALE)) THEN
            O_SCALE => SCALE
        ELSE
            ALLOCATE(O_SCALE(N), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(RWORK(2*N), STAT=L_STAT_ALLOC)
    ENDIF
    ! <<< Request work array(s) size >>>
    LWORK = -1
    CALL F77_GEEVX(O_BALANC,JOBVL,JOBVR,SENSE,N,A,LDA,W,O_VL,LDVL,O_VR, &
     & LDVR,O_ILO,O_IHI,O_SCALE,O_ABNRM,O_RCONDE,O_RCONDV,S_WORK,LWORK, &
     &                                                     RWORK,O_INFO)
    ! <<< Exit if error: bad parameters >>>
    IF(O_INFO /= 0) THEN
        GOTO 200
    ENDIF
    LWORK = S_WORK(1)
    ! <<< Allocate work arrays with requested sizes >>>
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(WORK(LWORK), STAT=L_STAT_ALLOC)
    ENDIF
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_GEEVX(O_BALANC,JOBVL,JOBVR,SENSE,N,A,LDA,W,O_VL,LDVL,  &
     &    O_VR,LDVR,O_ILO,O_IHI,O_SCALE,O_ABNRM,O_RCONDE,O_RCONDV,WORK, &
     &                                               LWORK,RWORK,O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Set output optional scalar parameters >>>
    IF(PRESENT(ABNRM)) THEN
        ABNRM = O_ABNRM
    ENDIF
    IF(PRESENT(IHI)) THEN
        IHI = O_IHI
    ENDIF
    IF(PRESENT(ILO)) THEN
        ILO = O_ILO
    ENDIF
    ! <<< Deallocate work arrays with requested sizes >>>
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
200    CONTINUE
    ! <<< Deallocate local and work arrays >>>
    IF(.NOT. PRESENT(SCALE)) THEN
        DEALLOCATE(O_SCALE, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(PRESENT(RCONDE) .AND..NOT. PRESENT(VL)) THEN
        DEALLOCATE(O_VL, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(PRESENT(RCONDE) .AND..NOT. PRESENT(VR)) THEN
        DEALLOCATE(O_VR, STAT=L_STAT_DEALLOC)
    ENDIF
    DEALLOCATE(RWORK, STAT=L_STAT_DEALLOC)
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE CGEEVX_F95

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

PURE SUBROUTINE CGEMM_BATCH_F95(A_ARRAY,B_ARRAY,C_ARRAY,M_ARRAY,N_ARRAY,&
     &                          K_ARRAY,GROUP_SIZE,TRANSA_ARRAY,        &
     &                          TRANSB_ARRAY,ALPHA_ARRAY,BETA_ARRAY)
    ! Fortran77 call:
    ! CGEMM_BATCH(TRANSA_ARRAY,TRANSB_ARRAY,M_ARRAY,N_ARRAY,K_ARRAY,
    !             ALPHA_ARRAY,A_ARRAY,LDA_ARRAY,B_ARRAY,LDB_ARRAY,
    !             BETA_ARRAY,C_ARRAY,LDC_ARRAY,GROUP_COUNT,GROUP_SIZE)
    ! TRANSA_ARRAY=Array where each element is one of 'N','C','T'; default: 'N'
    ! TRANSB_ARRAY=Array where each element is one of 'N','C','T'; default: 'N'
    ! ALPHA_ARRAY=Array of alpha values; default: array where each element=1
    ! BETA_ARRAY=Array of beta values; default: array where each element=0
    ! <<< Use statements >>>
    USE F77_BLAS, ONLY: F77_GEMM_BATCH, F77_XERBLA
    USE, INTRINSIC :: ISO_C_BINDING 
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Array arguments >>>
    INTEGER, INTENT(IN) :: M_ARRAY(:)
    INTEGER, INTENT(IN) :: N_ARRAY(:)
    INTEGER, INTENT(IN) :: K_ARRAY(:)
    INTEGER, INTENT(IN) :: GROUP_SIZE(:)
    ! TRANSA_ARRAY: INOUT intent instead of IN because PURE.
    CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL, TARGET :: TRANSA_ARRAY(:)
    ! TRANSB_ARRAY: INOUT intent instead of IN because PURE.
    CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL, TARGET :: TRANSB_ARRAY(:)
    ! ALPHA_ARRAY: INOUT intent instead of IN because PURE.
    COMPLEX(WP), INTENT(INOUT), OPTIONAL, TARGET :: ALPHA_ARRAY(:)
    ! BETA_ARRAY: INOUT intent instead of IN because PURE.
    COMPLEX(WP), INTENT(INOUT), OPTIONAL, TARGET :: BETA_ARRAY(:)
    INTEGER(KIND=C_SIZE_T), INTENT(IN) :: A_ARRAY(:)
    INTEGER(KIND=C_SIZE_T), INTENT(IN) :: B_ARRAY(:)
    INTEGER(KIND=C_SIZE_T), INTENT(INOUT) :: C_ARRAY(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=11), PARAMETER :: SRNAME = 'CGEMM_BATCH'
    ! <<< Local scalars >>>
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    INTEGER :: GROUP_COUNT
    INTEGER :: I
    ! <<< Local arrays >>>
    CHARACTER(LEN=1), POINTER :: O_TRANSA_ARRAY(:)
    CHARACTER(LEN=1), POINTER :: O_TRANSB_ARRAY(:)
    COMPLEX(WP), POINTER :: O_ALPHA_ARRAY(:)
    COMPLEX(WP), POINTER :: O_BETA_ARRAY(:)
    INTEGER, POINTER :: LDA_ARRAY(:)
    INTEGER, POINTER :: LDB_ARRAY(:)
    INTEGER, POINTER :: LDC_ARRAY(:)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init skipped scalars >>>
    GROUP_COUNT = SIZE(GROUP_SIZE)
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Init optional and skipped arrays >>>
    IF(PRESENT(ALPHA_ARRAY)) THEN
        O_ALPHA_ARRAY => ALPHA_ARRAY
    ELSE
        ALLOCATE(O_ALPHA_ARRAY(GROUP_COUNT), STAT=L_STAT_ALLOC)
        IF(L_STAT_ALLOC==0) THEN
            DO I=1, GROUP_COUNT
                O_ALPHA_ARRAY(I) = 1
            END DO
        ENDIF
    ENDIF
    IF(PRESENT(BETA_ARRAY)) THEN
        O_BETA_ARRAY => BETA_ARRAY
    ELSEIF(L_STAT_ALLOC==0) THEN
        ALLOCATE(O_BETA_ARRAY(GROUP_COUNT), STAT=L_STAT_ALLOC)
        IF(L_STAT_ALLOC==0) THEN
            DO I=1, GROUP_COUNT
                O_BETA_ARRAY(I) = 0
            END DO
        ENDIF
    ENDIF
    IF(PRESENT(TRANSA_ARRAY)) THEN
        O_TRANSA_ARRAY => TRANSA_ARRAY
    ELSEIF(L_STAT_ALLOC==0) THEN
        ALLOCATE(O_TRANSA_ARRAY(GROUP_COUNT), STAT=L_STAT_ALLOC)
        IF(L_STAT_ALLOC==0) THEN
            DO I=1, GROUP_COUNT
                O_TRANSA_ARRAY(I) = 'N'
            END DO
        ENDIF
    ENDIF
    IF(PRESENT(TRANSB_ARRAY)) THEN
        O_TRANSB_ARRAY => TRANSB_ARRAY
    ELSEIF(L_STAT_ALLOC==0) THEN
        ALLOCATE(O_TRANSB_ARRAY(GROUP_COUNT), STAT=L_STAT_ALLOC)
        IF(L_STAT_ALLOC==0) THEN
            DO I=1, GROUP_COUNT
                O_TRANSB_ARRAY(I) = 'N'
            END DO
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(LDA_ARRAY(GROUP_COUNT), STAT=L_STAT_ALLOC)
        IF(L_STAT_ALLOC==0) THEN
            DO I=1, GROUP_COUNT
                IF((O_TRANSA_ARRAY(I).EQ.'N' .OR.                       &
     &              O_TRANSA_ARRAY(I).EQ.'n')) THEN
                    LDA_ARRAY(I) = MAX(1,M_ARRAY(I))
                ELSE
                    LDA_ARRAY(I) = MAX(1,K_ARRAY(I))
                ENDIF
            END DO
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(LDB_ARRAY(GROUP_COUNT), STAT=L_STAT_ALLOC)
        IF(L_STAT_ALLOC==0) THEN
            DO I=1, GROUP_COUNT
                IF((O_TRANSB_ARRAY(I).EQ.'N' .OR.                       &
     &              O_TRANSB_ARRAY(I).EQ.'n')) THEN
                    LDB_ARRAY(I) = MAX(1,K_ARRAY(I))
                ELSE
                    LDB_ARRAY(I) = MAX(1,N_ARRAY(I))
                ENDIF
            END DO
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(LDC_ARRAY(GROUP_COUNT), STAT=L_STAT_ALLOC)
        IF(L_STAT_ALLOC==0) THEN
            DO I=1, GROUP_COUNT
                LDC_ARRAY(I) = MAX(1,M_ARRAY(I))
            END DO
        ENDIF
    ENDIF
    ! <<< Call blas77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_GEMM_BATCH(O_TRANSA_ARRAY,O_TRANSB_ARRAY,M_ARRAY,      &
     &                      N_ARRAY,K_ARRAY,O_ALPHA_ARRAY,A_ARRAY,      &
     &                      LDA_ARRAY,B_ARRAY,LDB_ARRAY,O_BETA_ARRAY,   &
     &                      C_ARRAY,LDC_ARRAY,GROUP_COUNT,GROUP_SIZE)
    ENDIF
    ! <<< Deallocate local arrays >>>
    IF(.NOT. PRESENT(ALPHA_ARRAY)) THEN
        IF (ASSOCIATED(O_ALPHA_ARRAY)) THEN
            DEALLOCATE(O_ALPHA_ARRAY, STAT=L_STAT_DEALLOC)
        ENDIF
    ENDIF
    IF(.NOT. PRESENT(BETA_ARRAY)) THEN
        IF (ASSOCIATED(O_BETA_ARRAY)) THEN
            DEALLOCATE(O_BETA_ARRAY, STAT=L_STAT_DEALLOC)
        ENDIF
    ENDIF
    IF(.NOT. PRESENT(TRANSA_ARRAY)) THEN
        IF (ASSOCIATED(O_TRANSA_ARRAY)) THEN
            DEALLOCATE(O_TRANSA_ARRAY, STAT=L_STAT_DEALLOC)
        ENDIF
    ENDIF
    IF(.NOT. PRESENT(TRANSB_ARRAY)) THEN
        IF (ASSOCIATED(O_TRANSB_ARRAY)) THEN
            DEALLOCATE(O_TRANSB_ARRAY, STAT=L_STAT_DEALLOC)
        ENDIF
    ENDIF
    IF (ASSOCIATED(LDA_ARRAY)) THEN
        DEALLOCATE(LDA_ARRAY, STAT=L_STAT_DEALLOC)
    ENDIF
    IF (ASSOCIATED(LDB_ARRAY)) THEN
        DEALLOCATE(LDB_ARRAY, STAT=L_STAT_DEALLOC)
    ENDIF
    IF (ASSOCIATED(LDC_ARRAY)) THEN
        DEALLOCATE(LDC_ARRAY, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(L_STAT_ALLOC .NE. 0) THEN
        CALL F77_XERBLA(SRNAME,1000)
    ENDIF
END SUBROUTINE CGEMM_BATCH_F95

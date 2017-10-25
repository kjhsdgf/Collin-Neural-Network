/*******************************************************************************
* Copyright 2006-2017 Intel Corporation All Rights Reserved.
*
* The source code,  information  and material  ("Material") contained  herein is
* owned by Intel Corporation or its  suppliers or licensors,  and  title to such
* Material remains with Intel  Corporation or its  suppliers or  licensors.  The
* Material  contains  proprietary  information  of  Intel or  its suppliers  and
* licensors.  The Material is protected by  worldwide copyright  laws and treaty
* provisions.  No part  of  the  Material   may  be  used,  copied,  reproduced,
* modified, published,  uploaded, posted, transmitted,  distributed or disclosed
* in any way without Intel's prior express written permission.  No license under
* any patent,  copyright or other  intellectual property rights  in the Material
* is granted to  or  conferred  upon  you,  either   expressly,  by implication,
* inducement,  estoppel  or  otherwise.  Any  license   under such  intellectual
* property rights must be express and approved by Intel in writing.
*
* Unless otherwise agreed by Intel in writing,  you may not remove or alter this
* notice or  any  other  notice   embedded  in  Materials  by  Intel  or Intel's
* suppliers or licensors in any way.
*******************************************************************************/

/*
 *
 * fftwnd_f77_create_plan - FFTW2 Fortran 77 wrapper to Intel(R) MKL.
 *
 ******************************************************************************
 */

#include "fftw.h"
#include "fftw2_mkl.h"
#include "fftw2_f77_mkl.h"

void
fftwnd_f77_create_plan(fftwnd_plan *plan_f77, int *rank, const int *n,
                       fftw_direction *dir, int *flags)
{
    int nn[DFTI_MAX_RANK];
    int i;

    if (plan_f77 == NULL) return;
    *(MKL_INT *)plan_f77 = 0;
    if (rank == NULL || n == NULL || dir == NULL || flags == NULL) return;

    for (i = 0; i < *rank; i++)
    {
        nn[i] = n[*rank - i - 1];
    }

    *plan_f77 = fftwnd_create_plan(*rank, nn, *dir, *flags);
}

/*******************************************************************************
* Copyright 2005-2017 Intel Corporation All Rights Reserved.
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
 * sfftw_plan_dft_r2c - FFTW3 Fortran 77 wrapper to Intel(R) MKL.
 *
 ******************************************************************************
 */

#include "fftw3_mkl_f77.h"

void
sfftw_plan_dft_r2c(PLAN *p, INTEGER *rank, INTEGER *n, REAL4 *in,
                   COMPLEX8 *out, INTEGER *flags)
{
    INTEGER zero = 0;
    INTEGER is[MKL_MAXRANK];
    INTEGER os[MKL_MAXRANK];
    int i, inplace;

    if (p == NULL || rank == NULL || n == NULL)
        return;

    if (*rank > MKL_MAXRANK)
    {
        *(MKL_INT64 *)p = 0;
        return;
    }

    inplace = ((void *)in == (void *)out);
    for (i = 0; i < *rank; i++)
    {
        if (i == 0)
        {
            is[i] = 1;
            os[i] = 1;
        }
        else if (i == 1)
        {
            os[i] = os[i - 1] * (n[i - 1] / 2 + 1);
            is[i] = inplace ? 2 * os[i] : is[i - 1] * n[i - 1];
        }
        else
        {
            is[i] = is[i - 1] * n[i - 1];
            os[i] = os[i - 1] * n[i - 1];
        }
    }

    sfftw_plan_guru_dft_r2c(p, rank, n, is, os, &zero, NULL, NULL, NULL, in,
                            out, flags);
}

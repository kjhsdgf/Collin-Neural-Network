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
 * dfftw_plan_dft - FFTW3 Fortran 77 wrapper to Intel(R) MKL.
 *
 ******************************************************************************
 */

#include "fftw3_mkl_f77.h"

void
dfftw_plan_dft(PLAN *p, INTEGER *rank, INTEGER *n, COMPLEX16 *in,
               COMPLEX16 *out, INTEGER *sign, INTEGER *flags)
{
    fftw_iodim64 dims64[MKL_MAXRANK];
    int i;

    if (p == NULL || rank == NULL || n == NULL || sign == NULL || flags == NULL)
        return;

    *(MKL_INT64 *)p = 0;
    if (*rank > MKL_MAXRANK) return;

    /* Reverse dimensions for column-major layout */
    for (i = 0; i < *rank; i++)
    {
        int j = *rank - i - 1;
        dims64[j].n = n[i];
        if (i == 0)
        {
            dims64[j].is = 1;
            dims64[j].os = 1;
        }
        else
        {
            dims64[j].is = dims64[j + 1].is * dims64[j + 1].n;
            dims64[j].os = dims64[j + 1].os * dims64[j + 1].n;
        }
    }

    *(fftw_plan *)p =
        fftw_plan_guru64_dft(*rank, dims64, 0, NULL, in, out, *sign, *flags);
}

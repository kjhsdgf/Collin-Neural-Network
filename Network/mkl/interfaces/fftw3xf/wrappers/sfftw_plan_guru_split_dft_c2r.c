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
 * sfftw_plan_guru_split_dft_c2r - FFTW3 Fortran 77 wrapper to Intel(R) MKL.
 *
 ******************************************************************************
 */

#include "fftw3_mkl_f77.h"

void
sfftw_plan_guru_split_dft_c2r(PLAN *p, INTEGER *rank, INTEGER *n, INTEGER *is,
                              INTEGER *os, INTEGER *howmany_rank,
                              INTEGER *howmany_n, INTEGER *howmany_is,
                              INTEGER *howmany_os, REAL4 *ri, REAL4 *ii,
                              REAL4 *out, INTEGER *flags)
{
    fftwf_iodim64 dims64[MKL_MAXRANK];
    fftwf_iodim64 howmany_dims64[MKL_ONE];
    int i;

    if (p == NULL || rank == NULL || n == NULL || flags == NULL)
        return;
    if (is == NULL || os == NULL) return;
    if (howmany_rank == NULL) return;
    if (*howmany_rank > 0 && (howmany_n == NULL || howmany_is == NULL || howmany_os == NULL)) return;

    *(MKL_INT64 *)p = 0;
    if (*rank > MKL_MAXRANK || *howmany_rank > MKL_ONE) return;

    for (i = 0; i < *rank; ++i)
    {
        dims64[i].n = n[*rank - i - 1];
        dims64[i].is = is[*rank - i - 1];
        dims64[i].os = os[*rank - i - 1];
    }
    for (i = 0; i < *howmany_rank; ++i)
    {
        howmany_dims64[i].n = howmany_n[*howmany_rank - i - 1];
        howmany_dims64[i].is = howmany_is[*howmany_rank - i - 1];
        howmany_dims64[i].os = howmany_os[*howmany_rank - i - 1];
    }
    *(fftwf_plan *)p =
        fftwf_plan_guru64_split_dft_c2r(*rank, dims64, *howmany_rank,
                                       howmany_dims64, ri, ii, out, *flags);
}

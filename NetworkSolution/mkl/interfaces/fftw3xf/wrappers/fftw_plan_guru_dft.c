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
 * fftw_plan_guru_dft - FFTW3 wrapper to Intel(R) MKL.
 *
 ******************************************************************************
 */

#include "fftw3_mkl.h"

fftw_plan
fftw_plan_guru_dft(int rank, const fftw_iodim *dims, int howmany_rank,
                   const fftw_iodim *howmany_dims, fftw_complex *in,
                   fftw_complex *out, int sign, unsigned flags)
{
    fftw_iodim64 dims64[MKL_MAXRANK];
    fftw_iodim64 howmany_dims64[MKL_ONE];
    int i;

    if (rank > MKL_MAXRANK || howmany_rank > MKL_ONE)
        return NULL;

    if (dims == NULL || (howmany_rank > 0 && howmany_dims == NULL))
        return NULL;

    for (i = 0; i < rank; ++i)
    {
        dims64[i].n = dims[i].n;
        dims64[i].is = dims[i].is;
        dims64[i].os = dims[i].os;
    }
    for (i = 0; i < howmany_rank; ++i)
    {
        howmany_dims64[i].n = howmany_dims[i].n;
        howmany_dims64[i].is = howmany_dims[i].is;
        howmany_dims64[i].os = howmany_dims[i].os;
    }

    return fftw_plan_guru64_dft(rank, dims64, howmany_rank, howmany_dims64, in,
                                out, sign, flags);
}

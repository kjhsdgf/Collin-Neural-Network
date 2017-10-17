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
 * fftw_plan_many_r2r - FFTW3 wrapper to Intel(R) MKL.
 *
 ******************************************************************************
 */

#include "fftw3_mkl.h"

fftw_plan
fftw_plan_many_r2r(int rank, const int *n, int howmany, double *in,
                   const int *inembed, int istride, int idist, double *out,
                   const int *onembed, int ostride, int odist,
                   const fftw_r2r_kind * kind, unsigned flags)
{
    fftw_iodim64 dims64[MKL_MAXRANK];
    fftw_iodim64 howmany64;
    int i;

    if (rank > MKL_MAXRANK)
        return NULL;

    if (n == NULL)
        return NULL;

    for (i = 0; i < rank; ++i)
    {
        dims64[i].n = n[i];
    }
    if (rank > 0)
    {
        dims64[rank - 1].is = istride;
        dims64[rank - 1].os = ostride;
    }
    if (!inembed)
        inembed = n;
    if (!onembed)
        onembed = n;
    for (i = rank - 1; i > 0; --i)
    {
        dims64[i - 1].is = dims64[i].is * inembed[i];
        dims64[i - 1].os = dims64[i].os * onembed[i];
    }

    howmany64.n = howmany;
    howmany64.is = idist;
    howmany64.os = odist;

    return fftw_plan_guru64_r2r(rank, dims64, 1, &howmany64, in, out, kind,
                                flags);
}

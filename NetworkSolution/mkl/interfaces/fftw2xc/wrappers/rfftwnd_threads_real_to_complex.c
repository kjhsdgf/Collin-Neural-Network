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
 * rfftwnd_threads_real_to_complex - FFTW2 wrapper to Intel(R) MKL.
 *
 ******************************************************************************
 */

#include "fftw2_mkl.h"

void
rfftwnd_threads_real_to_complex(int nthreads, rfftwnd_plan _plan, int howmany,
                                fftw_real *in, int istride, int idist,
                                fftw_complex *out, int ostride, int odist)
{
    MKL_LONG err = DFTI_NO_ERROR;
    fftw_plan_mkl *plan = (fftw_plan_mkl*)_plan;

    mkl_memory_layout layout = {istride, idist, ostride, odist, howmany, nthreads};
    DFTI_DESCRIPTOR_HANDLE mkl_desc = mkl_memory_layout_get(plan, &layout);
    if (mkl_desc == NULL) goto cannot_commit;

    err = DftiComputeForward(mkl_desc, in, out);
    mkl_memory_layout_recycle(plan, mkl_desc);
    if (err != DFTI_NO_ERROR) fftw_die("DftiComputeForward returned error "
            "in rfftwnd_threads_real_to_complex()");
    return;

cannot_commit:
    fftw_die("Cannot commit plan in rfftwnd_threads_real_to_complex()");
}

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
 * dfftw_plan_r2r_3d - FFTW3 Fortran 77 wrapper to Intel(R) MKL.
 *
 ******************************************************************************
 */

#include "fftw3_mkl_f77.h"

void
dfftw_plan_r2r_3d(PLAN *p, INTEGER *nx, INTEGER *ny, INTEGER *nz, REAL8 *in,
                  REAL8 *out, INTEGER *kindx, INTEGER *kindy, INTEGER *kindz,
                  INTEGER *flags)
{
    if (p == NULL) return;
    *p = 0;
    if(nx != NULL && ny != NULL && nz != NULL && kindx != NULL && kindy != NULL && kindz != NULL) {
        INTEGER three = 3;
        INTEGER n[3] = { *nx, *ny, *nz };
        INTEGER knd[3] = { *kindx, *kindy, *kindz };

        dfftw_plan_r2r(p, &three, n, in, out, knd, flags);
    }
}

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
 * fftw_plan_r2r_3d - FFTW3 wrapper to Intel(R) MKL.
 *
 ******************************************************************************
 */

#include "fftw3_mkl.h"

fftw_plan
fftw_plan_r2r_3d(int nx, int ny, int nz, double *in, double *out,
                 fftw_r2r_kind kindx, fftw_r2r_kind kindy, fftw_r2r_kind kindz,
                 unsigned flags)
{
    int n[3] = { nx, ny, nz };
    fftw_r2r_kind kind[3] = { kindx, kindy, kindz };

    return fftw_plan_r2r(3, n, in, out, kind, flags);
}

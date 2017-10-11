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
 * fftwf_execute_split_dft - FFTW3 wrapper to Intel(R) MKL.
 *
 ******************************************************************************
 */

#include "fftw3_mkl.h"

void
fftwf_execute_split_dft(const fftwf_plan plan, float *ri, float *ii,
                       float *ro, float *io)
{

    fftw_mkl_plan mkl_plan = (fftw_mkl_plan)plan;
    struct fftw_mkl_plan_s tmp_plan_s;

    if (!(mkl_plan && mkl_plan->execute)) return;

    if (ri == NULL || ii == NULL || ro == NULL || io == NULL) return;

    tmp_plan_s = mkl_plan[0];
    tmp_plan_s.io[0] = ri;
    tmp_plan_s.io[1] = ii;
    tmp_plan_s.io[2] = ro;
    tmp_plan_s.io[3] = io;

    tmp_plan_s.execute(&tmp_plan_s);

}

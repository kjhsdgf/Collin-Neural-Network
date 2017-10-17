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
 * fftw_version etc - FFTW3 wrapper to Intel(R) MKL.
 *
 ******************************************************************************
 */

#include "fftw3_mkl.h"

const char fftw_version[] = "FFTW 3.3.4 wrappers to Intel(R) MKL";
const char fftw_cc[] = "";
const char fftw_codelet_optim[] = "";

/* Supplementary definitions for the wrappers */

static void
delete_plan(fftw_mkl_plan p)
{
    if (p)
    {
        if (p->spar)
            fftw_free(p->spar);
        if (p->dpar)
            fftw_free(p->dpar);
        if (p->ipar)
            fftw_free(p->ipar);
        if (p->desc)
            DftiFreeDescriptor(&p->desc);
        fftw_free(p);
    }
}

static fftw_mkl_plan
new_plan(void)
{
    fftw_mkl_plan p = (fftw_mkl_plan)fftw_malloc(sizeof(*p));

    if (p)
    {
        p->desc = NULL;
        p->io[0] = p->io[1] = p->io[2] = p->io[3] = 0;
        p->ipar = NULL;
        p->dpar = NULL;
        p->spar = NULL;
        p->execute = NULL;
        p->destroy = delete_plan;
        p->mpi_plan = NULL;
    }
    return p;
}

#ifndef MKL_FFTW_MALLOC_ALIGNMENT
#define MKL_FFTW_MALLOC_ALIGNMENT (64)
#endif

fftw3_mkl_s fftw3_mkl = {
    0,  /* verbose */
    1,  /* nthreads */
    0.0,        /* timelimit */
    1,  /* Number of user threads variable. Will be depricated in nearest future */
    new_plan,
    MKL_FFTW_MALLOC_ALIGNMENT /* default_alignment */
};

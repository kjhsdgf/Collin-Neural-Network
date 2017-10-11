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
 * Definitions for FFTW2 Fortran wrappers to Intel(R) MKL.
 *
 ******************************************************************************
 */

#ifndef FFTW2_F77_MKL_H
#define FFTW2_F77_MKL_H

#if defined(_WIN32)

#define fftw_f77_create_plan FFTW_F77_CREATE_PLAN
#define fftw_f77_one FFTW_F77_ONE
#define fftw_f77 FFTW_F77
#define fftw_f77_destroy_plan FFTW_F77_DESTROY_PLAN
#define fftwnd_f77_create_plan FFTWND_F77_CREATE_PLAN
#define fftw2d_f77_create_plan FFTW2D_F77_CREATE_PLAN
#define fftw3d_f77_create_plan FFTW3D_F77_CREATE_PLAN
#define fftwnd_f77 FFTWND_F77
#define fftwnd_f77_one FFTWND_F77_ONE
#define fftwnd_f77_destroy_plan FFTWND_F77_DESTROY_PLAN
#define fftw_f77_threads_one FFTW_F77_THREADS_ONE
#define fftw_f77_threads FFTW_F77_THREADS
#define fftwnd_f77_threads FFTWND_F77_THREADS
#define fftwnd_f77_threads_one FFTWND_F77_THREADS_ONE
#define fftw_f77_threads_init FFTW_F77_THREADS_INIT

#define rfftw_f77_create_plan RFFTW_F77_CREATE_PLAN
#define rfftw_f77_one RFFTW_F77_ONE
#define rfftw_f77 RFFTW_F77
#define rfftw_f77_destroy_plan RFFTW_F77_DESTROY_PLAN
#define rfftwnd_f77_create_plan RFFTWND_F77_CREATE_PLAN
#define rfftw2d_f77_create_plan RFFTW2D_F77_CREATE_PLAN
#define rfftw3d_f77_create_plan RFFTW3D_F77_CREATE_PLAN
#define rfftwnd_f77_real_to_complex RFFTWND_F77_REAL_TO_COMPLEX
#define rfftwnd_f77_one_real_to_complex RFFTWND_F77_ONE_REAL_TO_COMPLEX
#define rfftwnd_f77_complex_to_real RFFTWND_F77_COMPLEX_TO_REAL
#define rfftwnd_f77_one_complex_to_real RFFTWND_F77_ONE_COMPLEX_TO_REAL
#define rfftwnd_f77_destroy_plan RFFTWND_F77_DESTROY_PLAN

#define fftw_f77_threads_one FFTW_F77_THREADS_ONE
#define fftw_f77_threads FFTW_F77_THREADS
#define fftwnd_f77_threads FFTWND_F77_THREADS
#define fftwnd_f77_threads_one FFTWND_F77_THREADS_ONE
#define rfftwnd_f77_threads_real_to_complex RFFTWND_F77_THREADS_REAL_TO_COMPLEX
#define rfftwnd_f77_threads_one_real_to_complex RFFTWND_F77_THREADS_ONE_REAL_TO_COMPLEX
#define rfftwnd_f77_threads_complex_to_real RFFTWND_F77_THREADS_COMPLEX_TO_REAL
#define rfftwnd_f77_threads_one_complex_to_real RFFTWND_F77_THREADS_ONE_COMPLEX_TO_REAL

#else

#ifdef _GNU
#define N(n) n ## __
#else
#define N(n) n ## _
#endif

#define fftw_f77_create_plan N(fftw_f77_create_plan)
#define fftw_f77_one N(fftw_f77_one)
#define fftw_f77 N(fftw_f77)
#define fftw_f77_destroy_plan N(fftw_f77_destroy_plan)
#define fftwnd_f77_create_plan N(fftwnd_f77_create_plan)
#define fftw2d_f77_create_plan N(fftw2d_f77_create_plan)
#define fftw3d_f77_create_plan N(fftw3d_f77_create_plan)
#define fftwnd_f77 N(fftwnd_f77)
#define fftwnd_f77_one N(fftwnd_f77_one)
#define fftwnd_f77_destroy_plan N(fftwnd_f77_destroy_plan)
#define fftw_f77_threads_one N(fftw_f77_threads_one)
#define fftw_f77_threads N(fftw_f77_threads)
#define fftwnd_f77_threads N(fftwnd_f77_threads)
#define fftwnd_f77_threads_one N(fftwnd_f77_threads_one)
#define fftw_f77_threads_init N(fftw_f77_threads_init)

#define rfftw_f77_create_plan N(rfftw_f77_create_plan)
#define rfftw_f77_one N(rfftw_f77_one)
#define rfftw_f77 N(rfftw_f77)
#define rfftw_f77_destroy_plan N(rfftw_f77_destroy_plan)
#define rfftwnd_f77_create_plan N(rfftwnd_f77_create_plan)
#define rfftw2d_f77_create_plan N(rfftw2d_f77_create_plan)
#define rfftw3d_f77_create_plan N(rfftw3d_f77_create_plan)
#define rfftwnd_f77_real_to_complex N(rfftwnd_f77_real_to_complex)
#define rfftwnd_f77_one_real_to_complex N(rfftwnd_f77_one_real_to_complex)
#define rfftwnd_f77_complex_to_real N(rfftwnd_f77_complex_to_real)
#define rfftwnd_f77_one_complex_to_real N(rfftwnd_f77_one_complex_to_real)
#define rfftwnd_f77_destroy_plan N(rfftwnd_f77_destroy_plan)

#define fftw_f77_threads_one N(fftw_f77_threads_one)
#define fftw_f77_threads N(fftw_f77_threads)
#define fftwnd_f77_threads N(fftwnd_f77_threads)
#define fftwnd_f77_threads_one N(fftwnd_f77_threads_one)
#define rfftwnd_f77_threads_real_to_complex N(rfftwnd_f77_threads_real_to_complex)
#define rfftwnd_f77_threads_one_real_to_complex N(rfftwnd_f77_threads_one_real_to_complex)
#define rfftwnd_f77_threads_complex_to_real N(rfftwnd_f77_threads_complex_to_real)
#define rfftwnd_f77_threads_one_complex_to_real N(rfftwnd_f77_threads_one_complex_to_real)

#endif

void fftw_f77(fftw_plan *plan, int *phowmany, fftw_complex *in, int *pistride, int *pidist, fftw_complex *out, int *postride, int *podist);
void fftw_f77_create_plan(fftw_plan *plan_f77, int *n, fftw_direction *dir, int *flags);
void fftw_f77_destroy_plan(fftw_plan *plan_f77);
void fftw_f77_one(fftw_plan *plan_f77, fftw_complex *in, fftw_complex *out);
void fftw_f77_threads(int *pnthreads, fftw_plan *plan, int *phowmany, fftw_complex *in, int *pistride, int *pidist, fftw_complex *out, int *postride, int *podist);
void fftw_f77_threads_init(int *thread_mode);
void fftw_f77_threads_one(int *nthreads, fftw_plan *plan, fftw_complex *in, fftw_complex *out);
void fftw2d_f77_create_plan(fftwnd_plan *plan_f77, int *nx, int *ny, fftw_direction *dir, int *flags);
void fftw3d_f77_create_plan(fftwnd_plan *plan_f77, int *nx, int *ny, int *nz, fftw_direction *dir, int *flags);
void fftwnd_f77(fftwnd_plan *plan, int *howmany, fftw_complex *in, int *istride, int *idist, fftw_complex *out, int *ostride, int *odist);
void fftwnd_f77_create_plan(fftwnd_plan *plan_f77, int *rank, const int *n, fftw_direction *dir, int *flags);
void fftwnd_f77_destroy_plan(fftwnd_plan *plan_f77);
void fftwnd_f77_one(fftwnd_plan *plan_f77, fftw_complex *in, fftw_complex *out);
void fftwnd_f77_threads(int *nthreads, fftwnd_plan *plan, int *howmany, fftw_complex *in, int *istride, int *idist, fftw_complex *out, int *ostride, int *odist);
void fftwnd_f77_threads_one(int *nthreads, fftwnd_plan *plan, fftw_complex *in, fftw_complex *out);
void rfftw_f77(rfftw_plan * plan, int *howmany, fftw_real *in, int *istride, int *idist, fftw_real *out, int *ostride, int *odist);
void rfftw_f77_create_plan(rfftw_plan * plan, int *n, fftw_direction *dir, int *flags);
void rfftw_f77_destroy_plan(rfftw_plan * plan);
void rfftw_f77_one(rfftw_plan * plan, fftw_real *in, fftw_real *out);
void rfftw_f77_threads(int *nthreads, rfftw_plan * plan, int *howmany, fftw_real *in, int *istride, int *idist, fftw_real *out, int *ostride, int *odist);
void rfftw_f77_threads_one(int *nthreads, rfftw_plan * plan, fftw_real *in, fftw_real *out);
void rfftw2d_f77_create_plan(fftwnd_plan *plan, int *nx, int *ny, fftw_direction *dir, int *flags);
void rfftw3d_f77_create_plan(rfftwnd_plan *plan, int *nx, int *ny, int *nz, fftw_direction *dir, int *flags);
void rfftwnd_f77_complex_to_real(rfftwnd_plan *plan, int *howmany, fftw_complex *in, int *istride, int *idist, fftw_real *out, int *ostride, int *odist);
void rfftwnd_f77_create_plan(rfftwnd_plan *plan_f77, int *rank, const int *n, fftw_direction *dir, int *flags);
void rfftwnd_f77_destroy_plan(rfftwnd_plan *plan_f77);
void rfftwnd_f77_one_complex_to_real(rfftwnd_plan *plan_f77, fftw_complex *in, fftw_real *out);
void rfftwnd_f77_one_real_to_complex(rfftwnd_plan *plan, fftw_real *in, fftw_complex *out);
void rfftwnd_f77_real_to_complex(rfftwnd_plan *plan, int *howmany, fftw_real *in, int *istride, int *idist, fftw_complex *out, int *ostride, int *odist);
void rfftwnd_f77_threads_complex_to_real(int *nthreads, rfftwnd_plan *plan, int *howmany, fftw_complex *in, int *istride, int *idist, fftw_real *out, int *ostride, int *odist);
void rfftwnd_f77_threads_one_complex_to_real(int *nthreads, rfftwnd_plan *plan, fftw_complex *in, fftw_real *out);
void rfftwnd_f77_threads_one_real_to_complex(int *nthreads, rfftwnd_plan *plan, fftw_real *in, fftw_complex *out);
void rfftwnd_f77_threads_real_to_complex(int *nthreads, rfftwnd_plan *plan, int *howmany, fftw_real *in, int *istride, int *idist, fftw_complex *out, int *ostride, int *odist);

#endif /* FFTW2_F77_MKL_H */

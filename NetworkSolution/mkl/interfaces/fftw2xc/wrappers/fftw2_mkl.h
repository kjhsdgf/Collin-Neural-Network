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
 * Definitions for FFTW2 wrappers to Intel(R) MKL.
 *
 ******************************************************************************
 */

#ifndef FFTW2_MKL_H
#define FFTW2_MKL_H

#include <stdlib.h>
#include <stdio.h>
#include "mkl_dfti.h"
#include "mkl_service.h"
#include "fftw_threads.h"
#include "rfftw_threads.h"
#if defined __APPLE__

#include <AvailabilityMacros.h>

#if ( MAC_OS_X_VERSION_MAX_ALLOWED >= 101200 && __STDC_VERSION__ >= 201112L && !defined(__STDC_NO_ATOMICS__) )
#   define DEPRECATED_OSAtomic
#   include <stdatomic.h>
#else
#   include <libkern/OSAtomic.h>
#endif

#endif

#define DFTI_MAX_RANK 7

#ifdef MKL_DOUBLE
    #define TYPE_PRECISION double
    #define MKL_PRECISION DFTI_DOUBLE
#else
    #define TYPE_PRECISION float
    #define MKL_PRECISION DFTI_SINGLE
    #define FFTW_ENABLE_FLOAT 1
#endif

typedef struct
{
    MKL_LONG istride, idist;
    MKL_LONG ostride, odist;
    MKL_LONG howmany;
    int nthreads;
} mkl_memory_layout;

/// Maximum number of memory layouts that can be cached for same plan
#define MKL_MEMORY_LAYOUT_CACHE_MAX (16)

typedef struct fftw_plan_mkl
{
    DFTI_DESCRIPTOR_HANDLE mkl_desc;
    int sign;
    int inplace;
    int readonly;
    int rank;
    MKL_LONG istride, idist;
    MKL_LONG ostride, odist;
    MKL_LONG *n;
    MKL_LONG howmany;
    int nthreads;
    struct {
        DFTI_DESCRIPTOR_HANDLE mkl_desc;
        mkl_memory_layout layout;
    } cache[MKL_MEMORY_LAYOUT_CACHE_MAX];
    volatile unsigned int cache_lock;
} fftw_plan_mkl;

#ifndef UNUSED
#define UNUSED(p) (void)p
#endif

/// Get DFTI descriptor handle for memory layout
/// If return value is NULL, creation of handle failed
/// To recycle handle returned by mkl_memory_layout_get,
/// call mkl_memory_layout_recycle
DFTI_DESCRIPTOR_HANDLE mkl_memory_layout_get(fftw_plan_mkl *P, const mkl_memory_layout *L);

/// Recycle DFTI descriptor handle returned by mkl_memory_layout_get
void mkl_memory_layout_recycle(fftw_plan_mkl *P, DFTI_DESCRIPTOR_HANDLE H);

/// Initialize list of memory layouts
void mkl_memory_layout_init(fftw_plan_mkl *P);

/// Free list of memory layouts
void mkl_memory_layout_free(fftw_plan_mkl *P);


#endif /* FFTW2_MKL_H */

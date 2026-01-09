/*-
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2021 Hesham Almatary
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <stdio.h>
#include "FreeRTOS.h"
/* IP stack includes. */
#include "FreeRTOS_IP.h"
#include "FreeRTOS_Sockets.h"

#include <dlfcn.h>
#include <rtl/rtl-freertos-compartments.h>

void CheriFreeRTOS_FaultHandler(void* pvParameter1, uint32_t comp_id) {

#if configCHERI_COMPARTMENTALIZATION
  rtl_cherifreertos_compartment_revoke_resources(comp_id);
#endif

  void* obj = dlopen("FreeRTOS_IP.c.8.o", RTLD_GLOBAL | RTLD_NOW);
  if (obj != NULL) {
      BaseType_t* xIPTaskInitialised= dlsym( obj, "xIPTaskInitialised" );
      if (xIPTaskInitialised) {
        *xIPTaskInitialised = pdFALSE;
      }

      BaseType_t* xNetworkUp = dlsym( obj, "xNetworkUp" );
      if (xNetworkUp) {
        *xNetworkUp = pdFALSE;
      }

      void** xNetworkEventQueue = dlsym( obj, "xNetworkEventQueue" );
      if (xNetworkEventQueue) {
        *xNetworkEventQueue = NULL;
      }

      TaskHandle_t* xIPTaskHandle= dlsym( obj, "xIPTaskHandle" );
      if (xIPTaskHandle) {
        *xIPTaskHandle = NULL;
      }

      BaseType_t* xCallEventHook= dlsym( obj, "xCallEventHook" );
      if (xCallEventHook) {
        *xCallEventHook = pdFALSE;
      }
  }

  obj = dlopen("BufferAllocation_2.c.8.o", RTLD_GLOBAL | RTLD_NOW);
  if (obj != NULL) {

      void** xNetworkBufferSemaphore = dlsym( obj, "xNetworkBufferSemaphore" );
      if (xNetworkBufferSemaphore) {
        *xNetworkBufferSemaphore = NULL;
      }
  }

  // Tell the application
  vApplicationIPNetworkEventHook( eNetworkDown );
}

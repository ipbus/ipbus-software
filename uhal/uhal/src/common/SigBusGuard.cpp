/*
---------------------------------------------------------------------------

    This file is part of uHAL.

    uHAL is a hardware access library and programming framework
    originally developed for upgrades of the Level-1 trigger of the CMS
    experiment at CERN.

    uHAL is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    uHAL is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with uHAL.  If not, see <http://www.gnu.org/licenses/>.


      Tom Williams, Rutherford Appleton Laboratory
      email: tom.williams <AT> cern.ch

      Dan Gastler, Boston University
      email: dgastler <AT> bu.edu

---------------------------------------------------------------------------
*/

#include "uhal/SigBusGuard.hpp"


#include <string.h>

#include "uhal/log/log.hpp"


namespace uhal {

  std::mutex SigBusGuard::sMutex;
  sigjmp_buf SigBusGuard::sEnv;
  volatile sig_atomic_t SigBusGuard::sProtected = 0;

  SigBusGuard::SigBusGuard() :
    mLockGuard(sMutex)
  {
    // 1) Register our signal handler for SIGBUS, saving original in mOriginalAction
    log(Debug(), "Registering uHAL SIGBUS handler");
    mAction.sa_handler = SigBusGuard::handle;
    sigemptyset(&mAction.sa_mask);
    if (sigaction(SIGBUS, &mAction, &mOriginalAction) != 0) {
      exception::SignalHandlerNotRegistered lExc;
      log(lExc, "Failed to register SIGBUS handler (in SigBusGuard constructor); errno=", Integer(errno), ", meaning ", Quote (strerror(errno)));
      throw lExc;
    }

    // 2) Update this thread's signal mask to unblock SIGBUS (and throw if already unblocked)
    sigset_t lMaskedSignals;
    sigfillset(&lMaskedSignals);
    sigdelset(&lMaskedSignals, SIGKILL); // Unblockable
    sigdelset(&lMaskedSignals, SIGSTOP); // Unblockable
    sigdelset(&lMaskedSignals, SIGINT); // Ctrl+C
    sigdelset(&lMaskedSignals, SIGBUS);
    const int lErrNo = pthread_sigmask(SIG_SETMASK, &lMaskedSignals, &mOriginalMask);
    if (lErrNo != 0) {
      exception::SignalMaskingFailure lExc;
      log(lExc, "Failed to update signal mask in SigBusGuard constructor; errno=", Integer(lErrNo), ", meaning ", Quote (strerror(lErrNo)));
      throw lExc;
    }
    if (sigismember(&mOriginalMask, SIGBUS) != 1) {
      exception::SignalNotBlocked lExc;
      log(lExc, "SIGBUS must be blocked (by all threads) before using SigBusGuard");
      throw lExc;
    }
  }


  SigBusGuard::~SigBusGuard()
  {
    // 1) Restore the original signal handler for SIGBUS
    if (sigaction(SIGBUS, &mOriginalAction, NULL) != 0)
      log(Error(), "Failed to re-register old SIGBUS handler (in SigBusGuard destructor); errno=", Integer(errno), ", meaning ", Quote (strerror(errno)));
    else
      log(Debug(), "Restored original SIGBUS handler");

    // 2) Update this thread's signal mask to block SIGBUS again
    const int lErrNo = pthread_sigmask(SIG_SETMASK, &mOriginalMask, NULL);
    if (lErrNo != 0)
      log(Error(), "Failed to update signal mask in SigBusGuard constructor; errno=", Integer(lErrNo), ", meaning ", Quote (strerror(lErrNo)));
  }


  void SigBusGuard::protect(const std::function<void()>& aAccess, const std::string& aMessage)
  {
    sProtected = 1;

    // First time sigsetjmp is called it just stores the context of where it is called
    // and returns 0. If a signal is received and siglongjmp is called in the handler,
    // then the thread will return here and sigsetjmp will then return that signal
    // NOTE: HW access must wrapped in a function and invoked in this function because if
    // siglongjmp is called then it must be called before function containing sigsetjmp returns
    if (SIGBUS == sigsetjmp(sEnv,1)) {
      // Raise exception with supplied message if SIGBUS received
      sProtected = 0;
      exception::SigBusError lException;
      log (lException, aMessage);
      throw lException;
    }
    else
      aAccess();

    sProtected = 0;
  }


  void SigBusGuard::blockSIGBUS()
  {
    sigset_t lSigSet;
    sigemptyset(&lSigSet);
    sigaddset(&lSigSet, SIGBUS);
    const int lErrNo = pthread_sigmask(SIG_BLOCK, &lSigSet, NULL);
    if (lErrNo != 0) {
      exception::SignalMaskingFailure lExc;
      log(lExc, "Failed to update signal mask; errno=", Integer(lErrNo), ", meaning ", Quote (strerror(lErrNo)));
      throw lExc;
    }
  }


  void SigBusGuard::handle(int aSignal)
  {
    // Warn users if SIGBUS raised outside of 'protect' function, as indicates that the offending code
    // needs to be modified so that offending line is wrapped by the 'protect' function
    if (sProtected == 0) {
      char message[] = "WARNING: A uHAL SigBusGuard has been constructed but SIGBUS was received outside of the 'protect' method. This will cause *undefined behaviour*.\nAfter creating a uhal::SigBusGuard instance, you must run any code that can raise SIGBUS inside the SigBusGuard::protect method (using its std::function argument).\n";
      write(STDOUT_FILENO, message, strlen(message));
    }

    // Jump back to the point in the stack described by sEnv (as set by sigsetjmp), with sigsetjmp now returning SIGBUS
    if (aSignal == SIGBUS)
      siglongjmp(sEnv, aSignal);
  }

}
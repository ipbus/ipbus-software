
#ifndef _uhal_detail_RobustSessionMutex_hpp_
#define _uhal_detail_RobustSessionMutex_hpp_

#include <cstdint>
#include <mutex>
#include <pthread.h>

#include "uhal/ClientInterface.hpp"
#include "uhal/log/exception.hpp"


namespace uhal {
namespace detail {

//! Exception class to handle errors from pthread mutex-related functions
UHAL_DEFINE_DERIVED_EXCEPTION_CLASS ( MutexError , uhal::exception::TransportLayerError , "Exception class to handle errors from pthread mutex-related functions." )

/*!
 * Mutex with session counter and 'session active' flag. Instantiated in shared memory by
 * the file-based client classes to prevent multiple clients from communicating with the
 * IPbus device at the same time, and to enable each client to work out if they were the
 * most recent client to communicate with the device (by checking whether the session
 * counter has changed). "Robust" since the robustness flag is set in the pthread mutex,
 * such that we can detect whether a process died before unlocking it.
 */
class RobustSessionMutex {
public:
  RobustSessionMutex();
  ~RobustSessionMutex();

  void lock();

  void unlock();

  uint64_t getCounter() const;

  bool isActive() const;

  void startSession();

  void endSession();

private:
  RobustSessionMutex(const RobustSessionMutex&);

  pthread_mutex_t mMutex;
  uint64_t mCount;
  bool mSessionActive;
};


typedef std::unique_lock<RobustSessionMutex> ScopedSessionLock;

}
}

#endif

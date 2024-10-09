
#include "uhal/detail/RobustSessionMutex.hpp"


#include <string.h>

#include "uhal/log/log.hpp"


namespace uhal {
namespace detail {

RobustSessionMutex::RobustSessionMutex() :
  mCount(0),
  mSessionActive(false)
{
  pthread_mutexattr_t lAttr;

  int s = pthread_mutexattr_init(&lAttr);
  if (s != 0) {
    MutexError lExc;
    log(lExc, "Error code ", Integer(s), " (", strerror(s), ") returned in mutex attr initialisation");
    throw lExc;
  }

  s = pthread_mutexattr_setpshared(&lAttr, PTHREAD_PROCESS_SHARED);
  if (s != 0) {
    MutexError lExc;
    log(lExc, "Error code ", Integer(s), " (", strerror(s), ") returned by pthread_mutexattr_setpshared");
    throw lExc;
  }

  s = pthread_mutexattr_setrobust(&lAttr, PTHREAD_MUTEX_ROBUST);
  if (s != 0) {
    MutexError lExc;
    log(lExc, "Error code ", Integer(s), " (", strerror(s), ") returned by pthread_mutexattr_setrobust");
    throw lExc;
  }

  s = pthread_mutex_init(&mMutex, &lAttr);
  if (s != 0) {
    MutexError lExc;
    log(lExc, "Error code ", Integer(s), " (", strerror(s), ") returned in mutex initialisation");
    throw lExc;
  }
}


RobustSessionMutex::~RobustSessionMutex()
{
}


void RobustSessionMutex::lock()
{
  int s = pthread_mutex_lock(&mMutex);
  bool lLastOwnerDied = (s == EOWNERDEAD);
  if (lLastOwnerDied)
    s = pthread_mutex_consistent(&mMutex);

  if (s != 0) {
    MutexError lExc;
    log(lExc, "Error code ", Integer(s), " (", strerror(s), ") returned when ", lLastOwnerDied ? "making mutex state consistent" : "locking mutex");
    throw lExc;
  }
}


void RobustSessionMutex::unlock()
{
  int s = pthread_mutex_unlock(&mMutex);
  if (s != 0)
    log(Error(), "Error code ", Integer(s), " (", strerror(s), ") returned when unlocking mutex");
}


uint64_t RobustSessionMutex::getCounter() const
{
  return mCount;
}


bool RobustSessionMutex::isActive() const
{
  return mSessionActive;
}


void RobustSessionMutex::startSession()
{
  mCount++;
  mSessionActive = true;
}

void RobustSessionMutex::endSession()
{
  mSessionActive = false;
}

}
}
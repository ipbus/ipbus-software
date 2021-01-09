
#include "uhal/utilities/TimeIntervalStats.hpp"


#include <chrono>
#include <ostream>                      // for operator<<, ostream, basic_os...


namespace uhal {

TimeIntervalStats::TimeIntervalStats() :
  nMeasurements(0)
{
}


TimeIntervalStats::~TimeIntervalStats()
{
}


size_t TimeIntervalStats::size() const
{
  return nMeasurements;
}


const TimeIntervalStats::Clock_t::duration& TimeIntervalStats::min() const
{
  return mMin;
}


const TimeIntervalStats::Clock_t::duration& TimeIntervalStats::max() const
{
  return mMax;
}


TimeIntervalStats::Clock_t::duration TimeIntervalStats::mean() const
{
  return mSum / nMeasurements;
}


const std::queue<TimeIntervalStats::Clock_t::duration>& TimeIntervalStats::getLatestMeasurements() const
{
  return mLatestMeasurements;
}


void TimeIntervalStats::add(const Clock_t::time_point& aT1, const Clock_t::time_point& aT2)
{
  const Clock_t::duration lInterval = aT2 - aT1;

  if ((nMeasurements == 0) or (lInterval < mMin))
  	mMin = lInterval;
  if ((nMeasurements == 0) or (lInterval > mMax))
  	mMax = lInterval;

  mSum += lInterval;
  nMeasurements += 1;

  mLatestMeasurements.push(lInterval);
  if (mLatestMeasurements.size() > 5)
  	mLatestMeasurements.pop();
}


void TimeIntervalStats::clear()
{
  for (size_t i = 0; i < nMeasurements; i++)
    mLatestMeasurements.pop();
  nMeasurements = 0;
}


std::ostream& operator<<(std::ostream& aStream, const TimeIntervalStats& aStats)
{
  if (aStats.size() == 0)
  	aStream << "no values recorded";
  else {
    typedef std::chrono::duration<float, std::milli> MilliSec_t;

  	aStream << "min / mean / max = " << MilliSec_t(aStats.min()).count() << " / " 
            << MilliSec_t(aStats.mean()).count() << " / " << MilliSec_t(aStats.max()).count() << " ms";
  	std::queue<TimeIntervalStats::Clock_t::duration> lLatestMeasurements = aStats.getLatestMeasurements();
  	aStream << ", last values ";
  	while (lLatestMeasurements.size() > 0) {
      aStream << MilliSec_t(lLatestMeasurements.front()).count() << (lLatestMeasurements.size() > 1 ? ", " : " ms");
      lLatestMeasurements.pop();
  	}
  }

  return aStream;
}


} // end ns uhal

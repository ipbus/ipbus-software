
#include "uhal/log/LogLevels.hpp"


#include "uhal/log/log_inserters.time.hpp"
#include "uhal/log/log_inserters.threadID.hpp"


namespace uhal
{

  void insert ( std::ostream& aStr , const uint32_t& aUint )
  {
    aStr << aUint;
  }

  void insert ( std::ostream& aStr , const int32_t& aInt )
  {
    aStr << aInt;
  }

  void insert ( std::ostream& aStr , const bool& aBool )
  {
    aStr << aBool;
  }


  FatalLevel::FatalLevel ( std::ostream& aStr , Base::fPtr aHeadFunction, Base::fPtr aTailFunction ) : Base ( aStr , aHeadFunction , aTailFunction ) {}

  void FatalLevel::colour_head ( std::ostream& aStr )
  {
    aStr << "\033[0;31m" //standard red
         << Time< day,'-',mth,'-',yr,' ',hr,':',min,':',sec,'.',usec > ( Now() )
         << " [" << ThisThreadID() << "]"
         << " FATAL - ";
  }

  void FatalLevel::colour_tail ( std::ostream& aStr )
  {
    aStr << "\033[0m" << std::endl;
  }

  FatalLevel Fatal;


  ErrorLevel::ErrorLevel ( std::ostream& aStr, Base::fPtr aHeadFunction, Base::fPtr aTailFunction ) : Base ( aStr , aHeadFunction , aTailFunction ) {}

  void ErrorLevel::colour_head ( std::ostream& aStr )
  {
    aStr << "\033[0;31m" //standard red
         << Time< day,'-',mth,'-',yr,' ',hr,':',min,':',sec,'.',usec > ( Now() )
         << " [" << ThisThreadID() << "]"
         << " ERROR - ";
  }

  void ErrorLevel::colour_tail ( std::ostream& aStr )
  {
    aStr << "\033[0m" << std::endl;
  }

  ErrorLevel Error;


  WarningLevel::WarningLevel ( std::ostream& aStr, Base::fPtr aHeadFunction, Base::fPtr aTailFunction )  : Base ( aStr , aHeadFunction , aTailFunction ) {}

  void WarningLevel::colour_head ( std::ostream& aStr )
  {
    aStr << "\033[0;33m" //standard yellow
         << Time< day,'-',mth,'-',yr,' ',hr,':',min,':',sec,'.',usec > ( Now() )
         << " [" << ThisThreadID() << "]"
         << " WARNING - ";
  }

  void WarningLevel::colour_tail ( std::ostream& aStr )
  {
    aStr << "\033[0m" << std::endl;
  }

  WarningLevel Warning;


  NoticeLevel::NoticeLevel ( std::ostream& aStr, Base::fPtr aHeadFunction, Base::fPtr aTailFunction )  : Base ( aStr , aHeadFunction , aTailFunction ) {}

  void NoticeLevel::colour_head ( std::ostream& aStr )
  {
    aStr << "\033[0;32m" //standard green
         << Time< day,'-',mth,'-',yr,' ',hr,':',min,':',sec,'.',usec > ( Now() )
         << " [" << ThisThreadID() << "]"
         << " NOTICE - ";
  }

  void NoticeLevel::colour_tail ( std::ostream& aStr )
  {
    aStr << "\033[0m" << std::endl;
  }

  NoticeLevel Notice;


  InfoLevel::InfoLevel ( std::ostream& aStr, Base::fPtr aHeadFunction, Base::fPtr aTailFunction ) : Base ( aStr , aHeadFunction , aTailFunction ) {}

  void InfoLevel::colour_head ( std::ostream& aStr )
  {
    aStr << "\033[0;36m" //standard cyan
         << Time< day,'-',mth,'-',yr,' ',hr,':',min,':',sec,'.',usec > ( Now() )
         << " [" << ThisThreadID() << "]"
         << " INFO - ";
  }

  void InfoLevel::colour_tail ( std::ostream& aStr )
  {
    aStr << "\033[0m" << std::endl;
  }

  InfoLevel Info;


  DebugLevel::DebugLevel ( std::ostream& aStr, Base::fPtr aHeadFunction, Base::fPtr aTailFunction )  : Base ( aStr , aHeadFunction , aTailFunction ) {}

  void DebugLevel::colour_head ( std::ostream& aStr )
  {
    aStr << "\033[0;34m" //standard blue
         << Time< day,'-',mth,'-',yr,' ',hr,':',min,':',sec,'.',usec > ( Now() )
         << " [" << ThisThreadID() << "]"
         << " DEBUG - ";
  }

  void DebugLevel::colour_tail ( std::ostream& aStr )
  {
    aStr << "\033[0m" << std::endl;
  }

  DebugLevel Debug;

}





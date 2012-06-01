/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_performance_hpp_
#define _uhal_performance_hpp_

#include <map>
#include <deque>
#include <sys/time.h>
#include <math.h>

#include <uhal/log.hpp>

#define _PASTE_(x,y) x ## _ ## y
#define PASTE(x,y)  _PASTE_(x,y)

#define _PERFORMANCE_( CODE , V1 , V2 , DESCRIPTION , FUNCTION , FILE , LINE ) \
timeval V1 , V2; \
gPerformanceMeasurement.mDepth.push_back( FUNCTION ); \
gettimeofday ( &V1 , NULL ); \
CODE \
gettimeofday ( &V2 , NULL ); \
gPerformanceMeasurement.AddEntry( DESCRIPTION , FUNCTION , FILE , LINE , V1 , V2 ); \
gPerformanceMeasurement.mDepth.pop_back();

#ifdef MEASUREPERFORMANCE
#define PERFORMANCE( DESCRIPTION , CODE ) _PERFORMANCE_( CODE , PASTE( STARTTIMER , __LINE__ ) , PASTE( ENDTIMER , __LINE__ ) , DESCRIPTION , __PRETTY_FUNCTION__ , __FILE__ , __LINE__ )
#else
#define PERFORMANCE( DESCRIPTION , CODE ) CODE
#endif


class PerformanceMeasurement
{
	public:

		struct tPerformanceMeasurement
		{
			std::string mDesc;
			std::string mFunction;
			std::string mFile;
			int mLine;
			timeval mStartTime;
			timeval mEndTime;
			/*uint32_t*/ std::deque< std::string > mDepth;

			tPerformanceMeasurement ( const std::string& aDesc , const std::string& aFunction , const std::string& aFile , const uint32_t& aLine , const timeval& aStartTime , const timeval& aEndTime , const /*uint32_t*/ std::deque< std::string >& aDepth ) :
				mDesc ( aDesc ),
				mFunction ( aFunction ),
				mFile ( aFile ),
				mLine ( aLine ),
				mStartTime ( aStartTime ),
				mEndTime ( aEndTime ),
				mDepth ( aDepth )
			{}
		};

		struct tStats
		{
			double mSum;
			double mSqSum;
			uint32_t mCount;

			double mean()
			{
				return mSum/mCount;
			}

			double var()
			{
				double lMean ( mean() );
				return ( mSqSum/mCount )- ( lMean*lMean );
			}

			double sd()
			{
				return sqrt ( var() );
			}

		};


		PerformanceMeasurement() :
			mDepth ( 0 )
		{}

		~PerformanceMeasurement()
		{
			std::map< uint32_t , std::deque< tPerformanceMeasurement* > > mSort;

			for ( std::deque< tPerformanceMeasurement >::iterator lIt = mPerformanceMeasurement.begin() ; lIt != mPerformanceMeasurement.end() ; ++lIt )
			{
				mSort[ lIt->mDepth.size() ].push_back ( & ( *lIt ) );
			}

			for ( std::map< uint32_t , std::deque< tPerformanceMeasurement* > >::iterator lIt = mSort.begin() ; lIt != mSort.end() ; ++lIt )
			{
				pantheios::log_NOTICE ( "\n\nFunction Depth " , pantheios::integer ( lIt->first ) );
				std::map< std::string , tStats > mSort2;

				for ( std::deque< tPerformanceMeasurement* >::iterator lIt2 = lIt->second.begin() ; lIt2 != lIt->second.end() ; ++lIt2 )
				{
					std::stringstream lStr;
					//lStr << "\"" << ( **lIt2 ).mDesc << "\" at " << ( **lIt2 ).mFile << ":" << ( **lIt2 ).mLine; // << " " << (**lIt2).mFunction;
					
					for( std::deque< std::string >::iterator lIt3 = ( **lIt2 ).mDepth.begin() ; lIt3 != ( **lIt2 ).mDepth.end() ; ++lIt3 ){
						lStr << "\n > " << *lIt3;
					}
					lStr << "\n\"" << ( **lIt2 ).mDesc << "\" at " << ( **lIt2 ).mFile << ":" << ( **lIt2 ).mLine << "\n" ; // << " " << (**lIt2).mFunction;
					
					
					double lStart ( ( ( double ) ( **lIt2 ).mStartTime.tv_sec*1000000.0 ) + ( ( double ) ( **lIt2 ).mStartTime.tv_usec ) );
					double lEnd ( ( ( double ) ( **lIt2 ).mEndTime.tv_sec*1000000.0 ) + ( ( double ) ( **lIt2 ).mEndTime.tv_usec ) );
					tStats& lSort2 = mSort2[ lStr.str() ];
					double lDiff ( lEnd - lStart );
					lSort2.mSum += lDiff;
					lSort2.mSqSum += ( lDiff * lDiff );
					lSort2.mCount++;
				}

				for ( std::map< std::string , tStats >::iterator lIt2 = mSort2.begin() ; lIt2 != mSort2.end() ; ++lIt2 )
				{
					pantheios::log_NOTICE ( ( *lIt2 ).first , /*" : " , */ pantheios::integer ( ( *lIt2 ).second.mCount ) , " calls averaging " , pantheios::real ( ( *lIt2 ).second.mean() ) , "+/-" , pantheios::real ( ( *lIt2 ).second.sd() ) , "us = " ,  pantheios::real ( ( *lIt2 ).second.mSum ) , "us in total\n" );
				}
			}
		}


		inline void AddEntry ( const std::string& aDesc , const std::string& aFunction , const std::string& aFile , const uint32_t& aLine , const timeval& aStartTime , const timeval& aEndTime )
		{
			mPerformanceMeasurement.push_back ( tPerformanceMeasurement ( aDesc , aFunction , aFile , aLine , aStartTime , aEndTime , mDepth ) );
		}

		/*uint32_t*/ std::deque< std::string > mDepth;
		
	private:
		std::deque< tPerformanceMeasurement > mPerformanceMeasurement;
};


extern PerformanceMeasurement gPerformanceMeasurement;

#endif

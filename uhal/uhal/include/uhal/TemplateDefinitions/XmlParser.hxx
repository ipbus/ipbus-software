/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#include <sstream>

namespace uhal
{

  template < typename R >
  BaseFunctionObject<R>::BaseFunctionObject()
  {}

  template < typename R >
  BaseFunctionObject<R>::~BaseFunctionObject()
  {}


  template < typename R , typename T >
  FunctionObject<R,T>::FunctionObject ( T& aT ) :
    mT ( aT )
  {}

  template < typename R , typename T >
  R FunctionObject<R,T>::operator() ( const pugi::xml_node& aNode )
  {
    return ( mT ) ( aNode );
  }

  template < typename R , typename T >
  FunctionObject<R,T*>::FunctionObject ( T* aT ) :
    mT ( aT )
  {}

  template < typename R , typename T >
  R FunctionObject<R,T*>::operator() ( const pugi::xml_node& aNode )
  {
    return ( *mT ) ( aNode );
  }


  template < typename R >
  Rule<R>::Rule( ) :
    mRuleId ( 0 ),
    mRequiredHash ( 0x0000000000000000 ),
    mForbiddenHash ( 0x0000000000000000 ),
    mFuncPtr ( NULL )
  {}

  template < typename R >
  Rule<R>::~Rule()
  {
    if ( mFuncPtr )
    {
      delete mFuncPtr;
      mFuncPtr = NULL;
    }
  }

  template < typename R >
  Rule<R>& Rule<R>::require ( const std::string& aStr )
  {
    if ( mForbidden.find ( aStr ) != mForbidden.end() )
    {
      log ( Error() , "Contradictory rule for attribute ", Quote ( aStr ) );
      ContradictoryParserRule().throwFrom ( ThisLocation() );
    }

    mRequired.insert ( aStr );
    return *this;
  }

  template < typename R >
  Rule<R>& Rule<R>::forbid ( const std::string& aStr )
  {
    if ( mRequired.find ( aStr ) != mRequired.end() )
    {
      log ( Error() , "Contradictory rule for attribute ", Quote ( aStr ) );
      ContradictoryParserRule().throwFrom ( ThisLocation() );
    }

    mForbidden.insert ( aStr );
    return *this;
  }

  template < typename R >
  Rule<R>& Rule<R>::optional ( const std::string& aStr )
  {
    mOptional.insert ( aStr );
    return *this;
  }

  template < typename R >
  std::string Rule<R>::description() const
  {
    std::set<std::string>::const_iterator lIt;
    std::stringstream lStr;
    lStr << "Rule " << mRuleId ;
    lIt = mRequired.begin();

    if ( lIt != mRequired.end() )
    {
      lStr << " {Require : ";

      while ( true )
      {
        lStr << *lIt ;

        if ( ( ++lIt ) == mRequired.end() )
        {
          break;
        }

        lStr<< ", ";
      }

      lStr << "}";
    }

    lIt = mForbidden.begin();

    if ( lIt != mForbidden.end() )
    {
      lStr << " {Forbid : ";

      while ( true )
      {
        lStr << *lIt ;

        if ( ( ++lIt ) == mForbidden.end() )
        {
          break;
        }

        lStr<< ", ";
      }

      lStr << "}";
    }

    lIt = mOptional.begin();

    if ( lIt != mOptional.end() )
    {
      lStr << " {Optional : ";

      while ( true )
      {
        lStr << *lIt ;

        if ( ( ++lIt ) == mOptional.end() )
        {
          break;
        }

        lStr<< ", ";
      }

      lStr << "}";
    }

    return lStr.str();
  }

  template < typename R >
  R Rule<R>::operator() ( const pugi::xml_node& aNode )
  {
    if ( mFuncPtr )
    {
      return ( *mFuncPtr ) ( aNode );
    }
    else
    {
      log ( Error() , "No action specified!" );
      NoActionSpecified().throwFrom ( ThisLocation() );
    }
  }


  template < typename R >
  Parser<R>::Parser() :
    mNextHash ( 0x0000000000000001 ),
    mRuleCounter ( 0 )
  {
    mHashes.clear();
  }

  template < typename R >
  Parser<R>::~Parser()
  {}



  template < typename R >
  template < typename T >
  void Parser<R>::addRule ( const Rule<R> & aRule , T aCallbackHandler )
  {
    mRules.push_back ( aRule );
    Rule<R>& lRule ( mRules.back() );

    for ( std::set<std::string>::iterator lIt = lRule.mRequired.begin() ; lIt != lRule.mRequired.end() ; ++lIt )
    {
      std::hash_map< std::string , uint64_t >::iterator lIt2 ( mHashes.find ( *lIt ) );

      if ( lIt2 == mHashes.end() )
      {
        if ( mNextHash == 0x0000000000000000 )
        {
          log ( Error() , "Too many attributes" );
          TooManyAttributes().throwFrom ( ThisLocation() );
        }

        lRule.mRequiredHash |= mNextHash;
        mHashes.insert ( std::make_pair ( *lIt , mNextHash ) );
        mNextHash <<= 1;
      }
      else
      {
        lRule.mRequiredHash |= lIt2->second;
      }
    }

    for ( std::set<std::string>::iterator lIt = lRule.mForbidden.begin() ; lIt != lRule.mForbidden.end() ; ++lIt )
    {
      std::hash_map< std::string , uint64_t >::iterator lIt2 ( mHashes.find ( *lIt ) );

      if ( lIt2 == mHashes.end() )
      {
        if ( mNextHash == 0x0000000000000000 )
        {
          log ( Error() , "Too many attributes" );
          TooManyAttributes().throwFrom ( ThisLocation() );
        }

        lRule.mForbiddenHash |= mNextHash;
        mHashes.insert ( std::make_pair ( *lIt , mNextHash ) );
        mNextHash <<= 1;
      }
      else
      {
        lRule.mForbiddenHash |= lIt2->second;
      }
    }

    for ( std::set<std::string>::iterator lIt = lRule.mOptional.begin() ; lIt != lRule.mOptional.end() ; ++lIt )
    {
      std::hash_map< std::string , uint64_t >::iterator lIt2 ( mHashes.find ( *lIt ) );

      if ( lIt2 == mHashes.end() )
      {
        if ( mNextHash == 0x0000000000000000 )
        {
          log ( Error() , "Too many attributes" );
          TooManyAttributes().throwFrom ( ThisLocation() );
        }

        mHashes.insert ( std::make_pair ( *lIt , mNextHash ) );
        mNextHash <<= 1;
      }
    }

    lRule.mRuleId = ++mRuleCounter;
    lRule.mFuncPtr = new FunctionObject<R,T> ( aCallbackHandler );
  }


  template < typename R >
  R Parser<R>::operator() ( const pugi::xml_node& aNode )
  {
    uint64_t lHash ( 0x0000000000000000 );

    for ( pugi::xml_attribute lAttr = aNode.first_attribute(); lAttr; lAttr = lAttr.next_attribute() )
    {
      std::hash_map< std::string , uint64_t >::iterator lIt2 ( mHashes.find ( lAttr.name() ) );

      if ( lIt2 == mHashes.end() )
      {
        log ( Error() , "Parser failed because of unknown attribute ", Quote ( lAttr.name() ) );
        UnknownAttribute().throwFrom ( ThisLocation() );
      }

      lHash |= lIt2->second;
    }

    std::deque< Rule<R>* > lPassed;
    std::deque< Rule<R>* > lFailedRequired;
    std::deque< Rule<R>* > lFailedForbidden;
    int i ( 0 );

    for ( typename std::deque< Rule<R> >::iterator lIt = mRules.begin() ; lIt != mRules.end() ; ++lIt, ++i )
    {
      if ( lIt->mForbiddenHash & lHash )
      {
        lFailedForbidden.push_back ( & ( *lIt ) );
        continue;
      }

      if ( lIt->mRequiredHash & ~lHash )
      {
        lFailedRequired.push_back ( & ( *lIt ) );
        continue;
      }

      lPassed.push_back ( & ( *lIt ) );
    }

    if ( lPassed.size() == 1 )
    {
      return ( *lPassed.front() ) ( aNode );
    }

    if ( lPassed.size() > 1 )
    {
      log ( Warning() , "Ambiguity! " , Integer ( lPassed.size() ) ," rules passed. Attempting to find the rule with the most stringent requirements." );
      Rule<R>* lMostStringent ( NULL );
      uint32_t lCounter ( 0 );

      for ( typename std::deque< Rule<R>* >::iterator lIt = lPassed.begin(); lIt != lPassed.end(); ++lIt )
      {
        if ( lCounter < ( **lIt ).mRequired.size() )
        {
          lMostStringent = ( *lIt );
          lCounter = ( **lIt ).mRequired.size();
        }
        else if ( lCounter == ( **lIt ).mRequired.size() )
        {
          lMostStringent = NULL;
        }
      }

      if ( !lMostStringent )
      {
        log ( Error() , "Ambiguity remains! Multiple rules passed " , Integer ( lCounter ) , " requirements." );
        AmbiguousParserRules().throwFrom ( ThisLocation() );
      }

      log ( Warning() , "In ambiguous case, selected " , lMostStringent->description() );
      return ( *lMostStringent ) ( aNode );
    }

    if ( lFailedRequired.size() )
    {
      log ( Error() , Integer ( lFailedRequired.size() ) , " rules failed because of missing required attributes" );
    }

    if ( lFailedForbidden.size() )
    {
      log ( Error() , Integer ( lFailedForbidden.size() ) , " rules failed because of included forbidden attributes" );
    }

    NoRulesPassed().throwFrom ( ThisLocation() );
  }

}


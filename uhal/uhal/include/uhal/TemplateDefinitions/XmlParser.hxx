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
  {
    logging();
  }

  template < typename R >
  BaseFunctionObject<R>::~BaseFunctionObject()
  {
    logging();
  }


  template < typename R , typename T >
  FunctionObject<R,T>::FunctionObject ( T& aT ) :
    mT ( aT )
  {
    logging();
  }

  template < typename R , typename T >
  R FunctionObject<R,T>::operator() ( const pugi::xml_node& aNode )
  {
    logging();
    return ( mT ) ( aNode );
  }

  template < typename R , typename T >
  FunctionObject<R,T*>::FunctionObject ( T* aT ) :
    mT ( aT )
  {
    logging();
  }

  template < typename R , typename T >
  R FunctionObject<R,T*>::operator() ( const pugi::xml_node& aNode )
  {
    logging();
    return ( *mT ) ( aNode );
  }


  template < typename R >
  Rule<R>::Rule( ) :
    mRuleId ( 0 ),
    mRequiredHash ( 0x0000000000000000 ),
    mForbiddenHash ( 0x0000000000000000 ),
    mFuncPtr ( NULL )
  {
    logging();
  }

  template < typename R >
  Rule<R>::~Rule()
  {
    logging();

    if ( mFuncPtr )
    {
      delete mFuncPtr;
      mFuncPtr = NULL;
    }
  }

  template < typename R >
  Rule<R>& Rule<R>::require ( const std::string& aStr )
  {
    logging();

    if ( mForbidden.find ( aStr ) != mForbidden.end() )
    {
      log ( Error() , "Contradictory rule for attribute ", Quote ( aStr ) );
      throw ContradictoryParserRule();
    }

    mRequired.insert ( aStr );
    return *this;
  }

  template < typename R >
  Rule<R>& Rule<R>::forbid ( const std::string& aStr )
  {
    logging();

    if ( mRequired.find ( aStr ) != mRequired.end() )
    {
      log ( Error() , "Contradictory rule for attribute ", Quote ( aStr ) );
      throw ContradictoryParserRule();
    }

    mForbidden.insert ( aStr );
    return *this;
  }

  template < typename R >
  Rule<R>& Rule<R>::optional ( const std::string& aStr )
  {
    logging();
    mOptional.insert ( aStr );
    return *this;
  }

  template < typename R >
  std::string Rule<R>::description() const
  {
    logging();
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
    logging();

    if ( mFuncPtr )
    {
      return ( *mFuncPtr ) ( aNode );
    }
    else
    {
      log ( Error() , "No action specified!" );
      throw NoActionSpecified();
    }
  }


  template < typename R >
  Parser<R>::Parser() :
    mNextHash ( 0x0000000000000001 ),
    mRuleCounter ( 0 )
  {
    logging();
    mHashes.clear();
  }

  template < typename R >
  Parser<R>::~Parser()
  {
    logging();
  }



  template < typename R >
  template < typename T >
  void Parser<R>::addRule ( const Rule<R> & aRule , T aCallbackHandler )
  {
    logging();
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
          throw TooManyAttributes();
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
          throw TooManyAttributes();
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
          throw TooManyAttributes();
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
    logging();
    uint64_t lHash ( 0x0000000000000000 );

    for ( pugi::xml_attribute lAttr = aNode.first_attribute(); lAttr; lAttr = lAttr.next_attribute() )
    {
      std::hash_map< std::string , uint64_t >::iterator lIt2 ( mHashes.find ( lAttr.name() ) );

      if ( lIt2 == mHashes.end() )
      {
        log ( Error() , "Parser failed because of unknown attribute ", Quote ( lAttr.name() ) );
        throw UnknownAttribute();
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
      }
      else if ( lIt->mRequiredHash & ~lHash )
      {
        lFailedRequired.push_back ( & ( *lIt ) );
        continue;
      }
      else
      {
        lPassed.push_back ( & ( *lIt ) );
      }
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
        throw AmbiguousParserRules();
      }

      log ( Warning() , "In ambiguous case, selected " , lMostStringent->description() );
      return ( *lMostStringent ) ( aNode );
    }

    std::stringstream lStr;

    for ( pugi::xml_attribute lAttr = aNode.first_attribute(); lAttr; lAttr = lAttr.next_attribute() )
    {
      lStr << lAttr.name() << "=\"" << lAttr.value() << "\", ";
    }

    std::string lString ( lStr.str() );
    lString.resize ( lString.size() - 2 );
    log ( Error() , "Node with attributes : " , lString , " failed all parser rules because : " );

    for ( typename std::deque< Rule<R>* >::iterator lIt = lFailedRequired.begin() ; lIt != lFailedRequired.end(); ++lIt )
    {
      std::stringstream lStr;
      uint64_t lTemp ( ( **lIt ).mRequiredHash & ~lHash );

      for ( std::hash_map< std::string , uint64_t >::iterator lIt2 = mHashes.begin() ; lIt2 != mHashes.end() ; ++lIt2 )
      {
        if ( ( lIt2->second ) & lTemp )
        {
          lStr << "\"" << lIt2->first << "\", ";
        }
      }

      std::string lString ( lStr.str() );
      lString.resize ( lString.size() - 2 );
      log ( Error() , " > Rule " ,  Integer ( ( **lIt ).mRuleId ) , " requires attributes : " , lString );
    }

    for ( typename std::deque< Rule<R>* >::iterator lIt = lFailedForbidden.begin() ; lIt != lFailedForbidden.end(); ++lIt )
    {
      std::stringstream lStr;
      uint64_t lTemp ( ( **lIt ).mForbiddenHash & lHash );

      for ( std::hash_map< std::string , uint64_t >::iterator lIt2 = mHashes.begin() ; lIt2 != mHashes.end() ; ++lIt2 )
      {
        if ( ( lIt2->second ) & lTemp )
        {
          lStr << "\"" << lIt2->first << "\", ";
        }
      }

      std::string lString ( lStr.str() );
      lString.resize ( lString.size() - 2 );
      log ( Error() , " > Rule " ,  Integer ( ( **lIt ).mRuleId ) , " forbids attributes : " , lString );
    }

    throw NoRulesPassed();
  }

}


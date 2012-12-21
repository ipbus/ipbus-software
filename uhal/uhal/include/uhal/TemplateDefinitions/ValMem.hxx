
namespace uhal
{

  template< typename T >

  ValHeader::ValHeader ( const ValWord<T>& aValWord ) :
    mMembers ( aValWord.mMembers )
  {
    logging();
  }



  template< typename T >

  ValHeader::ValHeader ( const ValVector<T>& aValVector ) :
    mMembers ( aValVector.mMembers )
  {
    logging();
  }


  template< typename T >
  template <class InputIterator>
  void ValVector<T>::assign ( InputIterator aBegin , InputIterator aEnd )
  {
    logging();

    if ( !/* *mValid */ mMembers->valid )
    {
      /* mValues-> */ mMembers->value.assign ( aBegin , aEnd );
    }
    else
    {
      throw ValMemImutabilityViolation();
    }
  }



}




namespace uhal
{

  template <class T>
  void NodeTreeBuilder::add ( const std::string& aNodeClassName )
  {
    logging();
    std::hash_map<std::string , boost::shared_ptr<CreatorInterface> >::const_iterator lIt = mCreators.find ( aNodeClassName ) ;

    if ( lIt != mCreators.end() )
    {
      //
      //ProtocolAlreadyExist().throwFrom( ThisLocation() );
      log ( Warning() , "Protocol \"" , aNodeClassName , "\" already exists in map of creators. Continuing for now, but be warned." );
      return;
    }

    mCreators[aNodeClassName] =  boost::shared_ptr<CreatorInterface> ( new Creator<T>() );
  }


  template <class T>
  Node* NodeTreeBuilder::Creator<T>::create ( const std::vector< std::pair<std::string, std::string> >& aAttributes )
  {
    logging();
    log ( Debug() , "Creator called for Node of type " , Quote ( Type < T >() ) );
    return new T ( aAttributes );
  }


}

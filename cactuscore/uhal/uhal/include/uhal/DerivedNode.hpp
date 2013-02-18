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


      Andrew Rose, Imperial College, London
      email: awr01 <AT> imperial.ac.uk

      Marc Magrans de Abril, CERN
      email: marc.magrans.de.abril <AT> cern.ch

---------------------------------------------------------------------------
*/

/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_DerivedNode_hpp_
#define _uhal_DerivedNode_hpp_

#include "uhal/Node.hpp"

#define REGISTER( classname ) uhal::RegistrationHelper< classname > classname##RegistrationHelper( #classname );

namespace uhal
{

  template < typename DerivedType >
  class DerivedNode : public Node
  {
    protected:

      /**
        CRTP function to produce a new copy of the current Node
        @return a new copy of the current Node
      */
      virtual Node* clone() const;

  };

  template< typename T >
  struct RegistrationHelper
  {
    RegistrationHelper ( const std::string& aDerivedClassName );
  };

}

#include "uhal/TemplateDefinitions/DerivedNode.hxx"

#endif

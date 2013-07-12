#ifndef _uhal_logo_hpp_
#define _uhal_logo_hpp_

namespace uhal
{

  class logo
  {
    public:
      logo();
      ~logo();

      void operator++ ( int );

    private:
      const char* mPtr;
      static const char* const mLogoStr;
  };

}
#endif

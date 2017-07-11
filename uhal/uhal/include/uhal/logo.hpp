#ifndef _uhal_logo_hpp_
#define _uhal_logo_hpp_

namespace uhal
{

  /**
    Fun class to print the "Imperial College London" logo one character at a time
    (A more interesting to show progress than printing dots)
  */
  class logo
  {
    public:
      /**
        Constructor
      */
      logo();
      /**
        Destructor
      */
      ~logo();

      /**
        Print the next character in the logo
        @param aDummy a dummy parameter to make the operator postfix
      */
      void operator++ ( int aDummy );

    private:
      //! The next character to print
      const char* mPtr;
      //! The raw message encoding the "Imperial College London" logo in ASCII
      static const char* const mLogoStr;
  };

}
#endif


#ifndef _uhal_detail_PacketFmt_hpp_
#define _uhal_detail_PacketFmt_hpp_


#include <cstdint>
#include <iosfwd>
#include <utility>
#include <vector>


namespace uhal {
namespace detail {

class PacketFmt {
public:
  PacketFmt(const uint8_t* const, const size_t);
  PacketFmt(const std::vector< std::pair<const uint8_t*, size_t> >& aData);
  ~PacketFmt();

  const std::vector< std::pair<const uint8_t*, size_t> > mData;
};


std::ostream& operator<<(std::ostream&, const PacketFmt&);

} // end namespace 
}

#endif

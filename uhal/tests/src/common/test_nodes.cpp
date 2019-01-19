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

      Tom Williams, Rutherford Appleton Laboratory, Oxfordshire
      email: tom.williams <AT> cern.ch

---------------------------------------------------------------------------
*/

#include <typeinfo>

#include "uhal/NodeTreeBuilder.hpp"
#include "uhal/uhal.hpp"

#include "uhal/tests/DummyDerivedNode.hpp"
#include "uhal/tests/fixtures.hpp"

#include <boost/filesystem.hpp>
#include <boost/test/unit_test.hpp>



namespace uhal {
namespace tests {

struct NodeFixture : public AbstractFixture {
public:
  NodeFixture();
  ~NodeFixture() {}

  struct Properties {
    Properties(const std::string& aIdPath, const uint32_t aAddress, const defs::BlockReadWriteMode aMode, const defs::NodePermission aPermission, const size_t aSize, const std::string& aModule, const size_t aNrDescendants);
    ~Properties() {}

    std::string path;
    std::string id;
    uint32_t address;
    defs::BlockReadWriteMode mode;
    defs::NodePermission permission;
    size_t size;
    uint32_t mask;
    std::string description;
    std::string module;
    std::vector<std::string> descendantIds;
    std::map<std::string, const std::type_info*> descendantTypes;
    std::string tags;
    boost::unordered_map<std::string,std::string> parameters;
    boost::unordered_map<std::string,std::string> fwInfo;
    const std::type_info* type;
  };

  std::string getAddrFileAbsPath(const std::string& aSuffix)
  {
    std::string lResult(addrFileAbsPath);
    lResult.replace(addrFileAbsPath.size() - 11, 7, aSuffix);
    return lResult;
  }

  const std::string addrFileURI;
  const std::string addrFileAbsPath;
  std::string addrFileLevel2AbsPath;
  std::string addrFileLevel3AbsPath;
  std::vector<Properties> nodeProperties;
};


NodeFixture::NodeFixture() :
  AbstractFixture(),
  addrFileURI(connectionFileURI.replace(connectionFileURI.size() - 15, 11, "address")),
  addrFileAbsPath(boost::filesystem::absolute(boost::filesystem::path(addrFileURI.substr(7))).native()),
  addrFileLevel2AbsPath(addrFileAbsPath),
  addrFileLevel3AbsPath(addrFileAbsPath)
{
  addrFileLevel2AbsPath.replace(addrFileAbsPath.size() - 11, 0, "level2_");
  addrFileLevel3AbsPath.replace(addrFileAbsPath.size() - 11, 0, "level3_");

  nodeProperties.push_back(Properties("", 0, defs::HIERARCHICAL, defs::READWRITE, 1, addrFileAbsPath, 55));

  nodeProperties.push_back(Properties("REG", 1, defs::SINGLE, defs::READWRITE, 1, addrFileAbsPath, 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(Properties("REG_READ_ONLY", 2, defs::SINGLE, defs::READ, 1, addrFileAbsPath, 0));
  nodeProperties.push_back(Properties("REG_WRITE_ONLY", 3, defs::SINGLE, defs::WRITE, 1, addrFileAbsPath, 0));
  nodeProperties.push_back(Properties("REG_UPPER_MASK", 4, defs::SINGLE, defs::READWRITE, 1, addrFileAbsPath, 0));
  nodeProperties.back().mask = 0xffff0000;
  nodeProperties.push_back(Properties("REG_LOWER_MASK", 4, defs::SINGLE, defs::READWRITE, 1, addrFileAbsPath, 0));
  nodeProperties.back().mask = 0xffff;
  nodeProperties.push_back(Properties("REG_MASKED_READ_ONLY", 5, defs::SINGLE, defs::READ, 1, addrFileAbsPath, 0));
  nodeProperties.back().mask = 0xffff0000;
  nodeProperties.push_back(Properties("REG_MASKED_WRITE_ONLY", 5, defs::SINGLE, defs::WRITE, 1, addrFileAbsPath, 0));
  nodeProperties.back().mask = 0xffff;
  nodeProperties.push_back(Properties("REG_PARS", 6, defs::SINGLE, defs::READWRITE, 1, addrFileAbsPath, 0));
  nodeProperties.back().parameters["arg0"] = "val100";
  nodeProperties.back().parameters["arg1"] = "val101";
  nodeProperties.push_back(Properties("FIFO", 0x100, defs::NON_INCREMENTAL, defs::READWRITE, 268435456, addrFileAbsPath, 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(Properties("REG_OUT_OF_ORDER", 6, defs::SINGLE, defs::READWRITE, 1, addrFileAbsPath, 0));
  nodeProperties.push_back(Properties("MEM", 0x100000, defs::INCREMENTAL, defs::READWRITE, 262144, addrFileAbsPath, 0));
  nodeProperties.back().description = "A block memory in an example XML file";

  nodeProperties.push_back(Properties("SUBSYSTEM1", 0x210001, defs::HIERARCHICAL, defs::READWRITE, 1, addrFileAbsPath, 5));
  nodeProperties.back().parameters["arg0"] = "val200";
  nodeProperties.back().parameters["arg1"] = "val201";
  nodeProperties.back().parameters["arg2"] = "val202";
  nodeProperties.push_back(Properties("SUBSYSTEM1.REG", 0x210002, defs::SINGLE, defs::READWRITE, 1, addrFileLevel2AbsPath, 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(Properties("SUBSYSTEM1.MEM", 0x210003, defs::INCREMENTAL, defs::READWRITE, 262144, addrFileLevel2AbsPath, 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(Properties("SUBSYSTEM1.SUBMODULE", 0x270001, defs::HIERARCHICAL, defs::READWRITE, 1, addrFileLevel2AbsPath, 2));
  nodeProperties.back().parameters["arg0"] = "val300";
  nodeProperties.back().parameters["arg1"] = "val301";
  nodeProperties.back().parameters["arg2"] = "val10302";
  nodeProperties.back().parameters["arg3"] = "val10303";
  nodeProperties.push_back(Properties("SUBSYSTEM1.SUBMODULE.REG", 0x270002, defs::SINGLE, defs::READWRITE, 1, addrFileLevel3AbsPath, 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(Properties("SUBSYSTEM1.SUBMODULE.MEM", 0x270003, defs::INCREMENTAL, defs::READWRITE, 256, addrFileLevel3AbsPath, 0));
  nodeProperties.back().tags = "test";

  nodeProperties.push_back(Properties("SUBSYSTEM2", 0x310001, defs::HIERARCHICAL, defs::READWRITE, 1, addrFileAbsPath, 5));
  nodeProperties.back().parameters["arg0"] = "val10000";
  nodeProperties.back().parameters["arg1"] = "val201";
  nodeProperties.back().parameters["arg2"] = "val202";
  nodeProperties.back().parameters["arg5"] = "val10005";
  nodeProperties.push_back(Properties("SUBSYSTEM2.REG", 0x310002, defs::SINGLE, defs::READWRITE, 1, addrFileLevel2AbsPath, 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(Properties("SUBSYSTEM2.MEM", 0x310003, defs::INCREMENTAL, defs::READWRITE, 262144, addrFileLevel2AbsPath, 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(Properties("SUBSYSTEM2.SUBMODULE", 0x370001, defs::HIERARCHICAL, defs::READWRITE, 1, addrFileLevel2AbsPath, 2));
  nodeProperties.back().parameters["arg0"] = "val300";
  nodeProperties.back().parameters["arg1"] = "val301";
  nodeProperties.back().parameters["arg2"] = "val10302";
  nodeProperties.back().parameters["arg3"] = "val10303";
  nodeProperties.push_back(Properties("SUBSYSTEM2.SUBMODULE.REG", 0x370002, defs::SINGLE, defs::READWRITE, 1, addrFileLevel3AbsPath, 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(Properties("SUBSYSTEM2.SUBMODULE.MEM", 0x370003, defs::INCREMENTAL, defs::READWRITE, 256, addrFileLevel3AbsPath, 0));
  nodeProperties.back().tags = "test";

  nodeProperties.push_back(Properties("SMALL_MEM", 0x400000, defs::INCREMENTAL, defs::READWRITE, 256, addrFileAbsPath, 0));
  nodeProperties.push_back(Properties("LARGE_MEM", 0x500000, defs::INCREMENTAL, defs::READWRITE, 26214400, addrFileAbsPath, 0));

  nodeProperties.push_back(Properties("SUBSYSTEM3", 0x600000, defs::HIERARCHICAL, defs::READWRITE, 1, addrFileAbsPath, 28));

  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDNODE", 0x600000, defs::HIERARCHICAL, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 4));
  nodeProperties.back().type = &typeid(DummyParentNode);
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDNODE.REG", 0x600001, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 0));
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDNODE.REG_WRITE_ONLY", 0x600003, defs::SINGLE, defs::WRITE, 1, getAddrFileAbsPath("derived_address"), 0));
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDNODE.REG_UPPER_MASK", 0x600004, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 0));
  nodeProperties.back().mask = 0xFFFF0000;
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDNODE.REG_LOWER_MASK", 0x600004, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 0));
  nodeProperties.back().mask = 0xFFFF;

  // 0x600000 + 0x10 + 0x10010
  const uint32_t lDerivedModule1Addr = 0x600000 + (0x10 | 0x10010);
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDMODULE1", lDerivedModule1Addr, defs::HIERARCHICAL, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 2));
  nodeProperties.back().type = &typeid(DummyChildNode);
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDMODULE1.REG", lDerivedModule1Addr + 1, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level2_module1"), 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDMODULE1.MEM", lDerivedModule1Addr + 2, defs::INCREMENTAL, defs::READWRITE, 262144, getAddrFileAbsPath("derived_level2_module1"), 0));
  nodeProperties.back().tags = "test";

  const uint32_t lDerivedModule2Addr = 0x600000 + (0x30 | 0x10010 | 0x10010);
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDMODULE2", lDerivedModule2Addr, defs::HIERARCHICAL, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 4));
  nodeProperties.back().parameters["arg3"] = "val3";
  nodeProperties.back().type = &typeid(DummyChildNode);
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDMODULE2.REG", lDerivedModule2Addr + 1, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level3_plain"), 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDMODULE2.REG_WRITE_ONLY", lDerivedModule2Addr + 3, defs::SINGLE, defs::WRITE, 1, getAddrFileAbsPath("derived_level3_plain"), 0));
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDMODULE2.REG_UPPER_MASK", lDerivedModule2Addr + 4, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level3_plain"), 0));
  nodeProperties.back().mask = 0xFFFF0000;
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDMODULE2.REG_LOWER_MASK", lDerivedModule2Addr + 4, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level3_plain"), 0));
  nodeProperties.back().mask = 0xFFFF;

  const uint32_t lDerivedModule3Addr = 0x600000 + (0x50 | 0x10010 | 0x10010);
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDMODULE3", lDerivedModule3Addr, defs::HIERARCHICAL, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 4));
  nodeProperties.back().parameters["arg3"] = "val3";
  nodeProperties.back().type = &typeid(DummyChildNode);
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDMODULE3.REG", lDerivedModule3Addr + 1, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level3_class"), 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDMODULE3.REG_WRITE_ONLY", lDerivedModule3Addr + 3, defs::SINGLE, defs::WRITE, 1, getAddrFileAbsPath("derived_level3_class"), 0));
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDMODULE3.REG_UPPER_MASK", lDerivedModule3Addr + 4, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level3_class"), 0));
  nodeProperties.back().mask = 0xFFFF0000;
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDMODULE3.REG_LOWER_MASK", lDerivedModule3Addr + 4, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level3_class"), 0));
  nodeProperties.back().mask = 0xFFFF;

  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDMODULE4", 0x610070, defs::HIERARCHICAL, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 4));
  nodeProperties.back().type = &typeid(DummyChildNode);
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDMODULE4.REG", 0x610071, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level3_class"), 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDMODULE4.REG_WRITE_ONLY", 0x610073, defs::SINGLE, defs::WRITE, 1, getAddrFileAbsPath("derived_level3_class"), 0));
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDMODULE4.REG_UPPER_MASK", 0x610074, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level3_class"), 0));
  nodeProperties.back().mask = 0xFFFF0000;
  nodeProperties.push_back(Properties("SUBSYSTEM3.DERIVEDMODULE4.REG_LOWER_MASK", 0x610074, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level3_class"), 0));
  nodeProperties.back().mask = 0xFFFF;

  nodeProperties.push_back(Properties("SUBSYSTEM3.BADNODE", 0x600100, defs::HIERARCHICAL, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 4));
  nodeProperties.push_back(Properties("SUBSYSTEM3.BADNODE.REG", 0x600101, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 0));
  nodeProperties.push_back(Properties("SUBSYSTEM3.BADNODE.REG_WRITE_ONLY", 0x600103, defs::SINGLE, defs::WRITE, 1, getAddrFileAbsPath("derived_address"), 0));
  nodeProperties.push_back(Properties("SUBSYSTEM3.BADNODE.REG_UPPER_MASK", 0x600104, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 0));
  nodeProperties.back().mask = 0xFFFF0000;
  nodeProperties.push_back(Properties("SUBSYSTEM3.BADNODE.REG_LOWER_MASK", 0x600104, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 0));
  nodeProperties.back().mask = 0xFFFF;

  nodeProperties.push_back(Properties("IPBUS_ENDPOINT", 0x700000, defs::SINGLE, defs::READWRITE, 1, addrFileAbsPath, 0));
  nodeProperties.back().fwInfo["type"] = "endpoint";
  nodeProperties.back().fwInfo["width"] = "0x10";

  for (size_t i=0; i < nodeProperties.size(); i++) {
    BOOST_REQUIRE_LE(nodeProperties.at(i).descendantIds.size(), (nodeProperties.size() - i - 1));
    for (size_t j=0; j < nodeProperties.at(i).descendantIds.size(); j++) {
      std::string lRelativePath = nodeProperties.at(i+j+1).path.substr(nodeProperties.at(i).path.empty() ? 0 : nodeProperties.at(i).path.size() + 1);
      nodeProperties.at(i).descendantIds.at(j) = lRelativePath;
      nodeProperties.at(i).descendantTypes[lRelativePath] = nodeProperties.at(i+j+1).type;
    }
  }
}

NodeFixture::Properties::Properties(const std::string& aIdPath, const uint32_t aAddress, const defs::BlockReadWriteMode aMode, const defs::NodePermission aPermission, const size_t aSize, const std::string& aModule, const size_t aNrDescendants) :
  path(aIdPath),
  id((aIdPath.rfind('.') == std::string::npos) ? aIdPath : aIdPath.substr(aIdPath.rfind('.') + 1)),
  address(aAddress),
  mode(aMode),
  permission(aPermission),
  size(aSize),
  mask(defs::NOMASK),
  module(aModule),
  descendantIds(aNrDescendants),
  type(&typeid(Node))
{
}


void checkProperties(const uhal::Node& aNode, const NodeFixture::Properties& aExpected)
{
  BOOST_CHECK_EQUAL(aNode.getPath(), aExpected.path);
  BOOST_CHECK_EQUAL(aNode.getId(), aExpected.id);
  BOOST_CHECK_EQUAL(aNode.getAddress(), aExpected.address);
  BOOST_CHECK_EQUAL(aNode.getPermission(), aExpected.permission);
  BOOST_CHECK_EQUAL(aNode.getSize(), aExpected.size);
  BOOST_CHECK_EQUAL(aNode.getMask(), aExpected.mask);
  BOOST_CHECK_EQUAL(aNode.getMode(), aExpected.mode);

  BOOST_CHECK_EQUAL(aNode.getDescription(), aExpected.description);
  BOOST_CHECK_EQUAL(aNode.getModule(), aExpected.module);
  BOOST_CHECK_EQUAL(aNode.getTags(), aExpected.tags);

  // Validate parameters map
  BOOST_CHECK_EQUAL(aNode.getParameters().size(), aExpected.parameters.size());
  for (boost::unordered_map<std::string,std::string>::const_iterator lIt=aExpected.parameters.begin(); lIt!=aExpected.parameters.end(); lIt++) {
    BOOST_CHECK_MESSAGE(aNode.getParameters().count(lIt->first) == size_t(1), "Parameter '" << lIt->first << "' was not found");
    if (aNode.getParameters().count(lIt->first) == size_t(1)){
      BOOST_CHECK_MESSAGE(aNode.getParameters().find(lIt->first)->second == lIt->second, "Parameter '" << lIt->first << "' has value '" << aNode.getParameters().find(lIt->first)->second << "'; expected value '" << lIt->second <<  "'");
    }
  }
  for (boost::unordered_map<std::string,std::string>::const_iterator lIt=aNode.getParameters().begin(); lIt!=aNode.getParameters().end(); lIt++) {
    BOOST_CHECK_MESSAGE(aExpected.parameters.count(lIt->first) == size_t(1), "Node has unexpected parameter '" << lIt->first << "'" << " (value: '" << lIt->second << "')");
  }

  // Validate firmware info map
  BOOST_CHECK_EQUAL(aNode.getFirmwareInfo().size(), aExpected.fwInfo.size());
  for (boost::unordered_map<std::string,std::string>::const_iterator lIt=aExpected.fwInfo.begin(); lIt!=aExpected.fwInfo.end(); lIt++) {
    BOOST_CHECK_MESSAGE(aNode.getFirmwareInfo().count(lIt->first) == size_t(1), "FW info '" << lIt->first << "' was not found");
    if (aNode.getFirmwareInfo().count(lIt->first) == size_t(1)){
      BOOST_CHECK_MESSAGE(aNode.getFirmwareInfo().find(lIt->first)->second == lIt->second, "FW info '" << lIt->first << "' has value '" << aNode.getFirmwareInfo().find(lIt->first)->second << "'; expected value '" << lIt->second <<  "'");
    }
  }
  for (boost::unordered_map<std::string,std::string>::const_iterator lIt=aNode.getFirmwareInfo().begin(); lIt!=aNode.getFirmwareInfo().end(); lIt++) {
    BOOST_CHECK_MESSAGE(aExpected.fwInfo.count(lIt->first) == size_t(1), "Node has unexpected FW info '" << lIt->first << "'" << " (value: '" << lIt->second << "')");
  }
}


void checkExceptionsThrownByReadWrite(const uhal::Node& aNode, const NodeFixture::Properties& aProperties)
{
  // Single-word read/write methods - check exceptions thrown when they should be
  if (aProperties.permission == defs::READ)
    BOOST_CHECK_THROW(aNode.write(1), uhal::exception::WriteAccessDenied);
  if (aProperties.permission == defs::WRITE) {
    BOOST_CHECK_THROW(aNode.read(), uhal::exception::ReadAccessDenied);
    if (aProperties.mask != 0xFFFFFFFF)
      BOOST_CHECK_THROW(aNode.write(1), uhal::exception::WriteAccessDenied);
  }

  // Block read/write methods - check exceptions thrown when they should be 
  if (aProperties.mode == defs::SINGLE) {
    for (size_t n=0; n <= 0xF; n++) {
      if (n == 1)
        continue;
      BOOST_CHECK_THROW(aNode.readBlock(n), exception::BulkTransferOnSingleRegister);
      BOOST_CHECK_THROW(aNode.writeBlock(std::vector<uint32_t>(n)), exception::BulkTransferOnSingleRegister);
    }
    if (aProperties.permission == defs::READ)
      BOOST_CHECK_THROW(aNode.writeBlock(std::vector<uint32_t>(1)), exception::WriteAccessDenied);
    if (aProperties.permission == defs::WRITE)
      BOOST_CHECK_THROW(aNode.readBlock(1), exception::ReadAccessDenied);
  }
  else if (aProperties.mode != defs::HIERARCHICAL) {
    for (size_t n=0; n <= aNode.getSize(); n++) {
      if (aProperties.permission == defs::WRITE)
        BOOST_CHECK_THROW(aNode.readBlock(n), exception::ReadAccessDenied);
      if (aProperties.permission == defs::READ)
        BOOST_CHECK_THROW(aNode.writeBlock(std::vector<uint32_t>(n)), exception::WriteAccessDenied);
    }

    for (size_t n=aNode.getSize()+1; n <= aNode.getSize() + 0x10; n++) {
      BOOST_CHECK_THROW(aNode.readBlock(n), exception::BulkTransferRequestedTooLarge);
      BOOST_CHECK_THROW(aNode.writeBlock(std::vector<uint32_t>(n)), exception::BulkTransferRequestedTooLarge);
    }
  }

  // Block read/write offset methods - check exceptions thrown when they should be 
  switch (aProperties.mode) {
    case defs::SINGLE :
      for (size_t i=0; i <= 0xF; i++) {
        // std::cout << " i = 0x" << std::hex << i << std::endl;
        for (size_t j=0; j <= 0xF; j++) {
          BOOST_CHECK_THROW(aNode.readBlockOffset(i, j), exception::BulkTransferOffsetRequestedForSingleRegister);
          BOOST_CHECK_THROW(aNode.writeBlockOffset(std::vector<uint32_t>(i), j), exception::BulkTransferOffsetRequestedForSingleRegister);
        }
      }
      break;

    case defs::NON_INCREMENTAL :
      for (size_t i=0; i <= 0xF; i++) {
        for (size_t j=0; j <= 0xF; j++) {
          BOOST_CHECK_THROW(aNode.readBlockOffset(i, j), exception::BulkTransferOffsetRequestedForFifo);
          BOOST_CHECK_THROW(aNode.writeBlockOffset(std::vector<uint32_t>(i), j), exception::BulkTransferOffsetRequestedForFifo);
        }
      }
      break;

    case defs::INCREMENTAL :
      for (size_t lOffset=0; lOffset <= 0xF; lOffset++) {
        for (size_t n = aNode.getSize() + 1 - lOffset; n <= aNode.getSize() + 0xF - lOffset; n++) {
          BOOST_CHECK_THROW(aNode.readBlockOffset(n, lOffset), exception::BulkTransferRequestedTooLarge);
          BOOST_CHECK_THROW(aNode.writeBlockOffset(std::vector<uint32_t>(n), lOffset), exception::BulkTransferRequestedTooLarge);
        }
      }
      break;

    case defs::HIERARCHICAL :
      break;
  }
}


void checkDescendants (const uhal::Node& aNode, const NodeFixture::Properties& aProperties)
{
  // 1) Given an empty ID string, 'getNode' should return same object
  BOOST_CHECK_EQUAL(&aNode.getNode(""), &aNode);

  // 2) 'getNode' should throw given an invalid ID
//FIXME - uncomment later    BOOST_CHECK_THROW(aNode.getNode("."), exception::NoBranchFoundWithGivenUID);
  BOOST_CHECK_THROW(aNode.getNode("some_invalid_id"), exception::NoBranchFoundWithGivenUID);

  // 3) 'getNodes' should return relative IDs of all descendants (order is not defined)
  std::vector<std::string> lReturnedIds(aNode.getNodes());
  BOOST_CHECK_EQUAL(lReturnedIds.size(), aProperties.descendantIds.size());
  std::vector<std::string> lExpectedIds(aProperties.descendantIds);
  std::sort(lReturnedIds.begin(), lReturnedIds.end());
  std::sort(lExpectedIds.begin(), lExpectedIds.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(lReturnedIds.begin(), lReturnedIds.end(), lExpectedIds.begin(), lExpectedIds.end());

  for (std::vector<std::string>::const_iterator lIt=lReturnedIds.begin(); lIt != lReturnedIds.end(); lIt++) {
    const std::type_info& lType = *aProperties.descendantTypes.at(*lIt);

    // 4) Each descendant should be accessible through 'getNode' method
    BOOST_CHECK_NO_THROW(aNode.getNode(*lIt));
    BOOST_CHECK(typeid(aNode.getNode(*lIt)) == lType);
    BOOST_CHECK_EQUAL(&aNode.getNode(*lIt), &aNode.getNode(*lIt).getNode(""));

    // 5) Templated 'getNode' method should return pointer to same object instance as non-templated method, but throw for invalid casts
    BOOST_CHECK_EQUAL(&aNode.getNode<Node>(*lIt), &aNode.getNode(*lIt));
    if (lType == typeid(Node)) {
      BOOST_CHECK_THROW(aNode.getNode<DummyParentNode>(*lIt), exception::BadNodeCast);
      BOOST_CHECK_THROW(aNode.getNode<DummyChildNode>(*lIt), exception::BadNodeCast);
    }
    else if (lType == typeid(DummyParentNode)) {
      BOOST_CHECK_EQUAL(static_cast<const Node*>(&aNode.getNode<DummyParentNode>(*lIt)), &aNode.getNode(*lIt));
      BOOST_CHECK_THROW(aNode.getNode<DummyChildNode>(*lIt), exception::BadNodeCast);
    }
    else if (lType == typeid(DummyChildNode)) {
      BOOST_CHECK_EQUAL(static_cast<const Node*>(&aNode.getNode<DummyParentNode>(*lIt)), &aNode.getNode(*lIt));
      BOOST_CHECK_EQUAL(static_cast<const Node*>(&aNode.getNode<DummyChildNode>(*lIt)), &aNode.getNode(*lIt));
    }
    else {
      BOOST_CHECK_MESSAGE(false, "Unit tests have not been written to handle node class '" << aProperties.type->name() << "'");
    }

    // 6) For each descendant, the same node object should be returned by calling 'getNode' once using relative ID path, or twice using 2 parts of relative ID path
    size_t lPositionOfDot(lIt->find("."));
    while (lPositionOfDot != std::string::npos) {
      std::string lPathPart1(lIt->substr(0, lPositionOfDot));
      std::string lPathPart2(lIt->substr(lPositionOfDot+1));

      BOOST_CHECK_EQUAL(&aNode.getNode(*lIt), &aNode.getNode(lPathPart1).getNode(lPathPart2));

      lPositionOfDot = lIt->find(".", lPositionOfDot + 1);
    }
  }

  // TODO: Add test that vector returned by getNodes("someRgex") is correct
}


bool nodeAddrCompare (const Node* aNodeL, const Node* aNodeR)
{
  return ( aNodeL->getAddress() < aNodeR->getAddress() );
}


void checkIteration(const uhal::Node& aNode, const NodeFixture::Properties& aProperties)
{
  // Check that sequence of nodes returned by iterator is correct
  std::vector<const uhal::Node*> lExpectedNodes;
  lExpectedNodes.push_back(&aNode);
  for (std::vector<std::string>::const_iterator lIt = aProperties.descendantIds.begin(); lIt != aProperties.descendantIds.end(); lIt++)
    lExpectedNodes.push_back(&aNode.getNode(*lIt));
  std::stable_sort(lExpectedNodes.begin(), lExpectedNodes.end(), nodeAddrCompare);

  size_t lItCount = 0;
  for (uhal::Node::const_iterator lIt = aNode.begin(); lIt != aNode.end(); lIt++, lItCount++) {
    if (lItCount < lExpectedNodes.size())
      BOOST_CHECK_EQUAL(&*lIt, lExpectedNodes.at(lItCount));
    else
      BOOST_CHECK(false);
  }
  BOOST_CHECK_EQUAL(lItCount, lExpectedNodes.size());
}


void checkNodeTree(const uhal::Node& aNode, const std::vector<NodeFixture::Properties>& aExpectedProperties)
{
  for (std::vector<NodeFixture::Properties>::const_iterator lIt = aExpectedProperties.begin(); lIt != aExpectedProperties.end(); lIt++) {
    BOOST_TEST_MESSAGE("Node '" << lIt->path << "'");
    const uhal::Node& lNode = (lIt->path.empty() ? aNode : aNode.getNode(lIt->path));

    checkProperties(lNode, *lIt);
    checkExceptionsThrownByReadWrite(lNode, *lIt);
    checkDescendants(lNode, *lIt);
    checkIteration(lNode, *lIt);
  }
}



BOOST_AUTO_TEST_SUITE( nodes )

BOOST_AUTO_TEST_SUITE( valid )

BOOST_FIXTURE_TEST_CASE (baseline, NodeFixture) {
  const boost::shared_ptr<uhal::Node> lTopNode(NodeTreeBuilder::getInstance().getNodeTree(addrFileURI, boost::filesystem::current_path() / "."));

  checkNodeTree(*lTopNode, nodeProperties);
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE_END()

}
}
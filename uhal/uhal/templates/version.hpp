#ifndef _uhal_version_hpp_
#define _uhal_version_hpp_

#include <chrono>
#include <cstddef>
#include <string>
#include <utility>

#include "boost/optional.hpp"
#include "boost/variant.hpp"


#define UHAL_VERSION_MAJOR __PROJECT_VERSION_MAJOR__
#define UHAL_VERSION_MINOR __PROJECT_VERSION_MINOR__
#define UHAL_VERSION_PATCH __PROJECT_VERSION_PATCH__

#define UHAL_VERSION (10000 * UHAL_VERSION_MAJOR + 100 * UHAL_VERSION_MINOR + UHAL_VERSION_PATCH)


namespace uhal {

// Based on swatch::phase2::PackageInfo
struct PackageInfo {

  struct Version {
    size_t major;
    size_t minor;
    size_t patch;
    boost::optional<std::string> prerelease;
  };

  struct Git {
    enum RefType {
      kTag,
      kBranch
    };

    std::string sha;
    bool clean;
    boost::optional<std::pair<RefType, std::string>> ref;
  };

  struct LocalBuild {
    size_t epochTime;
    std::string hostname;
    std::string username;
  };

  struct GitLabBuild {
    size_t epochTime;
    std::string serverURL;
    std::string projectPath;
    size_t projectID;
    size_t pipelineID;
    size_t jobID;
  };

  Version version;

  // Information from version control system
  boost::optional<Git> vcs;

  // Build info
  boost::variant<LocalBuild, GitLabBuild> build;
};

const PackageInfo::Version& getVersion();

const PackageInfo& getPackageInfo();

} // namespace uhal

#endif /* _uhal_version_hpp_ */
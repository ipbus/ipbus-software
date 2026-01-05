

#include "uhal/version.hpp"


// Copied from SWATCH
namespace {

uhal::PackageInfo createPackageInfo()
{
  using uhal::PackageInfo;
  PackageInfo lInfo;

  lInfo.version.major = __PROJECT_VERSION_MAJOR__;
  lInfo.version.minor = __PROJECT_VERSION_MINOR__;
  lInfo.version.patch = __PROJECT_VERSION_PATCH__;

#if __IN_GIT_REPO__
  lInfo.vcs = PackageInfo::Git();
  lInfo.vcs->sha = "__GIT_SHA__";
  lInfo.vcs->clean = __GIT_IS_REPO_CLEAN__;

#if __GIT_IS_BRANCH_CHECKED_OUT__
  lInfo.vcs->ref = std::pair<PackageInfo::Git::RefType, std::string>(PackageInfo::Git::kBranch, "__GIT_BRANCH_NAME__");
#endif

#if __GIT_IS_TAG_CHECKED_OUT__
  lInfo.vcs->ref = std::pair<PackageInfo::Git::RefType, std::string>(PackageInfo::Git::kTag, "__GIT_TAG_NAME__");
#endif

#endif

#if __IN_GITLAB_CI_JOB__
  PackageInfo::GitLabBuild lBuild;
  lBuild.serverURL = "__CI_SERVER_URL__";
  lBuild.projectPath = "__CI_PROJECT_PATH__";
  lBuild.projectID = __CI_PROJECT_ID__;
  lBuild.pipelineID = __CI_PIPELINE_ID__;
  lBuild.jobID = __CI_JOB_ID__;
#else
  PackageInfo::LocalBuild lBuild;
  lBuild.hostname = "__LOCAL_BUILD_HOSTNAME__";
  lBuild.username = "__LOCAL_BUILD_USERNAME__";
#endif
  lBuild.epochTime = __BUILDTIME_SECONDS_SINCE_EPOCH__;
  lInfo.build = lBuild;

  return lInfo;
}

const uhal::PackageInfo kPackageInfo = createPackageInfo();

} // namespace (unnamed)


namespace uhal {

const PackageInfo::Version& getVersion()
{
  return kPackageInfo.version;
}


const PackageInfo& getPackageInfo()
{
  return kPackageInfo;
}

} // namespace uhal

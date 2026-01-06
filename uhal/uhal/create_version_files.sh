
PROJECT_VERSION_MAJOR=$1
PROJECT_VERSION_MINOR=$2
PROJECT_VERSION_PATCH=$3

if [ $# -ne "3" ]; then
  echo "ERROR: Invalid number of arguments!"
  echo "usage: $0 MAJOR_VERSION MINOR_VERSION PATCH_VERSION"
  exit 1
fi


# 1. Get path to template file
SH_SOURCE=${BASH_SOURCE}
VERSION_HPP_TEMPLATE=$(cd $(dirname ${SH_SOURCE}) && pwd)/templates/version.hpp
VERSION_CPP_TEMPLATE=$(cd $(dirname ${SH_SOURCE}) && pwd)/templates/version.cpp


# 2a. Complex defintions for replacements: git repo info
if [ "$(git rev-parse --is-inside-work-tree 2>/dev/null)" = "true" ]; then
    IN_GIT_REPO=1
else
    IN_GIT_REPO=0
fi

git diff --quiet HEAD
if [ "$?" != "0" ]; then
    GIT_IS_REPO_CLEAN=0
else
    GIT_IS_REPO_CLEAN=1
fi

GIT_REF_NAME=$(git rev-parse --abbrev-ref HEAD)
GIT_IS_BRANCH_CHECKED_OUT=0
GIT_IS_TAG_CHECKED_OUT=0
if [ "${GIT_REF_NAME}" = "HEAD" ]; then
    GIT_TAG_NAME=$(git describe --exact-match ${GIT_REF_NAME} 2> /dev/null)
    if [ "$?" == "0" ]; then
        GIT_IS_TAG_CHECKED_OUT=1
    fi
else
    GIT_BRANCH_NAME=${GIT_REF_NAME}
    GIT_IS_BRANCH_CHECKED_OUT=1
fi


# 2b. Complex defintions for replacements: GitLab CI info
if [ "${GITLAB_CI}" == "true" ]; then
    IN_GITLAB_CI_JOB=1
else
    IN_GITLAB_CI_JOB=0
    LOCAL_BUILD_HOSTNAME=$(hostname)
    LOCAL_BUILD_USERNAME=$(whoami)
fi


# 3. Create version.hpp & version.cpp files from template, placing them in temporary area
TEMPORARY_VERSION_HPP_PATH=/tmp/version.hpp
TEMPORARY_VERSION_CPP_PATH=/tmp/version.cpp

sed -e "s#__PROJECT_VERSION_MAJOR__#${PROJECT_VERSION_MAJOR}#" \
    -e "s#__PROJECT_VERSION_MINOR__#${PROJECT_VERSION_MINOR}#" \
    -e "s#__PROJECT_VERSION_PATCH__#${PROJECT_VERSION_PATCH}#" \
    ${VERSION_HPP_TEMPLATE} > ${TEMPORARY_VERSION_HPP_PATH}

sed -e "s#__PROJECT_VERSION_MAJOR__#${PROJECT_VERSION_MAJOR}#" \
    -e "s#__PROJECT_VERSION_MINOR__#${PROJECT_VERSION_MINOR}#" \
    -e "s#__PROJECT_VERSION_PATCH__#${PROJECT_VERSION_PATCH}#" \
    \
    -e "s#__IN_GIT_REPO__#${IN_GIT_REPO}#" \
    -e "s#__GIT_SHA__#$(git rev-parse HEAD)#" \
    -e "s#__GIT_IS_REPO_CLEAN__#${GIT_IS_REPO_CLEAN}#" \
    -e "s#__GIT_IS_BRANCH_CHECKED_OUT__#${GIT_IS_BRANCH_CHECKED_OUT}#" \
    -e "s#__GIT_BRANCH_NAME__#${GIT_BRANCH_NAME}#" \
    -e "s#__GIT_IS_TAG_CHECKED_OUT__#${GIT_IS_TAG_CHECKED_OUT}#" \
    -e "s#__GIT_TAG_NAME__#${GIT_TAG_NAME}#" \
     \
    -e "s#__IN_GITLAB_CI_JOB__#${IN_GITLAB_CI_JOB}#" \
    -e "s#__CI_SERVER_URL__#${CI_SERVER_URL}#" \
    -e "s#__CI_PROJECT_PATH__#${CI_PROJECT_PATH}#" \
    -e "s#__CI_PROJECT_ID__#${CI_PROJECT_ID}#" \
    -e "s#__CI_PIPELINE_ID__#${CI_PIPELINE_ID}#" \
    -e "s#__CI_JOB_ID__#${CI_JOB_ID}#" \
    \
    -e "s#__LOCAL_BUILD_HOSTNAME__#${LOCAL_BUILD_HOSTNAME}#" \
    -e "s#__LOCAL_BUILD_USERNAME__#${LOCAL_BUILD_USERNAME}#" \
    ${VERSION_CPP_TEMPLATE} > ${TEMPORARY_VERSION_CPP_PATH}


# 4. Copy version.cpp from temporary area to final destination, if there are any changes
TARGET_VERSION_HPP_PATH=$(cd $(dirname ${SH_SOURCE}) && pwd)/include/uhal/version.hpp
TARGET_VERSION_CPP_PATH=$(cd $(dirname ${SH_SOURCE}) && pwd)/src/common/version.cpp


diff -q ${TEMPORARY_VERSION_HPP_PATH} ${TARGET_VERSION_HPP_PATH} > /dev/null 2>&1
if [ "$?" != "0" ]; then
    echo "${TARGET_VERSION_HPP_PATH} is missing or old. Updating it!"
    cp ${TEMPORARY_VERSION_HPP_PATH} ${TARGET_VERSION_HPP_PATH}
else
    echo "${TARGET_VERSION_HPP_PATH} is already up to date"
fi

diff --ignore-matching-lines='^.*lBuild.epochTime.*$' -q ${TEMPORARY_VERSION_CPP_PATH} ${TARGET_VERSION_CPP_PATH} > /dev/null 2>&1
if [ "$?" != "0" ]; then
    echo "${TARGET_VERSION_CPP_PATH} is missing or old. Updating it!"
    cp ${TEMPORARY_VERSION_CPP_PATH} ${TARGET_VERSION_CPP_PATH}
    sed -i -e "s#__BUILDTIME_SECONDS_SINCE_EPOCH__#$(date +%s)#" ${TARGET_VERSION_CPP_PATH}
else
    echo "${TARGET_VERSION_CPP_PATH} is already up to date"
fi

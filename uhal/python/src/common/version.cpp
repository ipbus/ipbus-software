#include "uhal/pycohal/version.hpp"

#include "pybind11/stl.h" // Automatically adds converters for STL collection/map classes
#include "pybind11/pybind11.h"

#include "uhal/version.hpp"


// Define `boost::variant` type caster, for C++ to Python conversion.
// Copied from pybind11 docs - https://pybind11.readthedocs.io/en/stable/advanced/cast/stl.html
namespace PYBIND11_NAMESPACE { namespace detail {
    template <typename... Ts>
    struct type_caster<boost::variant<Ts...>> : variant_caster<boost::variant<Ts...>> {};

    // Specifies the function used to visit the variant -- `apply_visitor` instead of `visit`
    template <>
    struct visit_helper<boost::variant> {
        template <typename... Args>
        static auto call(Args &&...args) -> decltype(boost::apply_visitor(args...)) {
            return boost::apply_visitor(args...);
        }
    };
}} // namespace PYBIND11_NAMESPACE::detail

// Define optional-type type_caster for boost::optional
namespace pybind11 {
namespace detail {
template <typename T>
struct type_caster<boost::optional<T>> : optional_caster<boost::optional<T>> {};
}
}


namespace py = pybind11;

void pycohal::wrap_version_and_build_info(pybind11::module_& aModule)
{
  using uhal::PackageInfo;

  auto packageInfoClass = py::class_<PackageInfo>(aModule, "PackageInfo");

  py::class_<PackageInfo::Version>(packageInfoClass, "Version")
    .def_readonly("major", &PackageInfo::Version::major)
    .def_readonly("minor", &PackageInfo::Version::minor)
    .def_readonly("patch", &PackageInfo::Version::patch)
    .def_readonly("prerelease", &PackageInfo::Version::prerelease);

  auto gitInfoClass = py::class_<PackageInfo::Git>(packageInfoClass, "Git");
  py::enum_<PackageInfo::Git::RefType>(gitInfoClass, "RefType")
    .value("Tag", PackageInfo::Git::kTag)
    .value("Branch", PackageInfo::Git::kBranch)
    .export_values();

  gitInfoClass
    .def_readonly("sha", &PackageInfo::Git::sha)
    .def_readonly("clean", &PackageInfo::Git::clean)
    .def_readonly("ref", &PackageInfo::Git::ref);

  py::class_<PackageInfo::LocalBuild>(packageInfoClass, "LocalBuild")
    .def_readonly("_epochTime", &PackageInfo::LocalBuild::epochTime)
    .def_readonly("hostname", &PackageInfo::LocalBuild::hostname)
    .def_readonly("username", &PackageInfo::LocalBuild::username);

  py::class_<PackageInfo::GitLabBuild>(packageInfoClass, "GitLabBuild")
    .def_readonly("_epochTime", &PackageInfo::GitLabBuild::epochTime)
    .def_readonly("serverURL", &PackageInfo::GitLabBuild::serverURL)
    .def_readonly("projectPath", &PackageInfo::GitLabBuild::projectPath)
    .def_readonly("projectID", &PackageInfo::GitLabBuild::projectID)
    .def_readonly("pipelineID", &PackageInfo::GitLabBuild::pipelineID)
    .def_readonly("jobID", &PackageInfo::GitLabBuild::jobID);

  packageInfoClass
    .def_readonly("version", &PackageInfo::version)
    .def_readonly("vcs", &PackageInfo::vcs)
    .def_readonly("build", &PackageInfo::build);

  aModule.def("getVersion", &uhal::getVersion);
  aModule.def("getPackageInfo", &uhal::getPackageInfo);
}

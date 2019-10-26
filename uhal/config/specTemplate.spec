%define _package __package__
%define _packagename __packagename__
%define _version __version__
%define _release __release__
%define _prefix  __prefix__
%define _sources_dir __sources_dir__
%define _tmppath /tmp
%define _packagedir __packagedir__
%define _os __os__
%define _platform __platform__
%define _project __project__
%define _author __author__
%define _summary __summary__
%define _url __url__
#%define _buildarch __buildarch__
#%define _includedirs __includedirs__

#
# SWATCH Specfile template
# References: 
# cmsos template file: https://svnweb.cern.ch/trac/cmsos/browser/trunk/config/spec.template
# cmsos RPM rules: https://svnweb.cern.ch/trac/cmsos/browser/trunk/config/mfRPM.rules
# Notice the _require and __require__ interplay with PACKAGE_REQUIRED_PACKAGE_LIST
#
Name: %{_packagename} 
Version: %{_version} 
Release: %{_release} 
Packager: %{_author}
Summary: %{_summary}
License: GPLv3
Source: %{_source}
URL: %{_url} 
BuildArch: 
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot 
Prefix: %{_prefix}

# Dependencies
BuildRequires: __build_requires__
Requires: __requires__


%description
__description__

#
# Devel RPM specified attributes (extension to binary rpm with include files)
#
%if %{defined _build_debuginfo_package}
%package -n %{_packagename}-debuginfo
Summary:  Debuginfo package for %{_summary}

%description -n %{_packagename}-debuginfo
__description__
%endif

#%prep

#%build

%install 
# copy files to RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/%{_prefix}/{bin,lib,include,etc}
mkdir -p $RPM_BUILD_ROOT/usr/lib/debug%{_prefix}/{bin,lib}
mkdir -p $RPM_BUILD_ROOT/usr/src/debug/%{_packagename}-%{_version}/

PKGDIR=%{_packagedir}
%if %{defined _build_debuginfo_package}
DEBUGDIR=/usr/src/debug/%{_packagename}-%{_version}
if [ "${#DEBUGDIR}" -gt "${#PKGDIR}" ]; then
  (>&2 echo "ERROR : Debuginfo destination directory path '${DEBUGDIR}' is shorter than source code directory path '${PKGDIR}'.")
  (>&2 echo "        Please make base directory path of source code $(expr ${#DEBUGDIR} - ${#PKGDIR}) characters longer in order to build RPMs.")
  exit 1
fi
%endif

if [ -d %{_packagedir}/bin ]; then
  cd %{_packagedir}/bin; \
# find . -name "*" -exec install -D -m 755 {} $RPM_BUILD_ROOT/%{_prefix}/bin/{} \;
  find . -type f -exec $BUILD_HOME/uhal/config/install.sh {} %{_prefix}/bin/%{_project}/{} 755 $RPM_BUILD_ROOT %{_packagedir} %{_packagename} %{_version} %{_prefix}/include '%{_includedirs}' \;
fi

%define versioned_python_command %(echo "$(python -c \"from sys import version_info; print('python' + str(version_info[0]))\")")
if [ -d %{_packagedir}/scripts ]; then
  cd %{_packagedir}/scripts; \
  find . -type f -exec install -D -m 755 {} $RPM_BUILD_ROOT/%{_prefix}/bin/%{_project}/{} \;
  find $RPM_BUILD_ROOT/%{_prefix}/bin/%{_project}/ -type f -exec sed -i "s|#!/usr/bin/env python|#!/usr/bin/env "%{versioned_python_command}"|" {} \;
fi

if [ -d %{_packagedir}/lib ]; then
  cd %{_packagedir}/lib; \
#  find . -name ".svn" -prune -o -name "*" -exec install -D -m 644 {} $RPM_BUILD_ROOT/%{_prefix}/lib/{} \;
  find . -type f -exec $BUILD_HOME/uhal/config/install.sh {} %{_prefix}/lib/{} 755 $RPM_BUILD_ROOT %{_packagedir} %{_packagename} %{_version} %{_prefix}/include '%{_includedirs}' \;
  find . -type l -exec sh -c 'ln -s -f %{_prefix}/lib/$(basename $(readlink $0)) $RPM_BUILD_ROOT/%{_prefix}/lib/$0' {} \;
fi


if [ -d %{_packagedir}/include ]; then
  cd %{_packagedir}/include; \
  find . \( -name "*.hpp"  -o -name "*.hxx" \)  -exec install -D -m 644 {} $RPM_BUILD_ROOT/%{_prefix}/include/{} \;
fi


if [ -d %{_packagedir}/etc ]; then
  cd %{_packagedir}/etc; \
  find . -type f -exec install -D -m 644 {} $RPM_BUILD_ROOT/%{_prefix}/etc/{} \;
fi

#cp -rp %{_sources_dir}/* $RPM_BUILD_ROOT%{_prefix}/.


#create debug.source - SLC6 beardy wierdo "feature"
%if %{defined _build_debuginfo_package}
cd %{_packagedir}
#find src -name '*.cpp' -o -name '*.cxx' -fprintf rpm/debug.source "%p\0"
#find src include -name '*.h' -print > rpm/debug.source -o -name '*.cc' -print > rpm/debug.source

cat %{_packagedir}/rpm/debug.source | sort -z -u | egrep -v -z '(<internal>|<built-in>)$' | egrep -v -z %{_packagedir} >  %{_packagedir}/rpm/debug.source.clean
# Copy all sources and include files for debug RPMs
cat  %{_packagedir}/rpm/debug.source.clean | ( cpio -pd0mL --quiet "$RPM_BUILD_ROOT/usr/src/debug/%{_packagename}-%{_version}" )

#cat %{_packagedir}/rpm/debug.source | sort -z -u | egrep -v -z '(<internal>|<built-in>)$' | ( cpio -pd0mL --quiet "$RPM_BUILD_ROOT/usr/src/debug/%{_packagename}-%{_version}" )
#cat %{_packagedir}/rpm/debug.include | sort -z -u | egrep -v -z '(<internal>|<built-in>)$' | ( cpio -pd0mL --quiet "$RPM_BUILD_ROOT/usr/src/debug/%{_packagename}-%{_version}" )
# correct permissions on the created directories
cd "$RPM_BUILD_ROOT/usr/src/debug/"
find ./ -type d -exec chmod 755 {} \;
%endif


%clean

%post 

%postun 

%files 
%defattr(-, root, root, -) 
%{_prefix}/bin
%{_prefix}/lib
%{_prefix}/etc
%{_prefix}/include

# Temporary workaround for subpackages not being built on CentOS8
%if %{defined _build_debuginfo_package}
%if %{_os} == centos8
/usr/lib/debug
/usr/src/debug
%endif
%endif


#
# Files that go in the debuginfo RPM
#
%if %{defined _build_debuginfo_package}
%files -n %{_packagename}-debuginfo
%defattr(-,root,root,-)
/usr/lib/debug
/usr/src/debug
%endif
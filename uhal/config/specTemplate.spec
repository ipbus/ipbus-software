%define _package __package__
%define _packagename __packagename__
%define _version __version__
%define _release __release__
%define _prefix  __prefix__
%define _sources_dir __sources_dir__
%define _tmppath /tmp
%define _packagedir __packagedir__
%define _project __project__
%define _author __author__
%define _summary __summary__
%define _url __url__
%define _python_versioned_command __python_versioned_command__


%define percent %( echo "%" )


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
%package debuginfo
Summary: Debuginfo package for %{_summary}

%description debuginfo
__description__
%endif


%prep


%build


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

# Change working directory to avoid "shell-init: error retrieving current directory: getcwd: cannot access parent directories: No such file or directory" error
cd $RPM_BUILD_ROOT

if [ -d %{_packagedir}/bin ]; then
  find %{_packagedir}/bin -type f -printf '%{percent}P\0' | xargs -0 -I {} $BUILD_HOME/uhal/config/install.sh %{_packagedir}/bin/{} %{_prefix}/bin/%{_project}/{} 755 $RPM_BUILD_ROOT %{_packagedir} %{_packagename} %{_version} %{_prefix}/include '%{_includedirs}'
fi

if [ -d %{_packagedir}/scripts ]; then
  find %{_packagedir}/scripts -type f -printf '%{percent}P\0' | xargs -0 -I {} install -D -m 755 %{_packagedir}/scripts/{} $RPM_BUILD_ROOT/%{_prefix}/bin/%{_project}/{}
  find $RPM_BUILD_ROOT/%{_prefix}/bin/%{_project} -type f -print0 | xargs -0 -I {} sed -i "s|#!/usr/bin/env python|#!/usr/bin/env "%{_python_versioned_command}"|" {}
fi

if [ -d %{_packagedir}/lib ]; then
  find %{_packagedir}/lib -type f -printf '%{percent}P\0' | xargs -0 -I {} $BUILD_HOME/uhal/config/install.sh %{_packagedir}/lib/{} %{_prefix}/lib/{} 655 $RPM_BUILD_ROOT %{_packagedir} %{_packagename} %{_version} %{_prefix}/include '%{_includedirs}' \;
  find %{_packagedir}/lib -type l -printf '%{percent}P\0' | xargs -0 -I {} sh -c 'ln -s -f %{_prefix}/lib/$(basename $(readlink %{_packagedir}/lib/$0)) $RPM_BUILD_ROOT/%{_prefix}/lib/$0' {}
fi


if [ -d %{_packagedir}/include ]; then
  find %{_packagedir}/include -type f -printf '%{percent}P\0' | xargs -0 -I {} install -D -m 644 %{_packagedir}/include/{} $RPM_BUILD_ROOT/%{_prefix}/include/{}
fi


if [ -d %{_packagedir}/etc ]; then
  find %{_packagedir}/etc -type f -printf '%{percent}P\0' | xargs -0 -I {} install -D -m 644 %{_packagedir}/etc/{} $RPM_BUILD_ROOT/%{_prefix}/etc/{}
fi

#create debug.source - SLC6 beardy wierdo "feature"
%if %{defined _build_debuginfo_package}
cd %{_packagedir}

cat %{_packagedir}/rpm/debug.source | sort -z -u | grep -E -v -z '(<internal>|<built-in>)$' | grep -E -v -z %{_packagedir} >  %{_packagedir}/rpm/debug.source.clean

# Copy all sources and include files for debug RPMs
cat  %{_packagedir}/rpm/debug.source.clean | ( cpio -pd0mL --quiet "$RPM_BUILD_ROOT/usr/src/debug/%{_packagename}-%{_version}" )

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

%if %{defined _build_debuginfo_package}
# Temporary workaround for subpackages not being built on RHEL8, CentOS8, RHEL9, Alma9 etc.
%if "%{dist}" != ".el8" && "%{dist}" != ".el9"
%files debuginfo
%defattr(-,root,root,-)
%endif

/usr/lib/debug
/usr/src/debug
%endif

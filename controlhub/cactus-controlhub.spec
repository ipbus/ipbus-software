#
# spec file for controlhub
#

Name: %{name}
Summary: IPbus packet-router
Version: %{version}
Release: %{release}
License: BSD License
URL: http://www.erlang.org/
Group: CACTUS
Source: %{tarball_file}
Requires: cactus-extern-erlang
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot

%description
IPbus packet-router

%prep

%build

%install
curdir=`pwd`
rm -rf $RPM_BUILD_ROOT

# copy lib content to RPM_BUILD_ROOT
echo "I AM HERE"
mkdir -p $RPM_BUILD_ROOT%{_prefix}/lib
cp -rp %{sources_dir}/lib/* $RPM_BUILD_ROOT%{_prefix}/lib/.

# copy releases content to RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT%{_prefix}/releases
cp -rp %{sources_dir}/releases/* $RPM_BUILD_ROOT%{_prefix}/releases/.

#Change access rights
chmod -R 755 $RPM_BUILD_ROOT%{_prefix}/lib
chmod -R 755 $RPM_BUILD_ROOT%{_prefix}/releases

#return to working directory
cd $curdir

%clean


%post

%postun

%files
%defattr(-, root, root)
%{_prefix}/lib/*
%{_prefix}/releases/*

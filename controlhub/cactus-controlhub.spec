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
# copy bin content to RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT%{_prefix}/bin
cp -rp %{sources_dir}/bin/* $RPM_BUILD_ROOT%{_prefix}/bin/.

# copy lib content to RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT%{_prefix}/lib
cp -rp %{sources_dir}/lib/* $RPM_BUILD_ROOT%{_prefix}/lib/.

# Now update the escript executable paths in various controlhub scripts
cd $RPM_BUILD_ROOT%{_prefix}/bin
sed -i "s|/bin/env escript|%{_prefix}/bin/escript|" controlhub_*
cd $curdir

#Change access rights
chmod -R 744 $RPM_BUILD_ROOT%{_prefix}/bin
chmod 755 $RPM_BUILD_ROOT%{_prefix}/bin/controlhub_appmon
chmod 755 $RPM_BUILD_ROOT%{_prefix}/bin/controlhub_stats
chmod 755 $RPM_BUILD_ROOT%{_prefix}/bin/controlhub_status
chmod -R 755 $RPM_BUILD_ROOT%{_prefix}/lib


%clean


%post

%postun

%files
%defattr(-, root, root)
%{_prefix}/bin/*
%{_prefix}/lib/*

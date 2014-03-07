#
# spefile for uHAL Library Tests
#
Name: %{name} 
Version: %{version} 
Release: %{release} 
Packager: %{packager}
Summary: uTCA HW Development Tools that depend on uHAL
License: BSD License
Group: CACTUS
Source: https://svnweb.cern.ch/trac/cactus/browser/trunk/uhal/tools
URL: https://svnweb.cern.ch/trac/cactus 
BuildArch: noarch
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot 
Prefix: %{_prefix}

%description
uTCA HW Development Tools that depend on uHAL

%prep

%build


%install 

# copy includes to RPM_BUILD_ROOT and set aliases
mkdir -p $RPM_BUILD_ROOT%{_prefix}
cp -rp %{sources_dir}/* $RPM_BUILD_ROOT%{_prefix}/.

#Change access rights
chmod -R 755 $RPM_BUILD_ROOT%{_prefix}/bin


%clean 

%post 

%postun 

%files 
%defattr(-, root, root,-) 
%{_prefix}/bin/*
%{_prefix}/etc/*

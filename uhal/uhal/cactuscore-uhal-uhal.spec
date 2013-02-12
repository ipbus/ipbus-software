#
# spefile for uHAL Library
#
Name: %{name} 
Version: %{version} 
Release: %{release} 
Packager: %{packager}
Summary: uHAL Library
License: BSD License
Group: CACTUS
Source: https://svnweb.cern.ch/trac/cactus/browser/trunk/uhal/uhal
URL: https://svnweb.cern.ch/trac/cactus 
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot 
Prefix: %{_prefix}

%description
uHAL Library

%prep

%build


%install 

# copy includes to RPM_BUILD_ROOT and set aliases
mkdir -p $RPM_BUILD_ROOT%{_prefix}/include
cp -rp %{sources_dir}/include/* $RPM_BUILD_ROOT%{_prefix}/include/.

# copy libs to RPM_BUILD_ROOT and set aliases
mkdir -p $RPM_BUILD_ROOT%{_prefix}/lib
cp -rp %{sources_dir}/lib/* $RPM_BUILD_ROOT%{_prefix}/lib/.

#Change access rights
chmod -R 755 $RPM_BUILD_ROOT%{_prefix}/lib
chmod -R 755 $RPM_BUILD_ROOT%{_prefix}/include

%clean 

%post 

%postun 

%files 
%defattr(-, root, root) 
%{_prefix}/lib/*
%{_prefix}/include/*


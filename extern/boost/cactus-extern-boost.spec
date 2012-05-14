#
# spefile for boost
#

%define License Boost Software License 
%define Link http://www.boost.org/

Name: %{name} 
Version: %{version} 
Release: %{release} 
Packager: %{packager}
Summary: Boost Library packaged for the CACTUS project
License: Boost Software License
Group: CACTUS
Source: https://svnweb.cern.ch/trac/cactus/browser/trunk
URL:  http://www.boost.org/
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot 
Prefix: %{_prefix}

%description
Boost Library packaged for the CACTUS project

%prep

%build


%install 
curdir=`pwd` 
rm -rf $RPM_BUILD_ROOT 

mkdir -p $RPM_BUILD_ROOT%{_prefix}/doc
cp -rp %{sources_dir}/doc/* $RPM_BUILD_ROOT%{_prefix}/doc/.
find $RPM_BUILD_ROOT%{_prefix}/doc -type d | xargs chmod 755 
find $RPM_BUILD_ROOT%{_prefix}/doc -type f | xargs chmod 644 

# copy includes to RPM_BUILD_ROOT and set aliases
mkdir -p $RPM_BUILD_ROOT%{_prefix}/include
cp -rp %{sources_dir}/include/* $RPM_BUILD_ROOT%{_prefix}/include/.

# copy libs to RPM_BUILD_ROOT and set aliases
mkdir -p $RPM_BUILD_ROOT%{_prefix}/lib
cp -rp %{sources_dir}/lib/* $RPM_BUILD_ROOT%{_prefix}/lib/.

#return to working directory
cd $curdir 


%clean 

%post 
#/sbin/ldconfig 

%postun 
#/sbin/ldconfig 

%files 
%defattr(-, root, root) 
%{_prefix}/lib/*
%{_prefix}/doc/*
%{_prefix}/include/*


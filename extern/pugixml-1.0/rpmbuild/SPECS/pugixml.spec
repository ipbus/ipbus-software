
###################################################################################################################################

%define name pugixml
%define version 1.0

%define tarball_name %{name}-%{version} 
%define underscore_version %(echo %{version} | sed -e 's/\\./_/g') 

###################################################################################################################################

Name: %{name} 
Summary: PugiXML : A light-weight, simple and fast XML parser for C++ with XPath support
Version: %{version}
Release: 0
License: MIT License 
URL: http://pugixml.org/
Group: System Environment/Libraries 
Source: %{tarball_name}.zip
BuildRoot: %{_tmppath}/boost-%{version}-root 

###################################################################################################################################

Prereq: /sbin/ldconfig 

###################################################################################################################################

%description 
PugiXML is a light-weight, simple and fast XML parser for C++ with XPath support

###################################################################################################################################

%prep

rm -rf  %{working_dir}/include
mkdir -p %{working_dir}/include/%{name} 
rm -rf  %{working_dir}/lib
mkdir %{working_dir}/lib

rm -rf %{name}-%{version} 
unzip ../SOURCES/%{tarball_name} -d %{name}-%{version} 

###################################################################################################################################

%build
cd %{name}-%{version} 
pwd
scons -f %{working_dir}/SConscript.build
cp *.so %{working_dir}/lib/
cp src/*.hpp %{working_dir}/include/%{name} 

###################################################################################################################################

%install 
curdir=`pwd` 
rm -rf $RPM_BUILD_ROOT 

# copy includes to RPM_BUILD_ROOT and set aliases
mkdir -p $RPM_BUILD_ROOT%{_includedir}/%{name}-%{underscore_version}
cd $RPM_BUILD_ROOT%{_includedir}
cp -rf  %{working_dir}/include/* .
ln -s %{name}-%{underscore_version}/%{name} ./%{name} 

# copy libs to RPM_BUILD_ROOT and set aliases
mkdir -p $RPM_BUILD_ROOT%{_libdir}
cd $RPM_BUILD_ROOT%{_libdir} 
cp -rf  %{working_dir}/lib/* .

#return to working directory
cd $curdir 

###################################################################################################################################

%clean 
rm -rf $RPM_BUILD_ROOT 

###################################################################################################################################

%post 
/sbin/ldconfig 

###################################################################################################################################

%postun 
/sbin/ldconfig 

###################################################################################################################################

%files 
%defattr(-, root, root) 
%{_libdir}/*
%{_includedir}/%{name} 
%{_includedir}/%{name}-%{underscore_version}

###################################################################################################################################

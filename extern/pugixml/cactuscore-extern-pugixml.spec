#
# spefile for PugiXML
#
Name: %{name} 
Version: %{version} 
Release: %{release} 
Packager: %{packager}
Summary: PugiXML - Light-weight, simple and fast XML parser for C++ with XPath support 
License: MIT License
Source: %{zip_file}
URL:  http://pugixml.org/
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot 
Prefix: %{_prefix}

%description
PugiXML - Light-weight, simple and fast XML parser for C++ with XPath support 

%prep

%build


%install 
curdir=$PWD
rm -rf $RPM_BUILD_ROOT 

# copy includes to RPM_BUILD_ROOT and set aliases
mkdir -p $RPM_BUILD_ROOT%{_prefix}/include
cp -rp %{sources_dir}/include/* $RPM_BUILD_ROOT%{_prefix}/include/.

# copy libs to RPM_BUILD_ROOT and set aliases
mkdir -p $RPM_BUILD_ROOT%{_prefix}/lib
cp -rp %{sources_dir}/lib/* $RPM_BUILD_ROOT%{_prefix}/lib/.

#Change access rights
chmod -R 755 $RPM_BUILD_ROOT%{_prefix}/lib
chmod -R 755 $RPM_BUILD_ROOT%{_prefix}/include

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
%{_prefix}/include/*


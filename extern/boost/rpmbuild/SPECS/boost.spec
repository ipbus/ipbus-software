
###################################################################################################################################

%define name boost
%define version 1.48.0 
%define release 2

%define underscore_version %(echo %{version} | sed -e 's/\\./_/g') 
%define short_underscore_version %(echo %{underscore_version} | sed -e 's/_[0-9]*$//') 
#%define last_version 1.47.0

%define tarball_name %{name}_%{underscore_version} 

###################################################################################################################################

Name: %{name} 
Summary: The Boost C++ Libraries 
Version: %{version} 
Release: %{release} 
License: Boost Software License 
URL: http://www.boost.org/ 
Group: System Environment/Libraries 
Source: %{tarball_name}.tar.bz2 
BuildRoot: %{_tmppath}/%{name}-%{version}-root 

###################################################################################################################################

Prereq: /sbin/ldconfig 

###################################################################################################################################

BuildRequires: libstdc++-devel bzip2-devel python

###################################################################################################################################

#Obsoletes: boost <= %{last_version} 
#Obsoletes: boost-devel <= %{last_version} 
#Obsoletes: boost-doc <= %{last_version} 
#Obsoletes: boost-python <= %{last_version} 

###################################################################################################################################

%description 
Boost provides free peer-reviewed portable C++ source libraries. The 
emphasis is on libraries which work well with the C++ Standard 
Library. One goal is to establish "existing practice" and provide 
reference implementations so that the Boost libraries are suitable for 
eventual standardization. (Some of the libraries have already been 
proposed for inclusion in the C++ Standards Committees upcoming C++ 
Standard Library Technical Report.) 

###################################################################################################################################

%prep
rm -rf %{working_dir}/doc
mkdir %{working_dir}/doc
rm -rf  %{working_dir}/include
mkdir %{working_dir}/include
rm -rf  %{working_dir}/lib
mkdir %{working_dir}/lib

###################################################################################################################################

%setup -n %{tarball_name} -q 

###################################################################################################################################

%build

#booststrap
./bootstrap.sh

#build and install include files and libs in working_dir
./b2 --without-mpi --prefix=%{working_dir}/ --libdir=%{working_dir}/lib variant=release link=shared threading=multi install

#copy doc files into working_dir
find . -name '*.html' -o -name '*.css' -o -name '*.htm' | cpio -p --make-directories %{working_dir}/doc

###################################################################################################################################

%install 
curdir=`pwd` 
rm -rf $RPM_BUILD_ROOT 

# copy docs to RPM_BUILD_ROOT and set permissions
mkdir -p $RPM_BUILD_ROOT%{_docdir}/%{name}-%{short_underscore_version}
cd $RPM_BUILD_ROOT%{_docdir}/%{name}-%{short_underscore_version}
cp -rf %{working_dir}/doc/* .
find . -type d | xargs chmod 755 
find . -type f | xargs chmod 644 

# copy includes to RPM_BUILD_ROOT and set aliases
mkdir -p $RPM_BUILD_ROOT%{_includedir}/%{name}-%{short_underscore_version}
cd $RPM_BUILD_ROOT%{_includedir}
cp -rf  %{working_dir}/include/* .
#ln -s %{name}-%{short_underscore_version}/%{name} ./%{name} 

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
#%{_includedir}/%{name}-%{short_underscore_version}
%{_docdir}/%{name}-%{short_underscore_version}

###################################################################################################################################

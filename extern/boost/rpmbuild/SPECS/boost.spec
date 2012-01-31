%define version 1.48.0 
%define underscore_version %(echo %{version} | sed -e 's/\\./_/g') 
%define short_underscore_version %(echo %{underscore_version} | sed -e 's/_[0-9]*$//') 
%define last_version 1.47.0
%define tarball_name boost_%{underscore_version} 
%define release 2 

%define python_arch %(python -c 'import os; print os.uname()[4]') 

Name: boost 
Summary: The Boost C++ Libraries 
Version: %{version} 
Release: %{release} 
License: Boost Software License 
URL: http://www.boost.org/ 
Group: System Environment/Libraries 
Source: %{tarball_name}.tar.bz2 
BuildRoot: %{_tmppath}/boost-%{version}-root 

Prereq: /sbin/ldconfig 

BuildRequires: libstdc++-devel bzip2-devel python

Obsoletes: boost <= %{last_version} 
Obsoletes: boost-devel <= %{last_version} 
Obsoletes: boost-doc <= %{last_version} 

Obsoletes: boost-python <= %{last_version} 


%description 
Boost provides free peer-reviewed portable C++ source libraries. The 
emphasis is on libraries which work well with the C++ Standard 
Library. One goal is to establish "existing practice" and provide 
reference implementations so that the Boost libraries are suitable for 
eventual standardization. (Some of the libraries have already been 
proposed for inclusion in the C++ Standards Committee's upcoming C++ 
Standard Library Technical Report.) 


%package devel 
Summary: The Boost C++ Headers 
Group: System Environment/Libraries 
Requires: boost = %{version}-%{release} 

Obsoletes: boost-python-devel <= %{last_version} 
Provides: boost-python-devel = %{version}-%{release} 


%description devel 
Headers and libraries for developing software that uses Boost C++. 


%package symlinks 
Summary: Compatibility Links to the Boost C++ %{version} Libraries 
Group: System Environment/Libraries 
Requires: boost = %{version}-%{release} 


%description symlinks 
Symllinks to the Boost %{version} libraries for compatibility with Boost 
RPMs distributed by Red Hat. 


%package devel-symlinks 
Summary: Unversioned Links to the Boost C++ %{version} Headers 
Group: System Environment/Libraries 
Requires: boost = %{version}-%{release} 
Requires: boost-devel = %{version}-%{release} 


%description devel-symlinks 
Unversioned symllinks to the Boost %{version} headers and libraries for 
compatibility with Boost RPMs distributed by Red Hat. 


%package doc 
Summary: The Boost C++ HTML docs 
Group: System Environment/Libraries 
Requires: boost = %{version}-%{release} 
Provides: boost-python-docs = %{version}-%{release} 


%description doc 
HTML documentation files for Boost C++ libraries 


%prep 
rm -rf $RPM_BUILD_ROOT 


%setup -n %{tarball_name} -q 


%ifarch i386 i486 i586 i686 
%define arch_args <cflags>-m32 <cxxflags>-m32 <linkflags>-m32 <linkflags>-L/usr/lib 
%else 
%ifarch x86_64 
%define arch_args <cflags>-m64 <cxxflags>-m64 <linkflags>-m64 <linkflags>-L/usr/lib64 
%endif 
%endif 


%build 
#if icu-config --ldflags ; then export ICU_PATH=/usr ICU_LINK=\"$(icu-config --ldflags | sed 's/-lpthread//g')\"; fi 
#without_libs='' 
#%if %{build_python} == 0 
#without_libs="$without_libs --without-python" 
#%endif 
## This is a hack. A better way needs to be devised for determining when 
## the local architecture does not match the target architecture. 
#%if %{python_arch} != %{_arch} 
#without_libs="$without_libs --without-iostreams" 
#%endif 
##build bjam 
#(cd tools/build/jam_src && ./build.sh) 
##build boost with bjam 
#BJAM=`find tools/build/jam_src/ -name bjam -a -type f` 
#PYTHON_VERSION=`python -V 2>&1 |sed 's,.* \([0-9]\.[0-9]\)\(\.[0-9]\)\?.*,\1,'` 
#PYTHON_FLAGS="-sPYTHON_ROOT=/usr -sPYTHON_VERSION=$PYTHON_VERSION" 
#$BJAM $PYTHON_FLAGS "-sTOOLS=gcc" "-sBUILD=release <threading>single/multi %{arch_args}" "--prefix=$RPM_BUILD_ROOT%{_prefix}" "--libdir=$RPM_BUILD_ROOT%{_libdir}" $without_libs install 
./bootstrap.sh
./b2 --without-mpi --layout=versioned --prefix=$RPM_BUILD_ROOT%{_prefix} --libdir=$RPM_BUILD_ROOT%{_libdir} variant=release link=shared threading=multi install
#--build-type=complete

%install 
curdir=`pwd` 
cd $RPM_BUILD_ROOT%{_includedir}
ln -s boost-%{short_underscore_version}/boost ./boost 


# Replace unversioned static and shared libraries with symlinks to the 
# versioned copy. The symlinks will be packaged separately for people who want 
# to use an unversioned Boost developer environment. This also prevents 
# ldconfig from complaining about duplicate shared libraries. 
# Create symlinks to the fully qualified library without the toolset name for 
# compatibility with other Boost RPMs.
 
cd $RPM_BUILD_ROOT%{_libdir} 
for f in libboost_*-%{short_underscore_version}.so ; do 
   unversioned_name=`echo $f | sed -e 's/-%{short_underscore_version}//'` 
   no_info_name=`echo $unversioned_name | sed -e 's/-gcc[0-9]*//'` 
   rm -f $unversioned_name 
   ln -s $f.%{version} $unversioned_name 
   ln -s $f.%{version} $no_info_name 
   ln -s $f.%{version} $no_info_name.%{version} 
   ln -s $f.%{version} $no_info_name.2
done 
for f in libboost_*-%{short_underscore_version}.a ; do 
   unversioned_name=`echo $f | sed -e 's/-%{short_underscore_version}//'` 
   no_info_name=`echo $unversioned_name | sed -e 's/-gcc//'` 
   rm -f $unversioned_name 
   ln -s $f $unversioned_name 
   ln -s $f $no_info_name
done 
cd $curdir 


#install doc files 
mkdir -p $RPM_BUILD_ROOT%{_docdir}/%{name}-%{version} 
#cp -a doc/html $RPM_BUILD_ROOT/%{_docdir}/%{name}-%{version} 
find . -name '*.html' -o -name '*.css' -o -name '*.htm' | cpio -p --make-directories $RPM_BUILD_ROOT/%{_docdir}/%{name}-%{version} 
find $RPM_BUILD_ROOT/%{_docdir}/%{name}-%{version} -type d | xargs chmod 755 
find $RPM_BUILD_ROOT/%{_docdir}/%{name}-%{version} -type f | xargs chmod 644 


%clean 
rm -rf $RPM_BUILD_ROOT 


%post 
/sbin/ldconfig 


%postun 
/sbin/ldconfig 


%files 
%defattr(-, root, root) 
%{_libdir}/*
%{_includedir}/boost 
%{_includedir}/boost-%{short_underscore_version}
%{_docdir}/boost-%{version} 





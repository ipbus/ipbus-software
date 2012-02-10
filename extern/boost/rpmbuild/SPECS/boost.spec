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
proposed for inclusion in the C++ Standards Committees upcoming C++ 
Standard Library Technical Report.) 


%prep
rm -rf ../../../doc
mkdir ../../../doc
rm -rf  ../../../include
mkdir ../../../include
rm -rf  ../../../lib
mkdir ../../../lib

%setup -n %{tarball_name} -q 

%build 
./bootstrap.sh
#./b2 --without-mpi --layout=versioned --prefix=$RPM_BUILD_ROOT%{_prefix} --libdir=$RPM_BUILD_ROOT%{_libdir} variant=release link=shared threading=multi install
./b2 --without-mpi --layout=versioned --prefix=../../../ --libdir=../../../lib variant=release link=shared threading=multi install
#./b2 --without-mpi --layout=versioned variant=release link=shared threading=multi stage
#find boost -name '*.h' -o -name '*.hh' -o -name '*.hpp' | cpio -p --make-directories ../../../include
#cp -rf stage/lib/* ../../../lib
find . -name '*.html' -o -name '*.css' -o -name '*.htm' | cpio -p --make-directories ../../../doc


%install 
curdir=`pwd` 
rm -rf $RPM_BUILD_ROOT 

mkdir -p $RPM_BUILD_ROOT%{_docdir}/%{name}-%{short_underscore_version}
cp -rf ../../../doc/* $RPM_BUILD_ROOT%{_docdir}/%{name}-%{short_underscore_version}

mkdir -p $RPM_BUILD_ROOT%{_includedir}
cp -rf  ../../../include/* $RPM_BUILD_ROOT%{_includedir}

mkdir -p $RPM_BUILD_ROOT%{_libdir}
cp -rf  ../../../lib/* $RPM_BUILD_ROOT%{_libdir}

cd $RPM_BUILD_ROOT%{_includedir}
ln -s %{name}-%{short_underscore_version}/%{name} ./%{name} 

# Replace unversioned static and shared libraries with symlinks to the 
# versioned copy. The symlinks will be packaged separately for people who want 
# to use an unversioned Boost developer environment. This also prevents 
# ldconfig from complaining about duplicate shared libraries. 
# Create symlinks to the fully qualified library without the toolset name for 
# compatibility with other Boost RPMs.
 
cd $RPM_BUILD_ROOT%{_libdir} 
for f in lib%{name}_*-%{short_underscore_version}.so ; do 
   unversioned_name=`echo $f | sed -e 's/-%{short_underscore_version}//'` 
   no_info_name=`echo $unversioned_name | sed -e 's/-gcc[0-9]*-mt//'` 
   rm -f $unversioned_name 
   ln -s $f.%{version} $unversioned_name 
   ln -s $f.%{version} $no_info_name 
   ln -s $f.%{version} $no_info_name.%{version} 
   ln -s $f.%{version} $no_info_name.2
done 
for f in lib%{name}_*-%{short_underscore_version}.a ; do 
   unversioned_name=`echo $f | sed -e 's/-%{short_underscore_version}//'` 
   no_info_name=`echo $unversioned_name | sed -e 's/-gcc//'` 
   rm -f $unversioned_name 
   ln -s $f $unversioned_name 
   ln -s $f $no_info_name
done 


cd $RPM_BUILD_ROOT%{_docdir}/%{name}-%{short_underscore_version}
find . -type d | xargs chmod 755 
find . -type f | xargs chmod 644 

cd $curdir 

%clean 
rm -rf $RPM_BUILD_ROOT 
rm -rf ../../../doc
rm -rf  ../../../include
rm -rf  ../../../lib

%post 
/sbin/ldconfig 


%postun 
/sbin/ldconfig 


%files 
%defattr(-, root, root) 
%{_libdir}/*
%{_includedir}/%{name} 
%{_includedir}/%{name}-%{short_underscore_version}
%{_docdir}/%{name}-%{short_underscore_version}





#
# spec file for ErlangOTP
#

Name: %{name} 
Summary: Programming language, runtime environment, and Open Telecoms Platform (OTP) libs
Version: %{version}
Release: %{release}
License: Erlang Public License (EPL) 
URL: http://www.erlang.org/
Group: CACTUS 
Source: %{tarball_file}
Requires: gcc gcc-c++ ncurses-devel
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot 

%description 
Erlang is a general-purpose programming language and runtime environment. Erlang
has built-in support for concurrency, distribution and fault tolerance.  The
Open Telecoms Platform (OTP) libraries are included to allow users to easily build
large-scale, distributed, reliable, soft real-time concurrent and fault-tolerant
systems.

%prep

%build

%install 
curdir=`pwd` 
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

%postun 

%files 
%defattr(-, root, root) 
%{_prefix}/include/*
%{_prefix}/lib/*

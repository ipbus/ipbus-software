#
# spec file for ErlangOTP
#

Name: %{name} 
Summary: Programming language, runtime environment, and Open Telecoms Platform (OTP) libs
Version: %{version}
Release: %{release}
License: Erlang Public License (EPL) 
URL: http://www.erlang.org/
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

# copy binaries to RPM_BUILD_ROOT and set aliases
mkdir -p $RPM_BUILD_ROOT%{_prefix}/bin
cp -rp %{sources_dir}/bin/* $RPM_BUILD_ROOT%{_prefix}/bin/.

# copy libs to RPM_BUILD_ROOT and set aliases
mkdir -p $RPM_BUILD_ROOT%{_prefix}/lib
cp -rp %{sources_dir}/lib/* $RPM_BUILD_ROOT%{_prefix}/lib/.

# Now replace some paths within the Erlang release so that they use the path for
# the RPM installation directory rather than the path where the RPM was created.
cd $RPM_BUILD_ROOT%{_prefix}/lib/erlang
sed -i "s|%{sources_dir}|%{_prefix}|" erts*/bin/{erl,start} releases/RELEASES bin/{erl,start}
cd $curdir

#Change access rights
chmod -R 755 $RPM_BUILD_ROOT%{_prefix}/bin
chmod -R 755 $RPM_BUILD_ROOT%{_prefix}/lib

%clean 


%post 

%postun 

%files 
%defattr(-, root, root) 
%{_prefix}/bin/*
%{_prefix}/lib/*

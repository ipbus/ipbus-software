#
# spefile for boost
#
Name: %{name} 
Version: %{version} 
Release: %{release} 
Packager: %{packager}
Summary: Boost Library packaged for the CACTUS project
License: Boost Software License
Source: %{tarball_file}
URL:  http://www.boost.org/
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot 
Prefix: %{_prefix}

%description
Boost Library packaged for the CACTUS project

%prep

%build


%install 
#cd $RPM_BUILD_ROOT 

# copy includes to RPM_BUILD_ROOT and set aliases
mkdir -p $RPM_BUILD_ROOT%{_prefix}/include
cp -rp %{sources_dir}/include/* $RPM_BUILD_ROOT%{_prefix}/include/.

# copy libs to RPM_BUILD_ROOT and set aliases
mkdir -p $RPM_BUILD_ROOT%{_prefix}/lib
cp -rp %{sources_dir}/lib/* $RPM_BUILD_ROOT%{_prefix}/lib/.

#Change access rights
chmod -R 755 $RPM_BUILD_ROOT%{_prefix}/lib
chmod -R 755 $RPM_BUILD_ROOT%{_prefix}/include

# Use lib64 for libraries if installing under /usr
%if "%{_prefix}" == "/usr"
%ifarch x86_64
mv $RPM_BUILD_ROOT%{_prefix}/lib $RPM_BUILD_ROOT%{_prefix}/lib64
%endif
%endif


%clean 

%post 
#/sbin/ldconfig 

%postun 
#/sbin/ldconfig 

%files 
%defattr(-, root, root) 
%{_prefix}/lib*/*
%{_prefix}/include/*


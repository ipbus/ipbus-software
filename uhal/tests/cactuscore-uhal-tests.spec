#
# spefile for uHAL Library Tests
#
Name: %{name} 
Version: %{version} 
Release: %{release} 
Packager: %{packager}
Summary: uHAL Library Tests
License: BSD License
Group: CACTUS
Source: https://svnweb.cern.ch/trac/cactus/browser/trunk/uhal/tests
URL: https://svnweb.cern.ch/trac/cactus 
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot 
Prefix: %{_prefix}

%description
uHAL Library Tests

%prep

%build


%install 

# copy includes to RPM_BUILD_ROOT and set aliases
mkdir -p $RPM_BUILD_ROOT%{_prefix}
cp -rp %{sources_dir}/* $RPM_BUILD_ROOT%{_prefix}/.

#Change access rights
chmod -R 755 $RPM_BUILD_ROOT%{_prefix}/lib
chmod -R 755 $RPM_BUILD_ROOT%{_prefix}/bin
chmod -R 755 $RPM_BUILD_ROOT%{_prefix}/include


%clean 

%post 

%postun 

%files 
%defattr(-, root, root,-) 
%{_prefix}/bin/*
%{_prefix}/lib/*
%{_prefix}/etc/*
%{_prefix}/include/*

#
# spec file for controlhub
#

Name: %{name}
Summary: IPbus packet-router
Version: %{version}
Release: %{release}
License: BSD License
URL: http://www.erlang.org/
Group: CACTUS
Source: %{tarball_file}
Requires: cactuscore-extern-erlang
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot


%description
IPbus packet-router


%prep


%build


%install

# Make directories
mkdir -p $RPM_BUILD_ROOT%{_prefix}/{bin,lib}
mkdir -p $RPM_BUILD_ROOT/etc/init.d
mkdir -p $RPM_BUILD_ROOT/etc/rsyslog.d

# Copy over files
cp -rp %{sources_dir}/bin/* $RPM_BUILD_ROOT%{_prefix}/bin/.
cp -rp %{sources_dir}/lib/* $RPM_BUILD_ROOT%{_prefix}/lib/.
cp -rp %{sources_dir}/controlhub $RPM_BUILD_ROOT/etc/init.d/.
cp -rp %{sources_dir}/rsyslog.d.conf $RPM_BUILD_ROOT/etc/rsyslog.d/controlhub.conf

# Link /var/log/controlhub to controlhub log dir
mkdir -p ${RPM_BUILD_ROOT}/var/log
ln -s %{_prefix}/lib/controlhub/log ${RPM_BUILD_ROOT}/var/log/controlhub

# Now update the CONTROLHUB_BIN_DIR variable in controlhub scripts
cd $RPM_BUILD_ROOT%{_prefix}/bin
sed -i "s|CONTROLHUB_BIN_DIR=.*|CONTROLHUB_BIN_DIR=/opt/cactus/lib/controlhub/bin|" controlhub_*
cd $curdir


##Change access rights
chmod 755 $RPM_BUILD_ROOT%{_prefix}/bin/controlhub_*
chmod -R 755 $RPM_BUILD_ROOT%{_prefix}/lib


%clean


%pre
# Can't overwrite dir with a symlink, so remove old log dir first on upgrades
if [ -d /var/log/controlhub ]; then \
  rm -rf /var/log/controlhub
fi

%post
# 1) Must stop ControlHub in case RPM being upgraded (i.e. rpm -U), or in case of error on previous RPM erase
/etc/init.d/controlhub stop || true
# 1b) Restart rsyslog so that it picks up configuration file change
/sbin/service rsyslog restart
# 2) Normal ControlHub start steps
/sbin/chkconfig --add controlhub
/etc/init.d/controlhub start 

%preun
if [ $1 = 0 ]; then
  /etc/init.d/controlhub stop 
  /sbin/chkconfig controlhub off
  /sbin/chkconfig --del controlhub
fi

%postun

%files
%defattr(-, root, root)
%{_prefix}/bin/*
%{_prefix}/lib/*
/etc/init.d/controlhub
/etc/rsyslog.d/controlhub.conf
/var/log/controlhub
%config(noreplace) %{_prefix}/lib/controlhub/controlhub.config

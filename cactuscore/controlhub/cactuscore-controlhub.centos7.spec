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
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot


%description
IPbus packet-router


%prep


%build


%install

# Make directories
mkdir -p $RPM_BUILD_ROOT%{_prefix}/{bin,lib}
mkdir -p $RPM_BUILD_ROOT/usr/lib/systemd/system
mkdir -p $RPM_BUILD_ROOT/etc/{rsyslog.d,logrotate.d}
mkdir -p $RPM_BUILD_ROOT/var/log/controlhub

# Copy over files
cp -rp %{sources_dir}/bin/* $RPM_BUILD_ROOT%{_prefix}/bin/.
cp -rp %{sources_dir}/lib/* $RPM_BUILD_ROOT%{_prefix}/lib/.
cp -rp %{sources_dir}/controlhub.service $RPM_BUILD_ROOT/usr/lib/systemd/system/
cp -rp %{sources_dir}/rsyslog.d.conf $RPM_BUILD_ROOT/etc/rsyslog.d/controlhub.conf
cp -rp %{sources_dir}/logrotate.d.conf $RPM_BUILD_ROOT/etc/logrotate.d/controlhub.conf

# Move user-editable configuration file under /etc
mv $RPM_BUILD_ROOT%{_prefix}/lib/controlhub/controlhub.config $RPM_BUILD_ROOT/etc/controlhub.config
sed -i "s|\"controlhub.config\"|\"/etc/controlhub.config\"|" $RPM_BUILD_ROOT%{_prefix}/lib/controlhub/releases/*/sys.config

# Update the CONTROLHUB_BIN_DIR variable in controlhub scripts
sed -i "s|CONTROLHUB_BIN_DIR=.*|CONTROLHUB_BIN_DIR=%{_prefix}/lib/controlhub/bin|" $RPM_BUILD_ROOT%{_prefix}/bin/controlhub_*

#Change access rights
chmod 755 $RPM_BUILD_ROOT%{_prefix}/bin/controlhub_*
chmod -R 755 $RPM_BUILD_ROOT%{_prefix}/lib $RPM_BUILD_ROOT/var/log/controlhub

%clean


%pre


%post
# 0) Ensure controlhub log is readable by all
touch /var/log/controlhub/controlhub.log
chmod 644 /var/log/controlhub/controlhub.log
# 1) Must stop ControlHub in case RPM being upgraded (i.e. rpm -U), or in case of error on previous RPM erase
systemctl stop controlhub
# 1b) Restart rsyslog so that it picks up configuration file change
/sbin/service rsyslog restart
# 2) Normal ControlHub start steps
systemctl daemon-reload
systemctl enable controlhub
systemctl start controlhub

%preun
if [ $1 = 0 ]; then
  systemctl stop controlhub
  systemctl disable controlhub
fi

%postun


%files
%defattr(-, root, root)
%{_prefix}/bin/*
%{_prefix}/lib/*
/usr/lib/systemd/system/controlhub.service
/etc/rsyslog.d/controlhub.conf
/var/log/controlhub
%config(noreplace) /etc/controlhub.config
%config(noreplace) /etc/rsyslog.d/controlhub.conf
%config(noreplace) /etc/logrotate.d/controlhub.conf


FROM centos:7.9.2009 as builder

ARG TARGETPLATFORM


# Install dependencies
RUN \
  if [ "${TARGETPLATFORM}" == "linux/arm/v7" ]; then \
    echo "armhfp" > /etc/yum/vars/basearch; \
    echo "armv7hl" > /etc/yum/vars/arch; \
    echo "armv7hl-redhat-linux-gpu" > /etc/rpm/platform; \
    echo -e "[epel]\nname=Epel rebuild for armhfp\nbaseurl=https://armv7.dev.centos.org/repodir/epel-pass-1/\nenabled=1\ngpgcheck=0" > /etc/yum.repos.d/epel.repo; \
    mkdir -p /usr/local/lib64; \
  else \
    yum -y install epel-release; \
  fi && \
  (for PACKAGE in \
    make rpm-build createrepo git-core \
    erlang gcc-c++-4.8.5 \
    boost-devel-1.53.0 pugixml-devel-1.8 \
    python-devel \
    python3-devel \
    python34-devel \
    sudo passwd createrepo; \
  do \
    yum -y install ${PACKAGE} || exit $?; \
  done) && \
  yum clean all


# Copy over source code & build
COPY . /tmp/________________________/ipbus-software
WORKDIR /tmp/________________________/ipbus-software
ARG PACKAGE_RELEASE_SUFFIX=
ARG PYTHONS="python2.7 python3.4 python3.6"
RUN \
  make Set=uhal BUILD_UHAL_PYTHON=0 BUILD_UHAL_GUI=0 -j$(nproc) &&\
  make Set=uhal BUILD_UHAL_PYTHON=0 BUILD_UHAL_GUI=0 -j$(nproc) rpm &&\
  find . -name "*.rpm" &&\
  mkdir /rpms &&\
  cp -v $(find . -name "*.rpm") /rpms/ &&\
  for PYTHON in ${PYTHONS}; do \
    echo "PYTHON: ${PYTHON}"\
    make -C uhal/gui PYTHON=$PYTHON &&\
    make -C uhal/gui PYTHON=$PYTHON rpm &&\
    make -C uhal/python PYTHON=$PYTHON &&\
    make -C uhal/python PYTHON=$PYTHON rpm &&\
    cp -v `find uhal/gui uhal/python -iname "*.rpm"` /rpms &&\
    make -C uhal/gui PYTHON=$PYTHON clean cleanrpm &&\
    make -C uhal/python PYTHON=$PYTHON clean cleanrpm &&\
    echo &&\
    echo;\
  done &&\
  cp ci/yum/yumgroups-centos7.xml /rpms/yumgroups.xml &&\
  cd /rpms &&\
  createrepo -vg yumgroups.xml .


#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####
FROM scratch as export
COPY --from=builder /rpms /rpms

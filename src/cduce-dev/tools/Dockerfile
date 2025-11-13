FROM ubuntu:20.04

LABEL maintainer="Kim Nguyen <kim.nguyen@universite-paris-saclay.fr>"

ENV OPAMYES=true
ENV DEBIAN_FRONTEND=noninteractive
ARG user_id

RUN apt-get update -y && \
 apt-get dist-upgrade -y && \
 apt-get install -y -q \
 locales tzdata ca-certificates sudo ssh git \
         autoconf automake autopoint autotools-dev binutils binutils-common \
         binutils-dev binutils-x86-64-linux-gnu bsdmainutils build-essential \
         cpp cpp-9 dwz g++ g++-9 gcc gcc-9 gcc-9-base gettext gettext-base \
         groff-base intltool-debian libarchive-zip-perl libasan5 libatomic1 \
         libbinutils libc-dev-bin libc6-dev libcc1-0 libcroco3 libcrypt-dev \
         libctf-nobfd0 libctf0 libdebhelper-perl libdpkg-perl libelf1 \
         libfile-stripnondeterminism-perl libgcc-9-dev libgmp-dev libgomp1 libiberty-dev\
         libisl22 libitm1 liblsan0 libssl-dev libmpc3 libmpfr6 libncurses-dev \
         libncurses5-dev libpipeline1 libpthread-stubs0-dev libquadmath0 \
         libsigsegv2 libstdc++-9-dev libsub-override-perl libtool libtsan0 libubsan1\
         libuchardet0 libx11-dev libxau-dev libxcb1-dev libxdmcp-dev \
         linux-libc-dev m4 make man-db pkg-config po-debconf x11proto-core-dev \
         x11proto-dev libcurl4-gnutls-dev libexpat1-dev pkg-config rsync wget unzip 
         
RUN echo "Europe/Paris" > /etc/timezone && \
     ln -nsf /usr/share/zoneinfo/Europe/Paris /etc/localtime && \
     sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
     locale-gen

RUN mkdir -p /usr/local/bin && wget -O /usr/local/bin/opam \
            'https://github.com/ocaml/opam/releases/download/2.1.5/opam-2.1.5-x86_64-linux' && \
             chmod 755 /usr/local/bin/opam

RUN adduser --uid "$user_id" --disabled-password --gecos "" --shell /bin/bash cduce
RUN mkdir /home/cduce/.ssh && chmod 700 /home/cduce/.ssh
COPY --chmod=600 id_rsa /home/cduce/.ssh
COPY --chmod=600 id_rsa.pub /home/cduce/.ssh
RUN chown -R cduce:cduce /home/cduce/.ssh

ENV TZ "Europe/Paris"
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8


USER cduce

ARG packages
ARG ocaml_version

RUN test "$ocaml_version" || test "$packages" || exit 1
ENV OPAMSOLVERTIMEOUT=0
RUN opam init  --disable-sandboxing --bare && \
    opam switch create "$ocaml_version" && \
    eval `opam config env` && \
    opam install -y ${packages}

ENTRYPOINT [ "opam", "exec", "--" ]

CMD [ "/bin/sh", "-c", "bash" ]

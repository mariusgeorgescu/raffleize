# syntax=docker/dockerfile:1.4

### STAGE 1: Build ###
FROM --platform=$TARGETPLATFORM benz0li/ghc-musl:9.6.6 AS builder

ARG TARGETARCH

# Install common dependencies
RUN apk add  bash curl ca-certificates git pkgconfig build-base \
    automake autoconf libtool gmp-dev zlib-dev musl-dev musl-utils


# Cardano Dependencies - Alpine equivalent
RUN apk add  \
    curl \
    ca-certificates \
    automake \
    build-base \
    pkgconfig \
    libffi-dev \
    gmp-dev \
    lmdb-dev \
    numactl-dev \
    openssl-dev \
    ncurses-dev \
    llvm-dev \
    zlib-dev \
    postgresql-dev \
    xz-dev \
    xz-libs \
    sqlite-dev \
    m4 \
    git \
    jq \
    wget \
    tmux \
    g++ \
    make \
    autoconf \
    libtool

RUN apk add  \
    postgresql-libs \
    ncurses-libs \
    libsodium \
    libsecp256k1 \
    libstdc++ \
    gmp \
    libgcc


# Build and install libsodium 1.0.19 (for libsodium.so.26)
RUN curl -LO https://download.libsodium.org/libsodium/releases/libsodium-1.0.19.tar.gz && \
    tar xvf libsodium-1.0.19.tar.gz && \
    cd libsodium-stable && \
    ./configure --prefix=/usr --enable-shared --disable-static && \
    make && make install


RUN git clone --depth 1 --branch 'v0.3.2' https://github.com/bitcoin-core/secp256k1 && \
    cd secp256k1 && \
    ./autogen.sh && \
    ./configure --enable-module-schnorrsig --enable-experimental && \
    make && \
    make check && \
    make install

ENV PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH

# BLST
ARG BLST_REF=v0.3.15
COPY libblst.pc /usr/lib/pkgconfig/
RUN git clone https://github.com/supranational/blst && \
    cd blst && \
    git checkout ${BLST_REF} && \
    ./build.sh && \
    cp bindings/blst_aux.h bindings/blst.h bindings/blst.hpp  /usr/include/ && \
    cp libblst.a /usr/lib/ && \
    chmod u=rw,go=r /usr/lib/pkgconfig/libblst.pc \
    /usr/include/blst_aux.h /usr/include/blst.h /usr/include/blst.hpp \
    /usr/lib/libblst.a  

# PostgreSQL repository setup for Alpine
RUN apk add  postgresql-client postgresql-dev



# Building My Project 
WORKDIR /raffleize

ENV PATH="/root/.cabal/bin:$PATH"

ENV CC=musl-gcc
ENV LD=musl-gcc


# Build static with musl-ghc

WORKDIR /raffleize

COPY *.cabal cabal.project ./

RUN cabal update 


# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies 

COPY src ./src


# Add and Install Application Code
RUN cabal install server --enable-executable-static --overwrite-policy=always --installdir=.  --install-method=copy --ghc-options="-optl=-static -optl=-pthread -optc=-static"


### STAGE 2: Runtime ###

FROM --platform=$TARGETPLATFORM alpine:3.20 AS runtime

# Install common dependencies
RUN apk add  bash curl ca-certificates git pkgconfig build-base \
    automake autoconf libtool gmp-dev zlib-dev musl-dev musl-utils


# Cardano Dependencies - Alpine equivalent
RUN apk add  \
    curl \
    ca-certificates \
    automake \
    build-base \
    pkgconfig \
    libffi-dev \
    gmp-dev \
    lmdb-dev \
    numactl-dev \
    openssl-dev \
    ncurses-dev \
    llvm-dev \
    zlib-dev \
    postgresql-dev \
    xz-dev \
    xz-libs \
    sqlite-dev \
    m4 \
    git \
    jq \
    wget \
    tmux \
    g++ \
    make \
    autoconf \
    libtool

RUN apk add  \
    postgresql-libs \
    ncurses-libs \
    libsodium \
    libsecp256k1 \
    libstdc++ \
    gmp \
    libgcc


RUN apk add  postgresql-client postgresql-dev

# Build and install libsodium 1.0.19 (for libsodium.so.26)
RUN curl -LO https://download.libsodium.org/libsodium/releases/libsodium-1.0.19.tar.gz && \
    tar xvf libsodium-1.0.19.tar.gz && \
    cd libsodium-stable && \
    ./configure --prefix=/usr --enable-shared --disable-static && \
    make && make install


RUN git clone --depth 1 --branch 'v0.3.2' https://github.com/bitcoin-core/secp256k1 && \
    cd secp256k1 && \
    ./autogen.sh && \
    ./configure --enable-module-schnorrsig --enable-experimental && \
    make && \
    make check && \
    make install

ENV PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH    

# BLST
ARG BLST_REF=v0.3.15
COPY libblst.pc /usr/lib/pkgconfig/
RUN git clone https://github.com/supranational/blst && \
    cd blst && \
    git checkout ${BLST_REF} && \
    ./build.sh && \
    cp bindings/blst_aux.h bindings/blst.h bindings/blst.hpp  /usr/include/ && \
    cp libblst.a /usr/lib/ && \
    chmod u=rw,go=r /usr/lib/pkgconfig/libblst.pc \
    /usr/include/blst_aux.h /usr/include/blst.h /usr/include/blst.hpp \
    /usr/lib/libblst.a  

# Copy config files
WORKDIR /app
COPY --from=builder /raffleize/server /app/server
COPY ./config  /app/config



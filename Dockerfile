

FROM haskell:9.2.8-buster

RUN apt-get update -y \
&& apt-get install -y automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf libsqlite3-dev m4 ca-certificates gcc libc6-dev \
&& apt-get clean

ENV PATH="/root/.cabal/bin:/root/.local/bin:$PATH"


RUN git clone https://github.com/IntersectMBO/libsodium \
    && cd libsodium \
    && git fetch --all --recurse-submodules --tags \
    && git tag \
    && ./autogen.sh \
    && ./configure \
    && make \
    && make install \
    && cd .. && rm -rf libsodium

RUN git clone --depth 1 --branch 'v0.3.2' https://github.com/bitcoin-core/secp256k1 \
    && cd secp256k1 \
    && ./autogen.sh \
    && ./configure --enable-module-schnorrsig --enable-experimental \
    && make \
    && make check \
    && make install 

ENV LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" \
    PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"

WORKDIR /raffleize

COPY *.cabal cabal.project /raffleize/ 

RUN cabal update 


# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4

COPY . /raffleize
RUN cabal build 

# Add and Install Application Code
RUN cabal install

CMD ["server"]

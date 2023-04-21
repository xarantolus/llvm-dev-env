FROM debian:buster-slim AS intermediate

# Install dependencies
RUN apt-get -qq update; \
    apt-get install -qqy --no-install-recommends \
    gnupg2 wget ca-certificates apt-transport-https sudo \
    autoconf automake cmake dpkg-dev file make patch libc6-dev git gdb

# Install LLVM 15
RUN echo "deb https://apt.llvm.org/buster llvm-toolchain-buster-15 main" \
    > /etc/apt/sources.list.d/llvm.list && \
    wget -qO /etc/apt/trusted.gpg.d/llvm.asc \
    https://apt.llvm.org/llvm-snapshot.gpg.key && \
    apt-get -qq update && \
    apt-get install -qqy -t llvm-toolchain-buster-15 clang-15 clang-tidy-15 clang-format-15 lld-15 libpolly-15-dev libz-dev && \
    for f in /usr/lib/llvm-15/bin/*; do ln -sf "$f" /usr/bin; done && \
    rm -rf /var/lib/apt/lists/*

# Set env variable for llvm-sys library
ENV LLVM_SYS_150_PREFIX=/usr/lib/llvm-15

# Install my scripts
RUN git clone https://github.com/xarantolus/scripts /var/scripts && cd /var/scripts && chmod +x *
ENV PATH="${PATH}:/var/scripts"

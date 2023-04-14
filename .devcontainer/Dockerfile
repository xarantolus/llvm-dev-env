FROM debian:buster-slim AS intermediate

RUN apt-get -qq update; \
    apt-get install -qqy --no-install-recommends \
        gnupg2 wget ca-certificates apt-transport-https \
        autoconf automake cmake dpkg-dev file make patch libc6-dev git

RUN echo "deb https://apt.llvm.org/buster llvm-toolchain-buster-16 main" \
        > /etc/apt/sources.list.d/llvm.list && \
    wget -qO /etc/apt/trusted.gpg.d/llvm.asc \
        https://apt.llvm.org/llvm-snapshot.gpg.key && \
    apt-get -qq update && \
    apt-get install -qqy -t llvm-toolchain-buster-16 clang-16 clang-tidy-16 clang-format-16 lld-16 libpolly-16-dev libz-dev && \
    for f in /usr/lib/llvm-16/bin/*; do ln -sf "$f" /usr/bin; done && \
    rm -rf /var/lib/apt/lists/*

ENV LLVM_SYS_160_PREFIX=/usr/lib/llvm-16

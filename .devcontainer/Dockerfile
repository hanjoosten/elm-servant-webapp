FROM ubuntu:22.04 AS base
## Inspiration from https://stackoverflow.com/questions/67680726/installing-haskells-cabal-or-ghcup-inside-a-dockerfile-wont-work

# Set environment variable for timezone to avoid timezone-related errors
ENV TZ=Europe/Berlin

# Define a variable for the user and set user-related environment variables
ARG USERNAME=vscode

# Set various environment variables to simplify path configurations
ENV USERNAME=${USERNAME} \
    USER_UID=1000 \
    USER_GID=1000 \
    DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF8 \
    WDIR=/home/${USERNAME}

# Install dependencies for Haskell, Elm, PostgreSQL client, and curl
RUN apt-get update && apt-get install -y \
    apt-transport-https \
    autoconf \
    automake \
    build-essential \
    curl \
    gcc \
    git \
    gnupg2 \
    graphviz \
    htop \
    iproute2 \
    libbz2-dev \
    libexpat1-dev \
    libffi-dev \
    libffi8ubuntu1 \
    libgmp-dev \
    libgmp10 \
    libncurses-dev \
    libncurses5 \
    libnuma-dev \
    libpq-dev \
    libtinfo5 \
    lsb-release \
    net-tools \
    openssh-server \
    pkg-config \
    postgresql \
    postgresql-contrib \
    postgresql-client \
    software-properties-common \
    sudo \
    unzip \
    wget \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

RUN groupadd --gid $USER_GID $USERNAME && \    
    useradd -ms /bin/bash -K MAIL_DIR=/dev/null --uid $USER_UID --gid $USER_GID -m $USERNAME && \
    mkdir -p /etc/sudoers.d && \
    echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME && \
    chmod 0440 /etc/sudoers.d/$USERNAME

USER ${USER_UID}:${USER_GID}    

RUN ulimit -n 8192


# install gpg keys
# ARG GPG_KEY=BA3CBA3FFE22B574
# RUN gpg --batch --keyserver keys.openpgp.org --recv-keys $GPG_KEY
# manual installation: see https://www.haskell.org/ghcup/install/#manual-installation

# install ghcup
# Inspiration: http://haskell.org/ghcup/install
# Install ghcup as the 'vscode' user


RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh


# Add ghcup and cabal to the PATH
ENV PATH="${WDIR}/.cabal/bin:${WDIR}/.ghcup/bin:${PATH}:${WDIR}/.local/bin:${PATH}"

ARG GHC=9.6.6
ARG CABAL=3.10.3.0
ARG HLS=2.9.0.1
ARG STACK=3.1.1

# install GHC and cabal

RUN ghcup install ghc --set ${GHC} && \
    ghcup install cabal --set ${CABAL} && \
    ghcup install stack --set ${STACK} && \
    ghcup install hls --set ${HLS}



FROM base AS tools

# Install global packages.
# Versions are pinned, since we don't want to accidentally break anything (by always installing latest).
RUN cabal install -v haskell-dap-0.0.16.0 && \
    cabal install -v stylish-haskell-0.14.5.0 && \
    cabal install -v ormolu-0.7.2.0 && \
    cabal install -v servant-server-0.20.2 && \
    cabal install -v servant-0.20.2
# RUN cabal install -v ghci-dap-0.0.19.0
# RUN cabal install -v haskell-debug-adapter-0.0.37.0
# RUN cabal install -v hlint-3.2.7
# RUN cabal install -v apply-refact-0.9.3.0
# RUN cabal install -v retrie-1.1.0.0
# RUN cabal install -v hoogle-5.0.18.3
# RUN cabal install -v liquidhaskell-0.8.10.2

# Generate hoogle db
# RUN hoogle generate && stack hoogle

# create a `binbin` directory, that contains the executables themselves. In .cabal/bin most (all?) files are symbolic links
RUN mkdir ${WDIR}/.cabal/binbin && \
    cp -L ${WDIR}/.cabal/bin/* ${WDIR}/.cabal/binbin

FROM base AS dev
COPY --from=tools ${WDIR}/.cabal/binbin ${WDIR}/.cabal/bin
ENV PATH="${WDIR}/.cabal/bin:${WDIR}/.ghcup/bin:${WDIR}/.local/bin:${PATH}"

# Install Elm as root (via sudo), because it requires writing to /usr/local/bin
RUN sudo curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz \
    && sudo gunzip elm.gz \
    && sudo chmod +x elm \
    && sudo mv elm /usr/local/bin

# Install Node.js and npm (requires sudo for system-wide installation)
RUN sudo curl -fsSL https://deb.nodesource.com/setup_16.x | sudo -E bash - \
    && sudo apt-get install -y \
      nodejs

# Install tooling for Elm
RUN sudo npm install -g \
     elm-language-server \
     elm-test \
     elm-format \
     elm-review

# Add just the .cabal file to capture dependencies
# COPY ./package.yaml ${WDIR}/package.yaml
# COPY ./stack.yaml ${WDIR}/stack.yaml

# RUN stack setup ${GHC}
ENV DEBIAN_FRONTEND=dialog

ENTRYPOINT ["/bin/bash"]

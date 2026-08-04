FROM rocker/r-ver:4.3.2

# Install apt dependencies
RUN apt-get update && apt-get install -y \
  ca-certificates \
  cmake \
  fftw3 \  
  git \
  libcurl4-openssl-dev \
  libfontconfig1-dev \
  libfribidi-dev \
  libgit2-dev \
  libglpk40 \
  libharfbuzz-dev \
  libssl-dev \
  librsvg2-2 \
  librsvg2-dev \
  libsodium-dev \
  libsecret-1-dev \
  libpoppler-cpp-dev \
  libxml2-dev \
  libxt6 \
  libzmq3-dev \
  perl \
  texinfo \
  wget \
  xclip \
  xdg-utils \
  python3-pip && \
  pip3 install radian

WORKDIR /sleepIPD_analysis

# Install tex
ENV CTAN_REPO="https://mirror.aarnet.edu.au/pub/CTAN/systems/texlive/tlnet/"
ENV PATH="$PATH:/usr/local/texlive/bin/linux"
RUN /rocker_scripts/install_pandoc.sh
RUN /rocker_scripts/install_texlive.sh

# These are all the latex packages that GitHub Actions tries to install
RUN tlmgr install academicons booktabs colortbl enumitem environ euenc fancyhdr \
  fontawesome fontspec fp ifmtarg l3packages latex-amsmath-dev pgf ragged2e setspace \
  sourcesanspro tabu tcolorbox tipa trimspaces unicode-math varwidth xifthen xunicode

# Install renv
ENV RENV_VERSION=1.0.3
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))" && \
  R -e "remotes::install_github('rstudio/renv@v${RENV_VERSION}')" 

# Setup renv
COPY ../renv.lock renv.lock
RUN R -e "renv::restore()"
# Install dev requirements that are seperate from the project
RUN R -e "renv::install(c('languageserver', 'nx10/httpgd', 'conflicted', 'dotenv', 'devtools', 'milesmcbain/fnmate','milesmcbain/tflow'))"
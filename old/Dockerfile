FROM fpco/stack-build:lts-10.4

RUN mkdir /juvix
WORKDIR /juvix
ADD . /juvix
RUN stack build --install-ghc && stack install && stack clean --full

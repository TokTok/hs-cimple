# hadolint ignore=DL3007
FROM toxchat/haskell:latest

RUN ["rm", "-rf", "/work/hs-cimple"]
COPY --chown=builder:users . /work/hs-cimple
RUN ["stack", "build", "cimple"]

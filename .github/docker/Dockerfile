FROM toxchat/toktok-stack:haskell AS build

RUN ["rm", "-rf", "/work/hs-cimple"]
COPY --chown=builder:users . /work/hs-cimple
RUN ["stack", "install", "cimple"]

FROM scratch
COPY --from=build /home/builder/.local/ /

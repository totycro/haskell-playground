# alpine would be nice, but latest ghc package is 8.4.3 and i don't want to compile
#FROM alpine:3.10.3
#
#RUN apk update && apk add ghc
#
## TOOD: unify apk installs later (keeping it to avoid expensive rebuilding)
#RUN apk add curl
# RUN curl -sSL https://get.haskellstack.org/ | sh  # :-(

FROM fpco/stack-build:lts-14.13

ENV HOME /mnt

WORKDIR /mnt

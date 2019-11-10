# alpine would be nice, but latest ghc package is 8.4.3 and i don't want to compile
#FROM alpine:3.10.3
#
#RUN apk update && apk add ghc
#
## TOOD: unify apk installs later (keeping it to avoid expensive rebuilding)
#RUN apk add curl
# RUN curl -sSL https://get.haskellstack.org/ | sh  # :-(

FROM fpco/stack-build:lts-14.13

# TODO:
# possibly switch to somethink like this (unmaintained):
# https://github.com/mrkkrp/haskell-docker/blob/master/ghc-8.4.4/Dockerfile

# (official haskel docker images don't provide a Dockerfile and it's git links don't work)

ENV HOME /mnt/docker-home

WORKDIR /mnt

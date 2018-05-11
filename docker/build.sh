#!/bin/bash

DIR=`dirname $0`
TAG=${1?param missing - docker image tag.}

docker build -t "dfacastro/base" "$DIR/base"

# build docker image
stack --stack-yaml $DIR/../stack-docker.yaml image container

# tag docker image
docker image tag dfacastro/2048-twenty48:latest dfacastro/2048-twenty48:$TAG

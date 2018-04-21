#!/bin/bash

DIR=`dirname $0`

docker build -t "dfacastro/base" "$DIR/base"
stack --stack-yaml $DIR/../stack-docker.yaml image container

#!/bin/bash

DIR=`dirname $0`

docker-compose --file=$DIR/../docker-compose.prod.yml up -d

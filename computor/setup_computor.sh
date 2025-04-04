#!/usr/bin/env bash

apt update
apt install -y \
    git \
    docker.io \
    docker-compose

mkdir -p ~/computor
cd ~/computor

git clone \
    git@gitlab.tugraz.at:codeability/development/execution-backend-stub.git \
    execution-backend

cd execution-backend
docker-compose up -d

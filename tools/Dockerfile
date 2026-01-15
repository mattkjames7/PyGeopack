FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get install -y --no-install-recommends software-properties-common && \
    add-apt-repository ppa:deadsnakes/ppa && \
    apt-get update && \
    apt-get install -y python3.14 python3.14-venv python3-pip \
    g++ gcc make gfortran --no-install-recommends && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

USER ubuntu
WORKDIR /home/ubuntu
CMD ["sleep", "infinity"]

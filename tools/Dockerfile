FROM ubuntu:24.04
ENV PYTHON_VERSION=3.14
# Install common packages and add deadsnakes PPA for multiple Python versions
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        gnupg && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -p /etc/apt/keyrings && \
    curl -fsSL https://keyserver.ubuntu.com/pks/lookup?op=get\&search=0x6A755776 \
      | gpg --dearmor \
      | tee /etc/apt/keyrings/deadsnakes.gpg > /dev/null

RUN echo "deb [signed-by=/etc/apt/keyrings/deadsnakes.gpg] \
http://ppa.launchpad.net/deadsnakes/ppa/ubuntu noble main" \
> /etc/apt/sources.list.d/deadsnakes.list

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        python${PYTHON_VERSION} \
        python${PYTHON_VERSION}-venv \
        python${PYTHON_VERSION}-dev && \
    rm -rf /var/lib/apt/lists/*

USER ubuntu
WORKDIR /home/ubuntu
CMD ["sleep", "infinity"]

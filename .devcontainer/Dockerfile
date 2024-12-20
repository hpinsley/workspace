FROM mcr.microsoft.com/devcontainers/python:3.12

#USER root
#RUN apt update && apt install -y nodejs npm

RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN chmod +x elm
RUN mv elm /usr/local/bin/

USER vscode

RUN python -m pip install aider-install
WORKDIR /home/vscode/.local/bin
ENV PATH /home/vscode/.local/bin:$PATH
RUN aider-install

FROM mcr.microsoft.com/devcontainers/python:3.12

USER root
RUN apt update && apt install -y nodejs npm

USER vscode

RUN python -m pip install aider-install
WORKDIR /home/vscode/.local/bin
ENV PATH /home/vscode/.local/bin:$PATH
RUN aider-install

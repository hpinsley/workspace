FROM mcr.microsoft.com/devcontainers/python:3.12

USER vscode

#RUN export PATH="$PATH:/root/.local/bin"

RUN python -m pip install aider-install
WORKDIR /home/vscode/.local/bin
# RUN ls -1 && xyzzy
ENV PATH /home/vscode/.local/bin:$PATH
RUN echo $PATH && ls -al && ./aider-install

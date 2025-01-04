FROM mcr.microsoft.com/devcontainers/python:3.12

# ACM PCA Root and Intermediates
ADD https://artifactory.blackstone.com/artifactory/misc/platform/certs/blackstone-certs-individual/aws-pca-root-us-east-1-prod.crt /usr/local/share/ca-certificates/aws-pca-root-us-east-1-prod.crt
ADD https://artifactory.blackstone.com/artifactory/misc/platform/certs/blackstone-certs-individual/bx-pca-nonprod-us-east-1.crt /usr/local/share/ca-certificates/bx-pca-nonprod-us-east-1.crt
ADD https://artifactory.blackstone.com/artifactory/misc/platform/certs/blackstone-certs-individual/bx-pca-nonprod-us-east-2.crt /usr/local/share/ca-certificates/bx-pca-nonprod-us-east-2.crt
ADD https://artifactory.blackstone.com/artifactory/misc/platform/certs/blackstone-certs-individual/bx-pca-prod-us-east-1.crt /usr/local/share/ca-certificates/bx-pca-prod-us-east-1.crt
ADD https://artifactory.blackstone.com/artifactory/misc/platform/certs/blackstone-certs-individual/bx-pca-prod-us-east-2.crt /usr/local/share/ca-certificates/bx-pca-prod-us-east-2.crt

RUN curl https://artifactory.blackstone.com/artifactory/misc/platform/certs/ZscalerRootCertificate-2048-SHA256.crt -o /usr/local/share/ca-certificates/ZscalerRootCertificate-2048-SHA256.crt

RUN curl https://artifactory.blackstone.com/artifactory/misc/platform/certs/blackstone-ca.crt -o /usr/local/share/ca-certificates/blackstone-ca.crt \
    && update-ca-certificates

ENV VERIFY_SSL true
# update-ca-certificates re-writes /etc/ssl/certs/ca-certificates.crt
ENV REQUESTS_CA_BUNDLE /etc/ssl/certs/ca-certificates.crt

#USER root
RUN apt update && apt install -y nodejs npm

RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN chmod +x elm
RUN mv elm /usr/local/bin/

USER vscode

RUN python -m pip install aider-install
WORKDIR /home/vscode/.local/bin
ENV PATH /home/vscode/.local/bin:$PATH
RUN aider-install

WORKDIR /workspace


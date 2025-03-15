# Use a Haskell base image
FROM haskell:latest

# Install Elm
RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz \
    && gunzip elm.gz \
    && chmod +x elm \
    && mv elm /usr/local/bin

# Install necessary tools
RUN apt-get update && apt-get install -y \
  curl \
  git \
  build-essential \
  postgresql-client \
  && rm -rf /var/lib/apt/lists/*

# Set the working directory
WORKDIR /workspace


# Expose port 3000 for the frontend
EXPOSE 3000
# Expose port 8080 for the backend
EXPOSE 8080

# Set default command
CMD [ "bash" ]

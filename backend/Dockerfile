# Backend Dockerfile

FROM haskell:9.6.6

WORKDIR /app

# Copy the backend project files
COPY . .

# Install necessary Haskell dependencies
RUN stack install

# Expose the port the backend is listening on
EXPOSE 8080

CMD ["cabal", "new-run", "my-backend"]

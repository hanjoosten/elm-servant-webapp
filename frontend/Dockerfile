# Frontend Dockerfile

FROM node:16

WORKDIR /frontend

# Install Elm
RUN npm install -g elm

# Copy the Elm project files
COPY . .

# Expose the port the frontend is listening on
EXPOSE 80

# The command compiles Elm and serves it using Python's HTTP server
CMD bash -c "elm make src/Main.elm --output=main.js && python3 -m http.server 80"

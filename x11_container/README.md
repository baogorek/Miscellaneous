My interactive Docker container with ssh capabilities
========================

# Prereqs
Install Docker for your system

Install an XServer like Xming (Windows) or xQuartz (MacOS)

# Build and setup

Create the persistent volume. You should only have to do this once:
```
docker volume create --driver local --opt type=none --opt device=<absolute-local-path> --opt o=bind devl-volume
```

Build the docker image when you change Dockerfile
```
docker build -t x11 .
```

Start the container up when you restart your local machine
```
docker run -itd -p 2222:22 --mount source=devl-volume,target=/devl x11
```

Whenver you want to use your container, ssh in
```
ssh -X root@127.0.0.1 -p 2222 -o UserKnownHostsFile=/dev/null
```

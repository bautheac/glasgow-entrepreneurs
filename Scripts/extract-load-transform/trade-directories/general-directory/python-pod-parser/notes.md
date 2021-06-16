# Code

## Linux

```bash
    docker run -d --rm -it --name podparser -v "$(pwd)"/1861-1862:/usr/src/app/1861-1862 \
        -v "$(pwd)"/scripts:/usr/src/app/scripts python2-pod-parser
```

`docker container exec -it podparser bash`

## Windows (git-bash)

```bash
docker run -d --rm -it --name podparser -v "/$(pwd)/1861-1862:/usr/src/app/1861-1862" \
    -v "/$(pwd)/scripts:/usr/src/app/scripts" python2-pod-parser
```

`docker container exec -it podparser bash`

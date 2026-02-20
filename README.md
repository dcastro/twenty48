# 2048 AI

An AI for the 2048 game using minimax and alpha-beta pruning, as described by John Hughes in the paper ["Why Functional Programming Matters"](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf).

The AI was written in Haskell and runs in a Yesod backend.
The decisions are streamed to the browser via a websockets connection.

Demo: <https://2048.diogocastro.com/>

## Run

Install stack, libsass, and, optionally, docker and docker-compose.

```sh
curl -sSL https://get.haskellstack.org/ | sh
brew install libsass
```

For development, you'll need either yesod or ghcid

```sh
stack install yesod-bin --install-ghc
stack install ghcid
```

### Dev mode

Setup database:

```sh
# with docker
source docker/dependencies.env
docker-compose up -d

# manually
sudo -u postgres psql
postgres=# CREATE USER twenty48 WITH PASSWORD 'twenty48';
postgres=# CREATE DATABASE twenty48 OWNER twenty48;
postgres=# GRANT ALL PRIVILEGES ON DATABASE twenty48 TO twenty48;
postgres=# \q

# verify setup
sudo -u postgres psql -c "\l" | grep twenty48
sudo -u postgres psql -c "\du" | grep twenty48
```

```sh
# with yesod
yesod devel

# or, using ghcid for subsecond code reload
make ghcid-yesod
```

### Optimized

```sh
stack build --exec twenty48
```

To run in a docker container with HTTPS, you'll first need to generate a [certificate for localhost](https://letsencrypt.org/docs/certificates-for-localhost/#making-and-trusting-your-own-certificates), and then run:

> NOTE:
> The docker build stopped working when `stack` removed the `image` command and I haven't bothered updating it.
> * https://github.com/commercialhaskell/stack/pull/4620/changes#diff-874f33563125619a7a5cb567ebe523c59258662d69858d78d24d947b275f9c6c
> * https://academy.fpblock.com/blog/2017/12/building-haskell-apps-with-docker/


```sh
make docker-build
source docker/nginx-localhost.env && docker-compose up -d
```

## Tests and benchmarks

```sh
stack test
```

```sh
stack bench
```

## Credits

Original game by [Gabriele Cirulli](https://gabrielecirulli.github.io/2048/).

Heuristic based on [Matt Overlan's](https://github.com/ovolve/2048-AI).

# 2048 Solver

A solver for the 2048 game using minimax and alpha-beta pruning, as described by John Hughes in the paper ["Why Functional Programming Matters"](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf).

The backend was written in Haskell using Yesod.
The decisions are streamed to the browser via a websockets connection.

Demo: <https://2048.diogocastro.com/>

## Run

For development, use [stack](https://docs.haskellstack.org/en/stable/) and either `yesod` or `ghcid`:

```sh
stack install yesod-bin --install-ghc
yesod devel
```

```sh
stack install ghcid
just ghcid-yesod
```

For an optimized build, use stack or nix:

```sh
stack run twenty48

nix build .#twenty48
```

You can also cross-compile to ARM64 (e.g. to deploy to a Raspberry Pi) using nix:

```sh
nix build .#twenty48-rpi
```

### Database

Setup a PostgreSQL database:

```sh
# with docker
source docker/dependencies.env
docker-compose up -d

# manually
sudo apt install postgresql
sudo -u postgres psql
postgres=# CREATE USER twenty48 WITH PASSWORD 'twenty48';
postgres=# CREATE DATABASE twenty48 OWNER twenty48;
postgres=# GRANT ALL PRIVILEGES ON DATABASE twenty48 TO twenty48;
postgres=# \q

# verify setup
sudo -u postgres psql -c "\l" | grep twenty48
sudo -u postgres psql -c "\du" | grep twenty48
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

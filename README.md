# 2048 AI

An AI for the 2048 game using minimax and alpha-beta pruning, as described by John Hughes in the paper ["Why Functional Programming Matters"](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf).

The AI was written in Haskell and runs in a Yesod backend.
The decisions are streamed to the browser via a websockets connection.

Demo: <https://2048.diogocastro.com/>

## Run

Install stack, libsass, and, optionally, docker and docker-compose.

```text
curl -sSL https://get.haskellstack.org/ | sh
brew install libsass
```

For development, you'll need either yesod or ghcid

```text
stack install yesod-bin --install-ghc
stack install ghcid
```

### Dev mode

```text
// setup database
source docker/dependencies.env
docker-compose up -d

// with yesod
yesod devel

// or, using ghcid for subsecond code reload
make ghcid-yesod
```

### Optimized

```text
stack build --exec twenty48
```

To run in a docker container with HTTPS, you'll first need to generate a [certificate for localhost](https://letsencrypt.org/docs/certificates-for-localhost/#making-and-trusting-your-own-certificates), and then run:

```text
make docker-build
source docker/nginx-localhost.env && docker-compose up -d
```

## Tests and benchmarks

```text
stack test
```

```text
stack bench
```

## Credits

Original game by [Gabriele Cirulli](https://gabrielecirulli.github.io/2048/).

Heuristic based on [Matt Overlan's](https://github.com/ovolve/2048-AI).

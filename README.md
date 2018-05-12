# 2048 AI

An AI for the 2048 game using minimax and alpha-beta pruning, as described by John Hughes in the paper ["Why Functional Programming Matters"](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf), written in Haskell + Yesod.

Demo: <https://2048.diogocastro.com/>

## Run

Install stack and, optionally, docker and docker-compose.

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

// or, using ghcid 0.7+ for subsecond code reload
ghcid
```

### Optimized

```text
stack build --exec twenty48
```

To run in a docker container with HTTPS, you'll first have to generate a [certificate for localhost](https://letsencrypt.org/docs/certificates-for-localhost/#making-and-trusting-your-own-certificates), and then run:

```text
docker/build.sh latest
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

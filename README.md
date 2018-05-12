# 2048 AI

An AI for the 2048 game using minimax and alpha-beta pruning, as described by John Hughes in the paper ["Why Functional Programming Matters"](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf), written in Haskell + Yesod.

Demo: <http://2048.diogocastro.com/>

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
docker-compose up -d

// with yesod
yesod devel

// or, using ghcid 0.7+ for subsecond code reload
ghcid
```

### Optimized

```text
stack build --exec twenty48

// or, in a container
docker/build.sh latest
docker/run.sh
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

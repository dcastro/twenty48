function GameManager(size, InputManager, Actuator, StorageManager, loginSvc) {
  this.size           = size; // Size of the grid
  this.inputManager   = new InputManager;
  this.storageManager = new StorageManager;
  this.actuator       = new Actuator;
  this.loginSvc       = loginSvc;
  this.showingTopScores = false;

  this.startTiles     = 2;

  this.inputManager.on("move", this.move.bind(this));
  this.inputManager.on("restart", this.restart.bind(this));
  this.inputManager.on("keepPlaying", this.keepPlaying.bind(this));
  this.inputManager.on("autoPlay", this.autoPlay.bind(this))
  this.inputManager.on("autoPlayOnce", this.autoPlayOnce.bind(this))
  this.inputManager.on("stopAutoPlay", this.stopAutoPlay.bind(this))
  this.inputManager.on("saveScore", () => this.saveScore(true))
  this.inputManager.on("showTopScores", () => this.showTopScores())

  const url = location.href.replace(/^(https:\/\/)|(http:\/\/)/, "ws://")
  this.conn = new WebSocket(url);

  this.autoPlaying = false;
  this.conn.onopen = () => this.setup();
}

GameManager.prototype.showTopScores = function() {

  if(this.showingTopScores) {
    this.actuator.hideTopScores();
  }
  else {
    const scoresPromise =
      this.loginSvc.userDetails().then(details => {
        const query = details === null? "" : "?user=" + details.email;

        return new Promise((fulfilled, rejected) => {
          $.get('/scores' + query)
            .done(fulfilled)
            .fail(rejected)
        });
      });
    this.actuator.showTopScores(scoresPromise);
  }
  this.showingTopScores = !this.showingTopScores;
}

GameManager.prototype.saveScore = function (promptSignIn) {
  this.getUserDetails(promptSignIn).then(details => {
    if (details !== null) {
      details.score = this.score;
      $.post('/score', JSON.stringify(details));
    } else {
      console.error("Can't save scores unless logged in");
    }
  })
}

GameManager.prototype.getUserDetails = function(promptSignIn) {
  return this.loginSvc.userDetails().then(details => {
    return details === null && promptSignIn ?
              this.loginSvc.signIn().then(() => this.loginSvc.userDetails()) :
              Promise.resolve(details);
  });
}

GameManager.prototype.autoPlay = function () {
  if (this.over)
    return;

  this.autoPlaying = true;

  const msg = {
    tag: "AutoPlayMsg",
    _board: this.grid.wsSerialize()
  };

  this.conn.send(JSON.stringify(msg));
  this.actuator.autoPlay();
}

GameManager.prototype.autoPlayOnce = function () {
  const msg = {
    tag: "AutoPlayOnceMsg",
    _board: this.grid.wsSerialize()
  };

  this.conn.send(JSON.stringify(msg));
}

GameManager.prototype.stopAutoPlay = function () {
  const msg = {
    tag: "StopMsg",
  };

  this.conn.send(JSON.stringify(msg));
  this.actuator.stopAutoPlay();
  this.autoPlaying = false;
}

// Restart the game
GameManager.prototype.restart = function () {
  this.storageManager.clearGameState();
  this.actuator.continueGame(); // Clear the game won/lost message
  this.setup();
};

// Keep playing after winning (allows going over 2048)
GameManager.prototype.keepPlaying = function () {
  this.keepPlaying = true;
  this.actuator.continueGame(); // Clear the game won/lost message
};

// Return true if the game is lost, or has won and the user hasn't kept playing
GameManager.prototype.isGameTerminated = function () {
  return this.over || (this.won && !this.keepPlaying);
};

// Set up the game
GameManager.prototype.setup = function () {
  var previousState = this.storageManager.getGameState();

  // Reload the game from a previous game if present
  if (previousState) {
    this.grid        = new Grid(previousState.grid.size,
                                previousState.grid.cells); // Reload grid
    this.score       = previousState.score;
    this.over        = previousState.over;
    this.won         = previousState.won;
    this.keepPlaying = previousState.keepPlaying;
  } else {
    this.grid        = new Grid(this.size);
    this.score       = 0;
    this.over        = false;
    this.won         = false;
    this.keepPlaying = false;

    // Add the initial tiles
    this.addStartTiles();
  }

  // Update the actuator
  this.actuate();

  this.conn.onmessage = msg => {
    const data = JSON.parse(msg.data);

    switch (data.tag) {
      case "PlayPlayerMsg":
        switch (data._direction) {
          // 0: up, 1: right, 2: down, 3: left
          case "U": this.move(0); break;
          case "R": this.move(1); break;
          case "D": this.move(2); break;
          case "L": this.move(3); break;
        }
        break;
      case "PlayComputerMsg":
        const cell = new Tile({ x: data._coord[0], y: data._coord[1], }, data._cell);
        this.grid.insertTile(cell);
        break;        
    }

    if (!this.movesAvailable()) {
      this.over = true; // Game over!
    }
    this.actuate();
  }
};

// Set up the initial tiles to start the game with
GameManager.prototype.addStartTiles = function () {
  for (var i = 0; i < this.startTiles; i++) {
    this.addRandomTile();
  }
};

// Adds a tile in a random position
GameManager.prototype.addRandomTile = function () {
  if (this.grid.cellsAvailable()) {
    var value = Math.random() < 0.9 ? 2 : 4;
    var tile = new Tile(this.grid.randomAvailableCell(), value);

    this.grid.insertTile(tile);
  }
};

// Sends the updated grid to the actuator
GameManager.prototype.actuate = function () {
  // Clear the state when the game is over (game over only, not win)
  if (this.over) {
    this.loginSvc.isSignedIn().then(signedIn => {
      if (signedIn) this.saveScore();
    });
    
    this.storageManager.clearGameState();
    this.actuator.stopAutoPlay();
    this.autoPlaying = false;
  } else {
    this.storageManager.setGameState(this.serialize());
  }

  this.loginSvc.isSignedIn().then(signedIn => {
    this.actuator.actuate(this.grid, {
      score:      this.score,
      over:       this.over,
      won:        this.won,
      terminated: this.isGameTerminated()
    },
    signedIn);
  });


};

// Represent the current game as an object
GameManager.prototype.serialize = function () {
  return {
    grid:        this.grid.serialize(),
    score:       this.score,
    over:        this.over,
    won:         this.won,
    keepPlaying: this.keepPlaying
  };
};

// Save all tile positions and remove merger info
GameManager.prototype.prepareTiles = function () {
  this.grid.eachCell(function (x, y, tile) {
    if (tile) {
      tile.mergedFrom = null;
      tile.savePosition();
    }
  });
};

// Move a tile and its representation
GameManager.prototype.moveTile = function (tile, cell) {
  this.grid.cells[tile.x][tile.y] = null;
  this.grid.cells[cell.x][cell.y] = tile;
  tile.updatePosition(cell);
};

// Move tiles on the grid in the specified direction
GameManager.prototype.move = function (direction) {
  // 0: up, 1: right, 2: down, 3: left
  var self = this;

  if (this.isGameTerminated()) return; // Don't do anything if the game's over

  var cell, tile;

  var vector     = this.getVector(direction);
  var traversals = this.buildTraversals(vector);
  var moved      = false;

  // Save the current tile positions and remove merger information
  this.prepareTiles();

  // Traverse the grid in the right direction and move tiles
  traversals.x.forEach(function (x) {
    traversals.y.forEach(function (y) {
      cell = { x: x, y: y };
      tile = self.grid.cellContent(cell);

      if (tile) {
        var positions = self.findFarthestPosition(cell, vector);
        var next      = self.grid.cellContent(positions.next);

        // Only one merger per row traversal?
        if (next && next.value === tile.value && !next.mergedFrom) {
          var merged = new Tile(positions.next, tile.value * 2);
          merged.mergedFrom = [tile, next];

          self.grid.insertTile(merged);
          self.grid.removeTile(tile);

          // Converge the two tiles' positions
          tile.updatePosition(positions.next);

          // Update the score
          self.score += merged.value;

          // The mighty 2048 tile
          if (merged.value === 2048) {
            self.won = true;
            self.loginSvc.isSignedIn().then(signedIn => {
              if (signedIn) self.saveScore();
            });

            self.stopAutoPlay();
          }
        } else {
          self.moveTile(tile, positions.farthest);
        }

        if (!self.positionsEqual(cell, tile)) {
          moved = true; // The tile moved from its original cell!
        }
      }
    });
  });

  if (moved) {
    if (!this.autoPlaying)
      this.addRandomTile();

    if (!this.movesAvailable()) {
      this.over = true; // Game over!
    }

    this.actuate();
  }
};

// Get the vector representing the chosen direction
GameManager.prototype.getVector = function (direction) {
  // Vectors representing tile movement
  var map = {
    0: { x: 0,  y: -1 }, // Up
    1: { x: 1,  y: 0 },  // Right
    2: { x: 0,  y: 1 },  // Down
    3: { x: -1, y: 0 }   // Left
  };

  return map[direction];
};

// Build a list of positions to traverse in the right order
GameManager.prototype.buildTraversals = function (vector) {
  var traversals = { x: [], y: [] };

  for (var pos = 0; pos < this.size; pos++) {
    traversals.x.push(pos);
    traversals.y.push(pos);
  }

  // Always traverse from the farthest cell in the chosen direction
  if (vector.x === 1) traversals.x = traversals.x.reverse();
  if (vector.y === 1) traversals.y = traversals.y.reverse();

  return traversals;
};

GameManager.prototype.findFarthestPosition = function (cell, vector) {
  var previous;

  // Progress towards the vector direction until an obstacle is found
  do {
    previous = cell;
    cell     = { x: previous.x + vector.x, y: previous.y + vector.y };
  } while (this.grid.withinBounds(cell) &&
           this.grid.cellAvailable(cell));

  return {
    farthest: previous,
    next: cell // Used to check if a merge is required
  };
};

GameManager.prototype.movesAvailable = function () {
  return this.grid.cellsAvailable() || this.tileMatchesAvailable();
};

// Check for available matches between tiles (more expensive check)
GameManager.prototype.tileMatchesAvailable = function () {
  var self = this;

  var tile;

  for (var x = 0; x < this.size; x++) {
    for (var y = 0; y < this.size; y++) {
      tile = this.grid.cellContent({ x: x, y: y });

      if (tile) {
        for (var direction = 0; direction < 4; direction++) {
          var vector = self.getVector(direction);
          var cell   = { x: x + vector.x, y: y + vector.y };

          var other  = self.grid.cellContent(cell);

          if (other && other.value === tile.value) {
            return true; // These two tiles can be merged
          }
        }
      }
    }
  }

  return false;
};

GameManager.prototype.positionsEqual = function (first, second) {
  return first.x === second.x && first.y === second.y;
};

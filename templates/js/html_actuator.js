function HTMLActuator() {
  this.tileContainer    = document.querySelector(".tile-container");
  this.scoreContainer   = document.querySelector(".score-container");
  this.messageContainer = document.querySelector(".game-message");

  this.score = 0;
}

HTMLActuator.prototype.actuate = function (grid, metadata, signedIn) {
  var self = this;

  window.requestAnimationFrame(function () {
    self.clearContainer(self.tileContainer);

    grid.cells.forEach(function (column) {
      column.forEach(function (cell) {
        if (cell) {
          self.addTile(cell);
        }
      });
    });

    self.updateScore(metadata.score);

    if (metadata.terminated) {
      if (metadata.over) {
        self.message(false, signedIn); // You lose
      } else if (metadata.won) {
        self.message(true, signedIn); // You win!
      }
    }

  });
};

// Continues the game (both restart and keep playing)
HTMLActuator.prototype.continueGame = function () {
  this.clearMessage();
};

HTMLActuator.prototype.clearContainer = function (container) {
  while (container.firstChild) {
    container.removeChild(container.firstChild);
  }
};

HTMLActuator.prototype.addTile = function (tile) {
  var self = this;

  var wrapper   = document.createElement("div");
  var inner     = document.createElement("div");
  var position  = tile.previousPosition || { x: tile.x, y: tile.y };
  var positionClass = this.positionClass(position);

  // We can't use classlist because it somehow glitches when replacing classes
  var classes = ["tile", "tile-" + tile.value, positionClass];

  if (tile.value > 2048) classes.push("tile-super");

  this.applyClasses(wrapper, classes);

  inner.classList.add("tile-inner");
  inner.textContent = tile.value;

  if (tile.previousPosition) {
    // Make sure that the tile gets rendered in the previous position first
    window.requestAnimationFrame(function () {
      classes[2] = self.positionClass({ x: tile.x, y: tile.y });
      self.applyClasses(wrapper, classes); // Update the position
    });
  } else if (tile.mergedFrom) {
    classes.push("tile-merged");
    this.applyClasses(wrapper, classes);

    // Render the tiles that merged
    tile.mergedFrom.forEach(function (merged) {
      self.addTile(merged);
    });
  } else {
    classes.push("tile-new");
    this.applyClasses(wrapper, classes);
  }

  // Add the inner part of the tile to the wrapper
  wrapper.appendChild(inner);

  // Put the tile on the board
  this.tileContainer.appendChild(wrapper);
};

HTMLActuator.prototype.applyClasses = function (element, classes) {
  element.setAttribute("class", classes.join(" "));
};

HTMLActuator.prototype.normalizePosition = function (position) {
  return { x: position.x + 1, y: position.y + 1 };
};

HTMLActuator.prototype.positionClass = function (position) {
  position = this.normalizePosition(position);
  return "tile-position-" + position.x + "-" + position.y;
};

HTMLActuator.prototype.updateScore = function (score) {
  this.clearContainer(this.scoreContainer);

  var difference = score - this.score;
  this.score = score;

  this.scoreContainer.textContent = this.score;

  if (difference > 0) {
    var addition = document.createElement("div");
    addition.classList.add("score-addition");
    addition.textContent = "+" + difference;

    this.scoreContainer.appendChild(addition);
  }
};

HTMLActuator.prototype.message = function (won, signedIn) {
  var type    = won ? "game-won" : "game-over";
  var message = won ? "You win!" : "Game over!";

  signedIn ? $("#save-score-button").hide() : $("#save-score-button").show();

  this.messageContainer.classList.add(type);
  this.messageContainer.getElementsByTagName("p")[0].textContent = message;
};

HTMLActuator.prototype.clearMessage = function () {
  // IE only takes one value to remove at a time.
  this.messageContainer.classList.remove("game-won");
  this.messageContainer.classList.remove("game-over");
};

HTMLActuator.prototype.autoPlay = function() {
  document.getElementsByClassName("auto-play-button")[0].style.display = 'none';
  document.getElementsByClassName("stop-auto-play-button")[0].style.display = 'block';
};

HTMLActuator.prototype.stopAutoPlay = function() {
  document.getElementsByClassName("auto-play-button")[0].style.display = 'block';
  document.getElementsByClassName("stop-auto-play-button")[0].style.display = 'none';
};

HTMLActuator.prototype.hideTopScores = function(topScores) {
  $(".top-scores-panel").fadeOut();
};

HTMLActuator.prototype.showTopScores = function(topScoresPromise) {

  $(".top-scores-tables tbody").html(Array(10).fill(loadingRow).map(function(f) { return f(); }));

  $(".top-scores-panel").fadeIn();

  topScoresPromise.then(function(topScores) {
    $(".my-scores-table tbody").html(scoreRows(topScores.myScores === null ? [] : topScores.myScores));
    $(".all-scores-table tbody").html(scoreRows(topScores.allScores));
  });
};

function loadingRow() {
  return $("<tr>").append(
    $("<td>").append($("<div>").html("&nbsp;").addClass("loading-scores"))
  );
}

function scoreRows (scores) {
  return rightPad(10, emptyRow,
    scores.map(function(s) {
      return $("<tr>")
        .append($("<td>").text(s.name))
        .append($("<td>").text(s.score))
    }));
}

function emptyRow() { return $("<tr>").append('<td>').append('<td>'); }

function rightPad(n, f, xs) {
  const need = n - xs.length;

  if (need <= 0)
    return xs;
  return xs.concat(Array(need).fill(f).map(function(f) { return f(); }));
}

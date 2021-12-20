// http://www.hackerrank.com/
// open JS console in Chrome, paste into JS console
// run:
//   Foo.playGames( 7, 99 ) 
// to play 99 games starting with the opening value of 7
Foo = (function () {
  var self = {autoContinue: true, regressions: {}};

  self.startGame = function (numCandies) {
    $.ajax({
      type: 'POST',
      url: '/splash/challenge.json',
      data: {
        n: numCandies,
        remote: true,
        utf8: '✓'
      },
      success: self.playRound
    });
  };

  self.nextMove = function (current) {
    var move = current % 6;
    if ( move === 0 ) {
      return 1;
    }
    return move;
  };

  self.playRound = function (data) {
    var nextMove, gameOver = false;
    console.log('play round: ' + JSON.stringify(data));
    if (data.message) {
      console.log(data.message);
    }

    if (data.exit) {
      gameOver = true;
    //  console.log('Game is over!');
    //  return;
    }

    if (data.game) {
      data = data.game;
    }

    if (data.solved) {
      console.log('Won the game :)');
      self.continue = null;
      return self.gameFinished();
    }

    if (gameOver) {
      console.log('Lost the game :(');
      self.continue = null;
      return self.gameFinished();
    }

    if (data.n !== self.currentValue) {
      console.error('ERROR: regression, data.n=%s self.currentValue=%s', data.n, self.currentValue);
      self.regressions[self.currentValue] = self.regressions[self.currentValue] || [];
      self.regressions[self.currentValue].push(data.n);
      // skip the game if it regresses
      self.playGames(self.currentValue-1);
      return;
    }

    // n the starting numCandies
    // current the number of candies left
    // limit: not sure wha thtis is
    // moves array of moves taken?
    nextMove = self.nextMove(data.current);
    // console.log('the next move is: ' + nextMove);
    if (nextMove < 1 ) {
      console.error('Whoops something went wrong, the next move is zero? ');
      console.dir(data);
      self.playGames(self.currentValue, self.numGames);
      return;
    }

    self.continue = function () {
      $.ajax({
        type: 'PUT',
        url: '/splash/challenge.json',
        data: {
          move: nextMove,
        remote: true,
        utf8: '✓'
        },
        success: self.playRound
      });
    };


    if (self.autoContinue) {
      self.continue();
    }
  };

  self.playGames = function (startValue) {
    self.currentValue = 6;
    if (startValue) {
      self.currentValue = startValue;
    }
    self.gameFinished = function () {
      if (self.currentValue >= 2560) {
        console.log('all games played');
        return;
      }
      if (self.currentValue < 1) {
        console.log('all games played');
        return;
      }
      //self.currentValue = self.currentValue + 1;
      self.currentValue = self.currentValue - 1;
      return self.startGame(self.currentValue);
    }
    return self.startGame(self.currentValue);
  };

  return self;
}());

// http://www.hackerrank.com/
// open JS console in Chrome, paste into JS console
// run:
//   Foo.playGames( 7, 99 ) 
// to play 99 games starting with the opening value of 7
Foo = (function () {
  var self = {autoContinue: false};

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
    return current % 6;
  };

  self.playRound = function (data) {
    var nextMove;
    console.log('play round: ' + JSON.stringify(data));
    if (data.message) {
      console.log(data.message);
    }

    //if (data.exit) {
    //  console.log('Game is over!');
    //  return;
    //}

    if (data.game) {
      data = data.game;
    }

    if (data.solved) {
      console.log('Game is solved!');
      self.continue = null;
      return self.gameWon();
    }

    // n the starting numCandies
    // current the number of candies left
    // limit: not sure wha thtis is
    // moves array of moves taken?
    nextMove = self.nextMove(data.current);
    console.log('the next move is: ' + nextMove);
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

  self.playGames = function ( startingValue, numGames ) {
    self.numGames = numGames;

    self.currentValue = startingValue - (startingValue % 6) + 1,

    self.gameWon = function () {
      if (self.numGames < 1) {
        console.log('games played, currentValue: ' + self.currentValue);
        return;
      }
      self.numGames = self.numGames - 1;
      self.currentValue += 6;
      console.log('playing next game[' + self.numGames + ']: ' + self.currentValue);
      return self.startGame(self.currentValue);
    };

    self.numGames = self.numGames - 1;
    console.log('playing next game[' + self.numGames + ']: ' + self.currentValue);
    return self.startGame(self.currentValue);
  };

  return self;
}());

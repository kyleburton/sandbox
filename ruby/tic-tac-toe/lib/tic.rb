# see: http://nealabq.com/content/src/tic_game_space.erl
# http://nealabq.com/blog/?p=1085
require 'rubygems'
require 'tic_tac_toe/board'
require 'tic_tac_toe/memory'
require 'tic_tac_toe/game'

module TicTacToe

  class App
    def initialize
    end

    def run
      game = TicTacToe::Game.new
      until game.board.winner
        puts game.board.print_to_string
        puts "It's #{game.board.curr_player}'s turn:"
        game.make_next_move
        puts ""
      end
    end
  end
end




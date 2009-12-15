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

    def build_board_db1
      if false
        b = Board.new("012345678")
        puts b.print_to_string
        puts ""
        b2 = b.mirror_down_diag
        puts b2.to_s
        puts b2.print_to_string
      end

      # to discover all the boards, iterate through all possible
      # boards using the various rotations to fold them into the board
      # store...

      # we have a trinary number system, ignore all 'zero's,
      # ' ' is zero, 'X' is 1, 'O' is 2

      max = 3**9
      num_valid = 0
      store = Memory.new
      if true
        (1..max).each do |board_as_i|
          board_b3 = sprintf "%09s", board_as_i.to_s(3)
          board_b3.gsub! '0', ' '
          board_b3.gsub! '1', 'X'
          board_b3.gsub! '2', 'O'
          ## many of these are invalid (# of X's and # of Y's must be
          ## within 1 of each other...)

          b = Board.new(board_b3)
          next unless b.valid?

          ## optional: only take games where X starts (meaning there are more X's than O's
          #if b.num_os > b.num_xs
          #  next
          #end

          # puts "new board # #{board_as_i} | #{board_b3}"
          num_valid = num_valid + 1
          store.add(b, {})
        end
      end
      puts "There were #{num_valid} valid boards"
      puts "After folding, there were #{store.boards.keys.length} boards"
      # this gets us down to 1261 boards, good enoguh for now, start
      # playing

      store.boards.values.each do |b|
        puts b[:board].to_s
        puts b[:board].print_to_string
        puts ""
      end

    end

    def build_board_db2 player='X',verbose=true
      if verbose
        puts "HINT: find boards where there is a win, plus extra moves"
        puts "that should collapse things further, since those boards"
        puts "can't be reached in a normal game, you need to assume"
        puts "that double win's won't happen since either player"
        puts "would win as soon as they could."
      end

      num_valid = 0
      store = Memory.new
      boards = [Board.new("         ",player == 'X' ? 'O' : 'X')]
      bnum = 0
      # TODO: need to do this starting with "O" too, not just "X"
      until boards.empty?
        bnum = bnum + 1
        board = boards.pop
        verbose and puts "==>"
        verbose and puts "Now it's #{board.next_player}'s turn, on board m:#{board.turn_no}; b:#{bnum}:"
        verbose and puts board.print_to_string_indented
        verbose and puts "==="
        if board.winner
          verbose and puts "{#{board.turn_no}} #{board.winner} won! no more moves on this board"
          next
        end

        unless board.valid?
          verbose and puts "{#{board.turn_no}} ERR: Board was invlaid?:\n#{board.print_to_string_indented}"
        end

        board.each_next_board do |b|
          if b.valid?
            verbose and puts "{#{board.turn_no}} next valid move:"
            verbose and puts "{#{board.turn_no}} move results in a win!: #{b.winner}" if b.winner
            num_valid = num_valid + 1
            boards.push(b) if store.add(b)
          else
            verbose and puts "{#{board.turn_no}} next move was invalid:"
          end
          verbose and puts b.print_to_string_indented

        end

      end

      puts "There were #{num_valid} valid boards"
      puts "After folding, there were #{store.boards.keys.length} boards"

      store
    end

    def run
      b = Board.new("XO       ")
      move = b.weighted_move
      puts "chosen move: #{b.weighted_move.inspect}"
      puts "chosen move: #{b.weighted_move.inspect}"
      puts "chosen move: #{b.weighted_move.inspect}"

      #playerX = { :player => 'X', :store => build_board_db2('X',false) }
      #playerO = { :player => 'O', :store => build_board_db2('O',false) }

      # a game consists
    end
  end
end




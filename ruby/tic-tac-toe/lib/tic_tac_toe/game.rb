module TicTacToe
  class Game
    def initialize(starting_player='X',players=nil,starting_board=nil)
      @board   = starting_board || Board.new("         ")
      @players = players
      unless @players
        @players = {
          'X' => { :store => Memory.new, :moves => [] },
          'O' => { :store => Memory.new, :moves => [] },
        }
        @players['X'][:store].construct_board_db
        @players['O'][:store].construct_board_db
      end

      @curr_player = starting_player
    end

    def reset
      @board = Board.new("         ")
      true
    end

    def make_next_move(coord=nil)
      if board.winner
        return nil
      end
      player = @players[@curr_player]
      coord = player[:store].next_move @board unless coord
      unless coord
        $stderr.puts "No more valid moves"
        return nil
      end
      $stderr.puts "Game.make_next_move: turn#=#{@board.turn_no} player #{@curr_player}'s next move: #{coord.inspect}"
      @board[coord[0],coord[1]] = @curr_player
      @curr_player = @curr_player == 'X' ? 'O' : 'X'
      player[:moves].push coord
      coord
    end

    def board
      @board
    end
  end
end

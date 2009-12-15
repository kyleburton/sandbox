module TicTacToe

  class Memory
    def initialize
      @boards = {}
    end

    def add(board,moves=nil)
      board = Board.new(board) unless board.is_a? Board
      found = nil
      board.rotations.each do |cand|
        if @boards[cand.to_s]
          found = cand
          break
        end
      end

      retval = false
      unless found
        # puts "Adding new board: #{board.to_s}"
        found = board
        retval = true
      end

      if moves.nil?
        moves = {}
        found.open_moves_xy.each do |coord|
          moves[coord.to_s] = { :coord => coord, :weight => 10 }
        end
      end
      @boards[found.to_s] = {:board => found, :moves => moves||{}}
      retval
    end

    def boards
      @boards
    end

    def construct_board_db player='X',verbose=false
      if verbose
        puts "HINT: find boards where there is a win, plus extra moves"
        puts "that should collapse things further, since those boards"
        puts "can't be reached in a normal game, you need to assume"
        puts "that double win's won't happen since either player"
        puts "would win as soon as they could."
      end

      num_valid = 0
      unexplored_boards = [Board.new("         ",player == 'X' ? 'O' : 'X')]
      bnum = 0
      # TODO: need to do this starting with "O" too, not just "X"
      until unexplored_boards.empty?
        bnum = bnum + 1
        board = unexplored_boards.pop
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
            unexplored_boards.push(b) if add(b)
          else
            verbose and puts "{#{board.turn_no}} next move was invalid:"
          end
          verbose and puts b.print_to_string_indented

        end

      end

      verbose and puts "There were #{num_valid} valid boards"
      verbose and puts "After folding, there were #{@boards.keys.length} boards"
      self
    end

    # be random for now...
    def next_move board
      moves = board.open_moves_xy
      idx = rand moves.length
      moves[idx]
    end

  end # class Memory
end

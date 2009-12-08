# see: http://nealabq.com/content/src/tic_game_space.erl
# http://nealabq.com/blog/?p=1085
module TicTacToe
  class Board
    def initialize(initval=nil)
      @board = (" "*9).split ''
      @board = initval.split '' if initval
    end

    def to_s
      "Board{#{@board.join('')}}"
    end

    def print_to_string
      [[@board[0], ' | ',@board[1],' | ',@board[2]].join(''),
       '--+---+--',
       [@board[3], ' | ',@board[4],' | ',@board[5]].join(''),
       '--+---+--',
       [@board[6], ' | ',@board[7],' | ',@board[8]].join('')
      ].join "\n";
    end

    def [](x,y)
      @board[x + (3*y)]
    end

    def []=(x,y,val)
      @board[x + (3*y)] = val
    end

    def rotate0
      self
    end

    def rotate90
      b = Board.new
      b[0,0] = self[0,2]
      b[1,0] = self[0,1]
      b[2,0] = self[0,0]

      b[0,1] = self[1,2]
      b[1,1] = self[1,1]
      b[2,1] = self[1,0]

      b[0,2] = self[2,2]
      b[1,2] = self[2,1]
      b[2,2] = self[2,0]

      b
    end

    def rotate180
      b = Board.new
      b[0,0] = self[2,2]
      b[1,0] = self[1,2]
      b[2,0] = self[0,2]

      b[0,1] = self[2,1]
      b[1,1] = self[1,1]
      b[2,1] = self[0,1]

      b[0,2] = self[2,0]
      b[1,2] = self[1,0]
      b[2,2] = self[0,0]

      b
    end

    def rotate270
      b = Board.new
      b[0,0] = self[2,0]
      b[1,0] = self[2,1]
      b[2,0] = self[2,2]

      b[0,1] = self[1,0]
      b[1,1] = self[1,1]
      b[2,1] = self[1,2]

      b[0,2] = self[0,0]
      b[1,2] = self[0,1]
      b[2,2] = self[0,2]

      b
    end

    def mirror_horiz
      b = Board.new
      b[0,0] = self[2,0]
      b[1,0] = self[1,0]
      b[2,0] = self[0,0]

      b[0,1] = self[2,1]
      b[1,1] = self[1,1]
      b[2,1] = self[0,1]

      b[0,2] = self[2,2]
      b[1,2] = self[1,2]
      b[2,2] = self[0,2]

      b
    end

    def mirror_vert
      # 0,0 | 1,0 | 2,0
      # ----+-----+----
      # 0,1 | 1,1 | 2,1
      # ----+-----+----
      # 0,2 | 1,2 | 2,2
      b = Board.new
      b[0,0] = self[0,2]
      b[1,0] = self[1,2]
      b[2,0] = self[2,2]

      b[0,1] = self[0,1]
      b[1,1] = self[1,1]
      b[2,1] = self[2,1]

      b[0,2] = self[0,0]
      b[1,2] = self[1,0]
      b[2,2] = self[2,0]

      b
    end

    def mirror_down_diag
      # 0,0 | 1,0 | 2,0
      # ----+-----+----
      # 0,1 | 1,1 | 2,1
      # ----+-----+----
      # 0,2 | 1,2 | 2,2
      # reflect along \
      #                 \
      #                   \
      b = Board.new
      b[0,0] = self[0,0]
      b[1,0] = self[0,1]
      b[2,0] = self[0,2]

      b[0,1] = self[1,0]
      b[1,1] = self[1,1]
      b[2,1] = self[1,2]

      b[0,2] = self[2,0]
      b[1,2] = self[2,1]
      b[2,2] = self[2,2]

      b
    end

    def mirror_up_diag
      #                    /
      #                  /
      # reflect along  /
      # 0,0 | 1,0 | 2,0
      # ----+-----+----
      # 0,1 | 1,1 | 2,1
      # ----+-----+----
      # 0,2 | 1,2 | 2,2
      b = Board.new
      b[0,0] = self[2,2]
      b[1,0] = self[2,1]
      b[2,0] = self[2,0]

      b[0,1] = self[1,2]
      b[1,1] = self[1,1]
      b[2,1] = self[1,0]

      b[0,2] = self[0,2]
      b[1,2] = self[0,1]
      b[2,2] = self[0,0]

      b
    end

    def rotations
      rots = []
      seen = {}
      [
       # these appear to be all the valid reflections + rotations
       # equiv to the longer list below...but faster to compute
       [:rotate0],
       [:rotate90],
       [:rotate180],
       [:rotate270],
       [:mirror_horiz],
       [:mirror_horiz,:rotate0],
       [:mirror_horiz,:rotate90],
       [:mirror_horiz,:rotate180],
       [:mirror_horiz,:rotate270],


#        [:rotate0],
#        [:rotate90],
#        [:rotate180],
#        [:rotate270],
#        [:mirror_horiz],
#        [:mirror_horiz,     :rotate90],
#        [:mirror_horiz,     :rotate180],
#        [:mirror_horiz,     :rotate270],
#        [:mirror_vert],
#        [:mirror_vert,      :rotate90],
#        [:mirror_vert,      :rotate180],
#        [:mirror_vert,      :rotate270],
#        [:mirror_down_diag],
#        [:mirror_down_diag, :rotate90],
#        [:mirror_down_diag, :rotate180],
#        [:mirror_down_diag, :rotate270],
#        [:mirror_up_diag],
#        [:mirror_up_diag,   :rotate90],
#        [:mirror_up_diag,   :rotate180],
#        [:mirror_up_diag,   :rotate270],

       # these don't seem to reduce any further...
       #[:mirror_horiz, :mirror_down_diag],
       #[:mirror_horiz, :mirror_up_diag],
       #[:mirror_vert,  :mirror_down_diag],
       #[:mirror_vert,  :mirror_up_diag],

       #[:mirror_down_diag, :mirror_up_diag],
       #[:mirror_up_diag,   :mirror_down_diag]

      ].each do |rtype|
        cand = self
        rtype.each do |m|
          cand = cand.send m
        end

        unless seen[cand.to_s]
          seen[cand.to_s] = cand
          rots.push cand
        end
      end

      rots
    end

    def rotation_of?(other)
      self.rotations.each do |cand|
        if other.to_s == cand.to_s
          return true
        end
      end
      return false
    end

    def num_xs
      @board.select {|x| x=='X'}.length
    end

    def num_os
      @board.select {|x| x=='O'}.length
    end

    def valid?
      diff = (num_xs - num_os).abs
      return diff < 2
    end

  end

  class Memory
    def initialize
      @boards = {}
    end

    def add(board,moves={})
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

      @boards[found.to_s] = {:board => found, :moves => moves||{}}
      retval
    end

    def boards
      @boards
    end
  end

  class App
    def initialize
    end

    def run
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

      puts "HINT: find boards where there is a win, plus extra moves"
      puts "that should collapse things further, since those boards"
      puts "can't be reached in a normal game, you need to assume"
      puts "that double win's won't happen since either player"
      puts "would win as soon as they could."
    end
  end
end




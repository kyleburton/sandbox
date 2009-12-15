module TicTacToe
  class Board
    POSITIONS_XY = [
                    [0,0],[1,0],[2,0],
                    [0,1],[1,1],[2,1],
                    [0,2],[1,2],[2,2]
                    ]
    POSITIONS = 0..8

    LINES = [
             # rows
             [[0,0],[1,0],[2,0]],
             [[0,1],[1,1],[2,1]],
             [[0,2],[1,2],[2,2]],

             # cols
             [[0,0],[0,1],[0,2]],
             [[1,0],[1,1],[1,2]],
             [[2,0],[2,1],[2,2]],

             # diags
             [[0,0],[1,1],[2,2]],
             [[2,0],[1,1],[0,2]],
            ]

    def initialize(initval=nil,curr_player='X')
      @board = (" "*9).split ''
      @board = initval.split '' if initval
      @curr_player = curr_player
      @weights = open_moves_xy.map do |coord|
        { :move => coord, :weight => 10 }
      end
    end

    def clone
      Board.new(@board.join(''))
    end

    def to_s
      "Board{#{@board.join('')}}"
    end

    def print_to_string(pfx='')
      [[pfx,' ', @board[0], ' | ',@board[1],' | ',@board[2], ' '].join(''),
       pfx + '---+---+---',
       [pfx,' ', @board[3], ' | ',@board[4],' | ',@board[5], ' '].join(''),
       pfx + '---+---+---',
       [pfx,' ', @board[6], ' | ',@board[7],' | ',@board[8], ' '].join('')
      ].join "\n";
    end

    def print_to_string_indented
      print_to_string(' ' * (turn_no-1))
    end

    def [](x,y)
      @board[x + (3*y)]
    end

    def []=(x,y,player)
      @curr_player = player
      @board[x + (3*y)] = @curr_player
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
      # num x's and o's must be within 1 of each other
      diff = (num_xs - num_os).abs
      return false unless diff < 2
      # can't have 2 winners
      return false if wins?('X') && wins?('O')
      return true
    end

    def open_moves_xy
      POSITIONS_XY.select do |pair|
        x,y = pair
        self[x,y] == ' '
      end
    end

    def each_next_board(player=next_player)
      each_next_move do |x,y|
        b = self.clone
        b[x,y] = player
        yield b
      end
    end

    def each_next_move
      open_moves_xy.each do |pt|
        x,y = pt
        yield x,y
      end
    end

    def winner
      return 'X' if wins?('X')
      return 'O' if wins?('O')
      return nil
    end

    def wins?(player)
      LINES.any? do |coords|
        coords.all? do |pt|
          x,y = pt
          self[x,y] == player
        end
      end
    end

    def turn_no
      10 - open_moves_xy.length
    end

    def curr_player
      @curr_player
    end

    def curr_player=(p)
      @curr_player = p
    end

    def next_player
      @curr_player == 'X' ? 'O' : 'X'
    end

    def weighted_move
      total     = @weights.inject 0 do |t,w| t+w[:weight]; end
      val       = rand(total)
      sorted    = @weights.sort {|l,r| l[:weight] <=> r[:weight] }
      # puts "Board.weighted_move: len=#{@weights.size}; total=#{total}; val=#{val}; sorted=#{sorted.inspect}"
      res = nil
      sorted.each do |pair|
        if val <= 0
          return pair[:move]
        end
        val = val - pair[:weight]
      end
      sorted[-1][:move]
    end

    def weights
      @weights
    end


  end # clas Board
end

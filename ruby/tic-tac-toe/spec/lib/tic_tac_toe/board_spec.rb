$:.unshift(File.join(File.dirname(__FILE__), %w[.. .. .. lib]))
require 'tic'

describe TicTacToe::Board do
  it "should stringify itself" do
    str = "012345678"
    b = TicTacToe::Board.new str
    b.to_s.should == "Board{#{str}}"
  end

  it "should render to a string containing all the moves" do
    b = TicTacToe::Board.new "012345678"
    b.print_to_string.should =~ /0/
    b.print_to_string.should =~ /1/
    b.print_to_string.should =~ /2/
    b.print_to_string.should =~ /3/
    b.print_to_string.should =~ /4/
    b.print_to_string.should =~ /5/
    b.print_to_string.should =~ /6/
    b.print_to_string.should =~ /7/
    b.print_to_string.should =~ /8/
    lines = b.print_to_string.split /\n/
    lines.length.should == 5
  end

  it "should rotate itself" do
    b = TicTacToe::Board.new "012345678"
    b.rotate90.rotate90.to_s.should == b.rotate180.to_s
    b.rotate90.rotate90.rotate90.to_s.should == b.rotate270.to_s
    b.rotate90.rotate90.rotate90.rotate90.to_s.should == b.to_s
  end

  it "should reflect itself" do
    b = TicTacToe::Board.new "012345678"
    b.mirror_horiz.mirror_horiz.to_s.should == b.to_s
    b.mirror_vert.mirror_vert.to_s.should == b.to_s
    b.mirror_down_diag.mirror_down_diag.to_s.should == b.to_s
    b.mirror_up_diag.mirror_up_diag.to_s.should == b.to_s
  end

  it "should report open moves" do
    b = TicTacToe::Board.new "         "
    b.open_moves_xy.should == TicTacToe::Board::POSITIONS_XY
    b.open_moves_xy.length.should == 9
    b = TicTacToe::Board.new "01234567 "
    b.open_moves_xy.length.should == 1
    b.open_moves_xy.should == [[2,2]]

    b = TicTacToe::Board.new "XXOOOXXOX"
    b.open_moves_xy.length.should == 0
    b.open_moves_xy.should == []

    b = TicTacToe::Board.new "         "
    b.open_moves_xy.length.should == 9
  end

  it "should report a winner" do
    b = TicTacToe::Board.new "         "
    b.wins?('X').should be_false
    b.wins?('O').should be_false

    b = TicTacToe::Board.new "      XXX"
    b.wins?('X').should be_true
    b.wins?('O').should be_false
    b.winner.should == 'X'

    b = TicTacToe::Board.new "X   X   X"
    b.wins?('X').should be_true
    b.wins?('O').should be_false
    b.winner.should == 'X'

    b = TicTacToe::Board.new "O   O   O"
    b.wins?('X').should be_false
    b.wins?('O').should be_true
    b.winner.should == 'O'
  end

  it "should report valid boards" do
    b = TicTacToe::Board.new "         "
    b.valid?.should be_true

    b = TicTacToe::Board.new "O   O    "
    b.valid?.should be_false

    b = TicTacToe::Board.new "O  XO  XO"
    b.valid?.should be_true

    b = TicTacToe::Board.new "XXXOOOXXX"
    b.valid?.should be_false

    b = TicTacToe::Board.new "XXXXXXXXX"
    b.valid?.should be_false
  end

  it "should report the number of moves per player" do
    b = TicTacToe::Board.new "         "
    b.num_xs.should == 0
    b.num_os.should == 0

    b = TicTacToe::Board.new "XOX      "
    b.num_xs.should == 2
    b.num_os.should == 1

    b = TicTacToe::Board.new "XXXXXXXXX"
    b.num_xs.should == 9
    b.num_os.should == 0
  end

  it "should report the turn number and next player" do
    b = TicTacToe::Board.new "         "
    b.turn_no.should == 1
    b.curr_player.should == 'X'
    b.next_player.should == 'O'

    b = TicTacToe::Board.new "XOX      "
    b.turn_no.should == 4
    b.curr_player.should == 'X'
    b.next_player.should == 'O'
  end

  it "should pick a move via the weight table" do
    b = TicTacToe::Board.new "XOX      "
    b.weights.each do |w|
      w[:weight] = 0
    end
    b.weights[0][:weight] = 100
    b.weighted_move.should == b.weights[0][:move]
  end

  it "should unrotate a move" do
    b = TicTacToe::Board.new "         "
    b.unrotate_move([0,0], [:rotate90    ]).should == [0,2]
    b.unrotate_move([0,0], [:rotate180   ]).should == [2,2]
    b.unrotate_move([0,0], [:rotate270   ]).should == [2,0]
    b.unrotate_move([0,0], [:mirror_horiz]).should == [0,2]
  end

end

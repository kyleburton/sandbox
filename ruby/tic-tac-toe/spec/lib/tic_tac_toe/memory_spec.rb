$:.unshift(File.join(File.dirname(__FILE__), %w[.. .. .. lib]))
require 'tic'

describe TicTacToe::Memory do

  it "should add a board only once" do
    m = TicTacToe::Memory.new
    b = TicTacToe::Board.new "X        "
    m.lookup(b).should be_nil
    m[b].should be_nil
    m.add(b).should be_true
    m[b].should be_true
    m.add(b).should be_false

    m.add(b.rotate90).should be_false
    m.add(b.rotate180).should be_false
    m.add(b.rotate270).should be_false
    m.add(b.mirror_horiz).should be_false
    m.add(b.mirror_vert).should be_false
    m.add(b.mirror_up_diag).should be_false
    m.add(b.mirror_down_diag).should be_false
  end

  it "should construct the board database" do
    m = TicTacToe::Memory.new
    m.construct_board_db
    m.boards.keys.should_not be_empty

    m.lookup(TicTacToe::Board.new("         ")).should_not be_nil
  end

end

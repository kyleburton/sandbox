$:.unshift(File.join(File.dirname(__FILE__), %w[.. lib]))
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
end

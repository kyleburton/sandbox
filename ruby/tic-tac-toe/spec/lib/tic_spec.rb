$:.unshift(File.join(File.dirname(__FILE__), %w[.. lib]))
require 'tic'

describe TicTacToe::App do

  it "should initialize" do
    TicTacToe::App.new.should_not be_nil
  end

end

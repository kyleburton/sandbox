$:.unshift(File.join(File.dirname(__FILE__), %w[.. .. .. lib]))
require 'tic'

describe TicTacToe::Game do
  before do
    @game = TicTacToe::Game.new
  end

  it "should initialize" do
    @game.should_not be_nil
  end

  it "should play a few turns" do
    @game.make_next_move
    $stderr.puts @game.board.print_to_string
    @game.make_next_move
    $stderr.puts @game.board.print_to_string
    @game.make_next_move
    $stderr.puts @game.board.print_to_string
  end

  it "should play a game until there is a winner" do
    (1..10).each do |turn|
      @game.make_next_move
      $stderr.puts @game.board.print_to_string
      break if @game.board.winner
    end
    @game.board.winner.should_not be_nil
  end
end

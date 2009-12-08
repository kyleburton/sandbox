#!/usr/bin/env ruby
$:.unshift(File.join(File.dirname(__FILE__), %w[.. lib]))
require 'tic'

TicTacToe::App.new.run


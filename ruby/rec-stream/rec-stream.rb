#!/usr/bin/env ruby
require 'rubygems'
require 'fastercsv'
require 'uri'

class RECStream
  attr_reader :headers
  attr_reader :uri

  @@drivers = {}

  def self.register(scheme,impl)
    if @@drivers[scheme]
      raise "Error: driver:#{impl} already registered for scheme:#{scheme} : #{@@drivers}"
    end
    @@drivers[scheme] = impl
  end

  def self.get_reader(input_uri)
    uri = URI.parse(input_uri)
    scheme = uri.scheme
    unless @@drivers[scheme]
      raise "Error: no driver registered for scheme:#{scheme} : #{@@drivers}"
    end
    @@drivers[scheme].new_reader(uri)
  end

  def self.get_writer(output_uri)
    uri = URI.parse(output_uri)
    scheme = uri.scheme
    unless @@drivers[scheme]
      raise "Error: no driver registered for scheme:#{scheme} : #{@@drivers}"
    end
    @@drivers[scheme].new_writer(uri)
  end

  def next_record_map
    rec = next_record
    return nil if rec.nil?
    rec_to_map rec
  end

  def rec_to_map
    ret = {}
    headers.each_with_index do |field,idx|
      ret[field] = rec[idx]
    end
    ret
  end

  def each_record
    rec = next_record
    while rec
      yield rec
      rec = next_record
    end
  end

  def each_record_map
    rec = next_record
    while rec
      yield rec_to_map rec
      rec = next_record
    end
  end

end

class CSVStream < RECStream
  register 'csv', CSVStream

  def self.new_reader(input_uri)
    rdr = CSVStream.new
    rdr.instance_eval do
      @uri = input_uri
      @input_file = @uri.path
      @headers, *lines = FasterCSV.read(@input_file)
      @lines = lines
      @curr_row = 0
    end
    return rdr
  end

  def self.new_writer(output_file,headers=nil)
    wrtr = CSVStream.new
    wrtr.instance_eval do
      @uri = input_uri
      @output_file = @uri.path
      set_headers headers if headers
    end
    wrtr
  end

  def set_headers(headers)
    @output = File.open(@output_file,"w")
    @headers = headers
    @output.puts headers.to_csv
  end

  def next_record
    return nil if @curr_row >= @lines.size
    row = @lines[@curr_row]
    @curr_row += 1
    row
  end

  def write_record(rec)
    @output.puts rec.to_csv
  end

  def close
    @output.close
  end
end

class TABStream < RECStream
  register 'tab', TABStream
  def self.new_reader(input_file)
    rdr = TABStream.new
    rdr.instance_eval do
      @uri = input_uri
      @input_file = @uri.path
      @input_file = input_file
      @headers, @lines = File.read(@input_file)
      @headers = @header.split /\t/
      @curr_row = 0
    end
    rdr
  end

  def self.new_writer(output_uri,headers=nil)
    wrtr = TABStream.new
    wrtr.instance_eval do
      @uri = output_uri
      @output_file = @uri.path
      set_headers headers if headers
    end
    wrtr
  end

  def set_headers(headers)
    @output = File.open(@output_file,"w")
    @headers = headers
    @output.puts headers.join "\t"
  end

  def next_record
    return nil if @curr_row >= @lines.size
    row = @lines[@curr_row].split /\t/
    @curr_row += 1
    row
  end

  def write_record(rec)
    @output.puts rec.map {|f| f.nil? ? nil : f.gsub(/\t/, '\\t') }.join "\t"
  end

  def close
    @output.close
  end
end

class RecView
  def initialize(inp)
    @inp = inp
    @max_width = @inp.headers.map {|h| h.size}.max
    @row = 0
  end

  def next_record
    rec = @inp.next_record
    return nil unless rec
    printf "rec: [% 4d] %s\n", @row+1, @inp.uri
    @inp.headers.each_with_index do |field,idx|
      printf " [% 3d] % *s : %s\n", idx+1, @max_width, field, rec[idx]
    end
    true
  end

  def self.stream_all(inp)
    viewer = RecView.new(inp)
    while viewer.next_record
      true
    end
  end
end

if false
  # example program prints your gmail / google contacts
  contacts_uri = "csv://#{ENV['HOME']}/Desktop/contacts.csv"
  puts "contacts_uri=#{contacts_uri}"

  ## Convert CSV to tab (stdout)
  # if false
  #   outp = RECStream.get_writer "tab:///dev/stdout"
  #   outp.set_headers inp.headers
  #   inp.each_record do |rec|
  #     outp.write_record rec
  #   end
  # end

  # RecView.stream_all(inp)


  names = DATA.readlines
  names.each do |name|

    inp = RECStream.get_reader contacts_uri

    inp.each_record_map do |rec|
    end
  end
end

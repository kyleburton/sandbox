require 'rubygems'

class LandmarkParser
  def initialize doc
    @doc = doc
    @ldoc = @doc.downcase
    @pos = 0
  end

  def apply_commands cmds
    cmds.each do |cmd|
      m, *args = cmd
      m = m.to_sym
      if self.respond_to? m
        unless self.send m, *args
          return nil
        end
      else
        raise "Error: unsupported command: #{m} '#{args.inspect}'"
      end
    end
  end

  def forward_to pat
    pos = @ldoc.index pat.downcase, @pos
    if pos
      @pos = pos
      true
    else
      false
    end
  end

  def forward_past pat
    pos = @ldoc.index pat.downcase, @pos
    if pos
      @pos = pos + pat.length
      true
    else
      false
    end
  end

  def rewind_to pat
    pos = @ldoc.rindex pat.downcase, @pos
    if pos 
      @pos = pos + pat.length
      true
    else
      false
    end
  end

  def rewind_past pat
    pos = @ldoc.rindex pat.downcase, @pos
    if pos 
      @pos = pos
      true
    else
      false
    end
  end

  def extract start_pats, end_pats
    orig_pos = @pos
    unless apply_commands start_pats
      @pos = orig_pos
      return nil
    end

    start_pos = @pos
    unless apply_commands end_pats
      @pos = orig_pos
      return nil
    end
    end_pos = @pos
    @doc[start_pos,end_pos-start_pos]
  end

  def extract_all start_pats, end_pats
    results = []
    while res = extract(start_pats, end_pats)
      results << res
    end
    results
  end

  def self.table_rows html
    self.new(html).extract_all([[:forward_to, "<tr"]], [[:forward_past,"</tr>"]])
  end

  def self.table_cells html
    self.table_rows(html).map do |row|
      stuff = self.new(row).extract_all([[:forward_to, "<td"]], [[:forward_past,"</td>"]])
      stuff
    end
  end

  def self.extract html, startp, endp
    self.new(html).extract(startp,endp)
  end

  def self.extract_all html, startp, endp
    self.new(html).extract_all(startp,endp)
  end
end


if false
  file = "span.html"
  unless File.exists?(file)
    system "curl -o #{file} http://some.url/"
  end

  start_pats = [
    [:forward_past, "Some Landmark"],
    [:forward_to,   "<table"],
  ]

  end_pats = [[:forward_past, "</table>"]]

  data = File.read(file)
  lp = LandmarkParser.new data
  res = lp.extract start_pats, end_pats
  raise "fail" unless res
  rows = LandmarkParser.table_rows(res)
  puts "rows=#{rows.inspect}"
end

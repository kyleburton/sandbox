#!/usr/bin/env ruby
unless 3 == ARGV.size 
  puts "#$0 <field-widths> <input-file> <output-file>"
  exit -1
end

field_widths = ARGV[0].split(',').map &:to_i
input_file   = ARGV[1]
output_file  = ARGV[2]

field_pattern = "A#{field_widths.join('A')}"
outp = File.open(output_file,"w")
File.foreach filename do |line|
  row = line.unpack field_pattern
  outp.puts row.join "\t"
end
outp.close

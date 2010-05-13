# example that [should] shows IO buffering on the PIPE
# the parent prints 3 lines to the child, the child reads1
# and 'exits', before it exits it shows the next 100 bytes
# awaiting it on its stdin, the parent not knowing if the
# child read all the data
def child  io
  puts "[#{$$}] child: reading"
  line = io.readline
  puts "[#{$$}] child: line='#{line}'"
  extra = io.read_nonblock 100
  puts "[#{$$}] child: extra='#{extra}'"
  exit
end

def parent io, pid
  puts "[#{$$}] parent: printing to child"
  io.puts "firstline\nsecondline"
  io.puts "thirdline"
  io.flush
  puts "[#{$$}] parent: waiting the child #{pid}"
  res = Process.waitpid pid
  puts "[#{$$}] parent: child exited #{pid} res: #{res}"
  $stdout.flush
  exit
end

puts "[#{$$}] main: created pipe"
rd, wr = IO.pipe

puts "[#{$$}] main: forking..."
if pid = fork
  rd.close
  parent wr, pid
else
  wr.close 
  child rd
end

puts "[#{$$}] main: after the fork"

lines = File.readlines(ARGV[0])

imports  = File.open(".grake/.imports",  "w")
toplevel = File.open(".grake/.toplevel", "w")
tasks    = File.open(".grake/.tasks",    "w")

while ! lines.empty?
  line = lines.shift
  if line =~ /^import\s*\(/
    while ! lines.empty?
      line = lines.shift
      if line =~ /^\s*\)\s*/
        break
      end
      imports.write line
    end
    next
  end

  if line =~ /^\s*func/
    while ! lines.empty?
      toplevel.write line
      line = lines.shift
      if line =~ /^}\s*$/
        toplevel.write line
        break
      end
    end
    next
  end

  tasks.write line
end

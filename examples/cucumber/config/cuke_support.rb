base_dir = File.dirname(__FILE__) + "/../features"
Dir["#{base_dir}/**/*.rb"].each do |f|
  puts "...requiring #{f}"; require f
end


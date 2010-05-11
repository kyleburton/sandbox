
def shorten_string thing, max_size=25
  s = thing.inspect
  if s.size > max_size
    s[0,max_size-1] + "..."
  else
    s
  end
end

def print_response response
  puts "response is: #{response}"
  [:code, :time, :headers, :headers_hash, :body].each do |prop|
    printf "    %15s [%4d]: %s\n",
      prop, response.send(prop).to_s.size,
      shorten_string(response.send(prop))
  end
end


def describe_num_guesses max, attempts, n
  if n == 1
    "Amazing! you got it on the first guess!"
  elsif n == 2
    "Smart! only #{n} guesses wow!"
  elsif n <= max/4
    "in only #{n} guesses, great job!"
  elsif n <= max/2
    "in only #{n} guesses, good job!"
  else
    "you did it in #{n} guesses!"
  end
end

def play max=10, attempts=-1
  puts "Between 1 and #{max}"
  num_guesses = 0
  target = rand(max).to_i + 1
  while attempts != 0
    print "Your guess?: "
    guess = gets.to_i
    num_guesses += 1
    attempts -= 1
    if guess == target
      puts "correct! (#{describe_num_guesses max, attempts, num_guesses})"
      return true
    elsif guess < target
      puts "Too low"
    else guess < target
      puts "Too high"
    end
  end
end


$stdout.sync = true
max = (ARGV.shift || 10).to_i
attempts = (ARGV.shift || -1).to_i
play max, attempts

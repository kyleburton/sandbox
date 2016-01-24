# • Print the string “Hello, world.”
puts "Hello, world."
# • For the string “Hello, Ruby,” find the index of the word “Ruby.”
puts "Hello, Ruby".index("Ruby")
# • Print your name ten times.
10.times do ||
  puts "your name"
end
# • Print the string “This is sentence number 1,” where the number 1 changes from 1 to 10.
(1..10).each do |ii|
  puts "This is sentence number #{ii}"
end
# • Run a Ruby program from a file.
# run this file:
#    ruby day1.rb
#

def guessing_game max=10
  answer = rand(max).to_i + 1
  puts "What is your guess?:"
  while true
    guess = STDIN.readline().to_i
    if answer == guess
      puts "CORRECT! you win"
      break
    end

    if guess > answer
      puts "Hrm...too high, try again:"
      next
    end

    if guess < answer
      puts "Aww...too low, try again:"
      next
    end

    raise "WHOAH, I'm confuesed, please find my author and have a few choice words with them."
  end
end

guessing_game 100


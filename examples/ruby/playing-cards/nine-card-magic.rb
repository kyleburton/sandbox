# From http://boingboing.net/2013/01/21/foolproof-card-trick-your-kids.html
# Curious, I wanted to see it work for each card in the deck

$suits = %w[H D C S]
$numbers = %w[2 3 4 5 6 7 8 9 T J Q K A]
$verbose = ENV['DEBUG'] ? true : false

$card_to_letters = {
   'H' => 'HEARTS',
   'D' => 'DIAMONDS',
   'C' => 'CLUBS',
   'S' => 'SPADES',
   '2' => 'TWO',
   '3' => 'THREE',
   '4' => 'FOUR',
   '5' => 'FIVE',
   '6' => 'SIX',
   '7' => 'SEVEN',
   '8' => 'EIGHT',
   '9' => 'NINE',
   'T' => 'TEN',    # allow 10 be 1 char, makes implementation simpler
   'J' => 'JACK',
   'Q' => 'QUEEN',
   'K' => 'KING',
   'A' => 'ACE'
}

def full_deck
  res = []
  $suits.each do |suit|
    $numbers.each do |num|
      res << num + suit
    end
  end
  res
end


def deal deck, num
  res = []
  num.times do |ii|
    res << deck.shift
  end
  res
end

def choose_pile piles
  pile = piles.slice!(rand(piles.size),1).first
  [ pile, piles[0].concat(piles[1]) ]
end

def speak_and_deal pile, word
  p1 = []
  word.split('').each do |letter|
    card = pile.shift
    $verbose and puts letter + " " + p1.inspect + " " + card + " " + pile.inspect
    p1.unshift card
  end
  pile.concat(p1)
end


def play_game deck=full_deck.shuffle

  $verbose and puts "Deck: #{deck.inspect}"

  piles = [deal(deck,3), deal(deck,3), deal(deck,3)]

  $verbose and puts piles.inspect

  chosen_pile, rest = choose_pile(piles);
  $verbose and puts chosen_pile.inspect
  $verbose and puts rest.inspect

  chosen_card = chosen_pile[-1]
  $verbose and puts "#{chosen_card} was chosen"

  pile = chosen_pile.concat(rest)

  pile = speak_and_deal pile, $card_to_letters[chosen_card[0]]
  $verbose and puts
  pile = speak_and_deal pile, 'OF'
  $verbose and puts
  pile = speak_and_deal pile, $card_to_letters[chosen_card[1]]
  $verbose and puts
  pile = speak_and_deal pile, 'MAGI'
  $verbose and puts 
  $verbose and puts "Your Card was: #{pile[0]}"

  {:chosen_card => chosen_card,
   :pile        => pile,
   :passed      => pile[0] == chosen_card}
end

# run a random game
def random_game
  result = play_game full_deck.shuffle

  if result[:passed]
    puts "PASSED"
    exit 0
  else
    puts "FAILED"
    puts result.inspect
    exit 0
  end
end

def run_all_games
  stats = {
    :passed => 0,
    :failed => 0,
    :runs   => 0
  }
  52.times do |ii|
    deck = full_deck
    deck = deck.slice(ii,deck.size-ii).concat(deck.slice(0,ii))
    $verbose and puts deck.inspect
    result = play_game deck
    stats[:runs] += 1
    if result[:passed]
      stats[:passed] += 1
    else
      stats[:failed] += 1
    end
  end

  puts stats.inspect
end

# full_deck.each do |card|
#   num  = card[0]
#   suit = card[1]
#   letters = $card_to_letters[num] + 'OF' + $card_to_letters[suit]
#   puts "#{letters}"
#   printf "% 3s %s %s\n", $card_to_letters[num].size,  (9 % $card_to_letters[num].size),  $card_to_letters[num]
#   printf "% 3s %s %s\n", 'OF'.size,                   (9 % 'OF'.size),                   'OF'
#   printf "% 3s %s %s\n", $card_to_letters[suit].size, (9 % $card_to_letters[suit].size), $card_to_letters[suit]
#   printf "% 3s %s %s\n", 'MAGIC'.size,                (9 % 'MAGIC'.size),                'MAGIC'
# end

# random_game
run_all_games

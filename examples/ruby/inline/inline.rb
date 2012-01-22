require 'rubygems'
require 'inline'

class InlineTest
  inline do |builder|
    builder.c "
    long dummy(int input) {
      long i = 1;
      while (input >= 1 ) {
        input--; 
        i *= 2;
      }
      return i;
    }
    "
  end
end

puts InlineTest.dummy(8)

require 'myfirst'

RSpec.describe Myfirst, "Testing myfirst" do
    context "this is an alias for describe / a group" do
        it "should do nothing" do
            res = Myfirst.some_func
            expect(res).to eq "not really"
        end
    end
end

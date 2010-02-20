import com.github.kyleburton.SomeClass
# import org.mockito.Mockito

describe SomeClass do
  before(:each) do
    @the_name = "a user"
    @the_url = "http://asymmetrical-view.com/"
    @some_class = SomeClass.new @the_name, @the_url
  end

  it "should accept constructor parameters" do
    @some_class.user_name.should == @the_name
    @some_class.remote_url.should == @the_url
  end

#   it "should attempt to download when you ask for the content" do

#   end
end

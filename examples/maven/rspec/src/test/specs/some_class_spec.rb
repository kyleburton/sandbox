import com.github.kyleburton.SomeClass
import org.mockito.Mockito

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

  it "should download the conten when content is accessedt" do
    some_content = "this is some content"
    downloader = Mockito.mock(SomeClass::Downloader.java_class)
    Mockito.when(downloader.download()).then_return(some_content)
    @some_class.downloader = downloader
    @some_class.content.should_not be_empty
    @some_class.content.should == some_content
  end
end

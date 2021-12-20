require 'rubygems'


SPEC = Gem::Specification.new do |s|
  s.name = "lparser"
  s.version = "0.0.1"
  s.author = "Kyle Burton"
  s.email = "kyle.burton@gmail.com"
  s.platform = Gem::Platform::RUBY
  s.description = <<DESC
Landmark Parsing and Extraction For Semi-Structured Documents.

This is a parsing technique for semi-structured documents.  The definition of
'semi structured' includes, but is not limited to, HTML, XML, Programming Language
text, and many other textual document formats.  

The approach works by navigating the document by locating absolute or relative
'landmarks' within the text and delimiting text to be extracted between
landmarks.  Landmarks may be part of the document's strucutre (as with an HTML
tag), or part of the document content (eg: a phone number).

The approach is somewhat agnostic to the document's actual strucutre and may
work well for you in situations where you have mixed content or are automating
the determination of the landmarks themselves.

DESC
  s.summary = "Landmark Parsing and Extraction For Semi-Structured Documents."
  # s.rubyforge_project = "typrtail"
  s.homepage = "http://github.com/kyleburton/sandbox/ruby/gems/lparser"
  s.files = Dir.glob("**/*")
  s.require_path = "lib"
  s.has_rdoc = false
end

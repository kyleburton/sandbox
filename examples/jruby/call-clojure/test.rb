require 'java'
#require 'clojure-1.0.0.jar'
Dir["#{File.dirname(__FILE__)}/*.jar"].each { |jar| puts "requiring: #{jar}"; require jar }
import "clojure.lang.RT"

class CljHelper
  def initialize *pkgs
    @mappings = {}
    @ns_map  = RT.var "clojure.core", "ns-map"
    @symbol  = RT.var "clojure.core", "symbol"
    @require = RT.var "clojure.core", "require"
    pkgs.each do |pkg|
      _import pkg
    end
  end

  # TODO: can we import all of a package's symbols?
  def _import pkg_name, sym=nil, sym_alais=nil
    #pkg_name = pkg_name.gsub '-', '_'
    puts "calling clojure.core/require #{pkg_name}"
    @require.invoke @symbol.invoke(pkg_name)
    if sym
      sym_alias ||= sym
      @mappings[sym_alias] = RT.var pkg_name, sym
      return
    end
    puts "Importing all symbols from #{pkg_name}"
    pkg = @symbol.invoke pkg_name
    puts " => #{pkg}"
    @ns_map.invoke(pkg).each do |sym,var|
      #puts "importing: #{sym} => #{var}"
      @mappings[sym.to_s] = var
    end
  end

  def _invoke m, *args
    fun = @mappings[m.to_s] || @mappings[m.to_s.gsub "_", "-"]
    # TODO: should we try to auto-lookup the function?  Right now you have to
    # import it first...maybe auto lookup would be faster...
    unless fun
      raise "Error, no current binding for symbol=#{m}"
    end
    fun.invoke *args
  end

  def _alias new, old
    @mappings[new] = @mappings[old]
  end

  def method_missing symbol, *args
    _invoke symbol, *args
  end

end

core = CljHelper.new
puts "core=#{core}"
#core._import 'clojure.core', 'prn'
core._import 'clojure.core'
core.prn "Hello Clojure, this is JRuby"
core._invoke "prn", "Hello Clojure, this is JRuby"

str_utils = CljHelper.new 'clojure.contrib.str-utils'
puts "Join: #{str_utils._invoke "str-join", ":", ["a", "b", "c"]}"
puts "Join: #{str_utils.str_join ":", ["a", "b", "c"]}"

m = {:a => 1, :b => 2}
puts "keys: #{core.keys(m)}"

clj_xpath = CljHelper.new 'com.github.kyleburton.clj-xpath'
puts "clj-xpath: "
puts clj_xpath._invoke("$x:text", "//foo", "<foo>bar</foo>")
clj_xpath._alias "x_txt", "$x:text"
puts clj_xpath.x_txt "//foo", "<foo>bar</foo>"



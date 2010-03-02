module CukeExample
  def base_url url
    (ENV['BASE_URL'] || "http://localhost") + url
  end
end

require File.dirname(__FILE__) + "/../cuke_env"

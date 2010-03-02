module NavigationHelpers
  def base_url(url)
    (ENV['BASE_URL'] || 'http://localhost:80') + url
  end
  def path_to(page_name)
    case page_name
    when /home/
      base_url '/'
    when /search/
      base_url '/search/index.cgi'
    else
      raise "Can't find mapping from #{page_name} to a url path.  # vi #{__FILE__}"
    end
  end
end

World(NavigationHelpers)

require File.expand_path(File.join(File.dirname(__FILE__), "..", "support", "paths"))

Given /^I am on the (.+) page$/ do |path|
  visit path_to(path)
end

When /^I choose the "([^"]+)" "([^\"]+)" search option$/ do |radio_button,search_box|
  locator = "xpath=//*[@name='#{search_box}']/../..//input[@value='#{radio_button}']"
  puts "...locator=#{locator}"
  selenium.click locator
end

When /^I type "([^\"]*)" into "([^\"]*)"$/ do |text,input_label|
  locator = "xpath=//*[@name='#{input_label}']"
  selenium.type locator, text
  # uncomment the 'ruby-debug' line to 'pause' things, this allows you use the
  # ruby debugger so you can try different thigns out on selenium or webrat,
  # then 'c' to continue -- keep in mind that you'll probably end up with
  # timeouts when you drop to the debugger, but it can be invaluable to be able
  # to try things out:
  #require 'ruby-debug'; debugger; 1;
end

When /^I click "([^\"]*)"$/ do |name|
  click_button(name)
end

Then /^"([^\"]+)" should be visible?$/ do |text|
  locator = %Q|xpath=//*[(contains(text(), "#{text}") or contains(@value, "#{text}"))]|
  selenium.wait_for_element(locator, :timeout_in_seconds => 5)
  #require 'ruby-debug'; debugger; 1;
  selenium.is_element_present(locator).should be_true
end


When /^I type "([^\"]*)" into the search box$/ do |text|
  locator = %Q|xpath=//*[contains(text(),'SEARCH')]/../..//input[@type='text']|
  selenium.type locator, text
  #require 'ruby-debug'; debugger; 1;
end

When /^I click the Go button$/ do
  locator = %Q|xpath=//input[@src='/images/top_nav/search_go2.gif']|
  selenium.click locator, :timeout_in_seconds => 10
end


require File.expand_path(File.join(File.dirname(__FILE__), "..", "support", "paths"))

Given /^I am on the (.+) page$/ do |path|
  visit path_to(path)
end

Given /^I choose the "([^"]+)" "([^\"]+)" search option$/ do |radio_button,search_box|
  locator = "xpath=//*[@name='#{search_box}']/../..//input[@value='#{radio_button}']"
  puts "...locator=#{locator}"
  selenium.click locator
end

Given /^I type "([^\"]*)" into "([^\"]*)"$/ do |text,input_label|
  locator = "xpath=//*[@name='#{input_label}']"
  selenium.type locator, text
  require 'ruby-debug'; debugger; 1;
end

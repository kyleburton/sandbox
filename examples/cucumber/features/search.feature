Feature: Search for Products
  In order to find a product
  As a user
  I want to be able to use the search feature to locate my product

  Scenario: Search using Exact Phrase
    Given I am on the search page
    And I choose the "exact" "Keywords" search option
    And I type "timed pregnant mice" into "Keywords"


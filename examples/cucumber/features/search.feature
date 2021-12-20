Feature: Search for Products
  In order to find a product
  As a user
  I want to be able to use the search feature to locate my product

  Scenario: Search using Exact Phrase
    Given I am on the search page
    When I choose the "exact" "Keywords" search option
    And I type "timed pregnant mice" into "Keywords"
    And I click "search"
    Then "Countdown Controller" should be visible

  @dev
  Scenario: search from the homepage
    Given I am on the home page
    When I type "timed pregnant mice" into the search box
    And I click the Go button
    Then "Life Science Magazine" should be visible

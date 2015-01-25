Getting Started:

  go get
  go build
  ./jira search 'assignee=--my-name-- and sprint in openSprints ()' 2>&1 | less



What do we want to be able to do:

  * list issue statuses (what's in the ENUM?)
  * list issues in current sprint
  * show an issue by ID
    * view some of the fields: id, assignee, secondary assignee, comments, etc
  * search issues based on 'any' text (eg: description)
  * optionally pull the full JSON document

Nices to have:

  * counts of issues by type in the current sprint?

Across all searches / lists:

  * ability to filter by one or more statuses (eg: In Progress, Done Done, etc)
  * ability to filter by one or more assignees


Updates:
  * change status
  * set asignee and secondary asignee (is this related to set-pair?)
  * add a comment


Create an iusse:


Examples:

    go build && ./jira search 'assignee=kburton and sprint in openSprints ()' 2>&1 | less

References

* https://developer.atlassian.com/display/JIRADEV/JIRA+REST+API+Example+-+Edit+issues

{:toc}

# Overview

This is a log of some of the ideas I want to learn more about or explore.

## Rand-a-minder

Web site/service that allows you to configure random reminders.  Random both over a time interval, and random from a set of messages.

## Building an Ensemble Fuzzy String Matching Algorithim with Genetic Programming

Needs a better tagline, but the idea is an extension of thoughts I had at Barcamp Philly in Nov 2009.

1. Create a training set of data
  1. Person Names
  2. Address Data
  3. Organization Names
2. The chromosome is a chaining and weighting of the matchers
  1. a matcher returns true/false
  2. the chromosme returns true/false

Vector of

## Examples

* Remind Me to Buy Flowers for my wife
  * Interval: M-F, 9am-5pm
  * Frequency: once every other week
  * Duration: forever
  * Message: Pick up flowers on the way home.  Lillys are her favorite.

* Reminde Me to call my Brother/Sister
  * Interval: Weekends, 10am-8pm
  * Frequency: Every Weekend
  * Duration: forever
  * Message: Your sibling would like to hear from you!

* Remind Me to buy a present for my cousin
  * Interval: October through November 7th (bday on 11th)
  * Frequency: About twice a week
  * Duration: till I reply 'done' or cancel
  * Message: Little Susy/Johnny would love to get a gift for their birthday.

Web based, allow reminding via IM (aim, gtalk), allow reminding via email, allow reminding via twitter (DM only).

# Ideas for Talks

## jQuery and JavaScript

* Introduction
* Techniques
** 'countdown latch' approach for lightboxes / dialogs and for coordinating on multiple ajax requests completing (or erroring)

## Matching

This is probably multiple talks.

### Define the problem

Where does this problem manifest itself?  Hospital admissions systems.  Patients may be incoherent, unconscious, not have ID, not have arrived with someone with id, not speak your language.  Yet, the hospital wants to know if they've seen you before, you know, in case the patient may be allergic to certin medicines (and of course to know if you have insurance).  Companies buy other companies, and then all of a sudden need to merge their customer databases.  Large companies have multiple subsidaries and need to see a unified view of the customers across all the subsidaries.  You may have access to multiple regional, and overlapping, data sets and want to create a unified national or global consolidated data set.

I worked for a company for 8 years that faced that last problem.  We were building a national health care provider master file out of thousands of different data sources.  When I left, we were handling over three thousand different data models and a few billion records, consolidating them all into a file of about five million health care practitioners.

As a user with two databases, that I _know_ contain records on the same real world people, I want to match them together.

Just like a database join - only your RDBMS doens't allow the join, it fails you in that you need to do a fuzzy join.

#### Matching of 2 Data Sets

Ideally you would match all the records in A to all the records in B, unfortunately that cross product for large data sets is way to large.

Even cloud computing won't save you. [picture of an angry or sad cloud?]

So you need to come up with a candidate selection strategy.

_need some sample data sets_

### ETL Challenges

### Data Model Normlaization Challenges


### Sliding Window

Create a proxy key.

### dynamic sliding windows
###
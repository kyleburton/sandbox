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



h1. Schedule Service / Timer Service for AMQP

This would be the 'call me back' type service that you've talked about before.

It is an AMQP listener.  AMQP is chosen for the following features:

* open standard, clients for several technologies
* asynchronous, tolerant of temporary system issues, facilitates retrying
* buffers both requests and messages
* best chace for a 'drop in' to existing architectures - I think that an MQ allows for finer grained, incremental adoption and less radical changes in existing applications and designs.

h2. Use Cases

h3. Generalized Retry

The initial use case seen for this service was for delaying activities within a group of services.  An example is for transactions that need to be retried.  Many designs either completely ignore retry semantics, for example by raising an exception in the face of a rolled back database transaction, or re-implement retry semantics within each service or application area.  As the trend of factoring monlithic applications and systems towards service oriented designs continues, this provides the opportunity to implement a service which implements retry semantics across other services.

This will not be an appropriate for all services, eg: for thos which need to return a binary status of either success or failure.  For services which an asynchronous (call back) response is tolerable, this may be an appropriate solution.

For a REST service, this scenario might play out in the following manner:

A service encoutners an error procesing a request.  Perhaps this is due to another service (such as a datbase) being unavailable, or other error for which another attempt is appropraite.  Catching the rollback, or otherwise detecting the retryable condition, the service bundles up a request to be re-invoked again in sixty seconds.  It posts this request to the scheduler service queue, and then returns a pending status back to its caller instead of immediate failure.  Approximately 60s later, the scheduler service performs the RPC (REST) against the service.  If the RPC call succeeds, the scheduler service deletes its copy of the job, completing its obligation for the job.  If the RPC call is detected to fail, the service uses error handling configuration to determine how to proceed.

h3. Deisgn Considerations

h4. Job Failure and Scheduler Retry Semantics

Consideration must be taken into account for the possible retrying of requests or transactions that have already been executed.  If a service processes a retry and it appears to be in error to the scheuler service, the request may be retried by the service (dependent on job configuration).

The job error configuration should support a fixed count of retry attempts, deferring to a failure configuration when the retry attempts have been exhausted.

h4. Adapters

HTTP, AMQP message forwarding, EJB, CORBA, execution of shell commands, other types of calls.

h4. Scheduler Failure

h5. Persistent Storage of Jobs

h3. Periodic Job Execution (CRON)

This type of service should be useable as a replacement for CRON, it will already contain many of the necessary components (quartz scheduler, registry of jobs) for a scheduler.

* use quartz timing specificaiton
* wrap an AMQP msg body, send it to the service, it'll send back when the timer fires
* support repeated pinging
* support REST callback?
* Failure for REST / RPC style callbacks should be to reschedule
* Suppot reactive / functional - eg: something like a heartbeat, if something hasn't checked in in a while, ping it?

h1. Visualizations

DD/MM/YYYY vs MM/DD/YYYY in sorting, make a graph?

<pre>
    x
    x
    x
    x
    x
    x
    x
    x
  x x
  x x
  x x
x x x
</pre>

Vs:

<pre>
  x
  x
  x
  x
  x
  x
  x
  x
  x x
  x x
  x x
x x x
</pre>

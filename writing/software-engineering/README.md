Posts I've had in mind & would like to write

## Kyle's Hierarchy of Success

[Slide Deck](https://docs.google.com/presentation/d/1VdAWJKMi7QmsLSJfcTdEJEHFcEgJs6cjFfBfqjkzYBQ/edit#slide=id.g310c4173436_0_273) - can I flesh this out into a post or talk to offer to a conference?

## The Stages of Knowledge Acquisition

The four stages of knowledge acquisition and how I see this in the Jr/Mid/Sr engineers I interview and have worked with: What you don't know you don't know | What you know you don't know | What you know you know | What you don't know you know.  How the relative ratio of these changes over your career, ways you can use this model to grow, how you can use this to assess (and often help) other engineers.

Related:
* [Did I Ever Show you my IDE?](https://docs.google.com/presentation/d/1axt7WDq-tkbDRsm0hEDwjjiddlvmnlpGcoznzAe_8o4/edit#slide=id.p), [Nick Mcavoy's Your IDE Sucks](https://docs.google.com/presentation/d/1iQ710sKdjW2mCNTE2JB_k5N0jSHf8E79Lk552HdfjUk/edit#slide=id.p)
* [Four Stages of Competence](https://en.wikipedia.org/wiki/Four_stages_of_competence)
* [Shu Ha Ri](https://en.wikipedia.org/wiki/Four_stages_of_competence)
* [Dreyfus model of skill acquisition](https://en.wikipedia.org/wiki/Dreyfus_model_of_skill_acquisition)
* [You don't know what you don't know](https://www.fatrank.com/you-dont-know-what-you-dont-know/)

## Problem Solving and my "Domains"

When building software I try to think about the work that I am doing to these domains, which often use different tools and produce different outputs (artifacts).  While not linear, the flow is often "to the right" as we construct and operate our systems.  As we move rightward, we lose flexibility and constraints increase.  Concepts also become more fixed, concrete and realized.  Misunderstandings and errors become more costly.  As we move rightward, we often learn more about the leftward domains, leading to valuable insights and iteration.

I will acknowledge that I may have read about these ideas somewhere, though I do not recall reading about it or where.

### The Problem Domain

We define the problem being solved (and not how to solve it, only what the problem is).  Typical tooling is documents, one-pagers, requirements, diagrams, there are few constraints here, but we are deciding what we are going to build and what it must be capable of doing.

### The Solution Domain

The Solution domain is where we define what solutions to "the problem" look like, we define the criteria by which alternatives and proposals will be evaluated.  This is often where we begin introducing constraints.  This is where we begin to decide the "how" (designs), we choose major components (eg: RDBMS vs NoSQL, vertical vs horizontal scaling).  Typical tooling is diagrams, design proposals, proof of concepts, architectural plans, deployment and roll out planning.

### Implementation Domain

The implementation domain is where we begin converting the abstract into the concrete, we build and code and construct.  Typical tools include programming languages, coding tools, IDEs, unit testing frameworks. Here we introduce more constraints: we are forced to pick data types, integer types, string lengths, encodings and storage types and formats.

### Execution Domain

Build/execution/deployment/verification - our tooling here is our build tooling and automation, linting, testing, CI / CD, this is our automated deployments, canaries, blue/green deployment approaches, and rollback logic.  This is where we introduce constraints like: global deployment topology (i.e. "game shards/pods", or data locality decisions); this is where we've made decisions about our production infrastructure.

We give up more flexibility, to gain certainty.

### Operational Domain

The Operational Domain is where our systems are "alive", where they run, handle request, service users and customers. Our tools here are even more constrained both in what we can do and in what is available to us, if we were prescient, we have configurable run-time parameters, we have telemetry, monitoring (of infra, of service, of business logic).  We have hopefully built in diagnostic endpoints to provide additional visibility that allow us to observe live system state or behaviors, or leverage things like distributed tracing.



## Lessons in Leadership from Popular Culture and Media

As I have worked at being a supportive and inspiring leader, I have identified a few examples of leadership in popular culture that I feel are great representations of different aspects of leadership:

* ["Being the Ricardos"](https://en.wikipedia.org/wiki/Being_the_Ricardos)
* ["Gordon Ramsay's Kitchen Nightmares](https://en.wikipedia.org/wiki/Kitchen_Nightmares) - the British version, NOT the American TV version
* [Ted Lasso](https://en.wikipedia.org/wiki/Kitchen_Nightmares)

For Kitchen Nightmares: the show feels like a text book example of a strong and effective leader, entering into a team under stress, quickly analyzing their current state, identifying core problems, breaking them (the problems and the people) down, addressing an achievable set of issues, showing the team they can thrive and succeed together (building them back up) and having them demonstrate their ability to newly work as a team.

For Ted Lasso, while it is a comedy, I still feel there are many leadership principles demonstrated throughout the show.  Early on Ted invests in relationships, he listens to the team and fixes issues that are low hanging fruit (water pressure in the showers) to show the team he is there to change the status quo.


## "reject the status quo, unless we can re-derive it from first-principles"

One of [my aphorisms](https://gist.github.com/kyleburton/8362332) is "reject the status quo, unless we can re-derive it from first-principles".  Reading:

https://benadam.me/thoughts/my-experience-at-amazon/

it said this: it's a lot easier to partially solve the problem than to actually think through things from first principles)

Why did these feel related to me?

We forego first principles out of expediency.  over time we develop heuristics for the first principles that have worked and we start further down the chain of logical reasoning.  Our openness to revisiting earlier links in the change is part of "demanding strong models", it's related to quips like "strong convictions loosely held" and being willing to take in new evidence that may invalidate or force us to change our first principles.  Our ability to do this quickly (reassert, confirm or invalidate those principles, axioms) is a proxy for intelligence, is a measure of productivity and greatly aids in triage and analysis of complex problems under pressure.

Wow, this article is full of good stuff...

"The larger point here is this: companies’ critical needs will always trump your career ambitions and often there is a large delta between the job you were hired to do, and the job you actually do."


## How to listen to competing ideas

This is skill, it's something that I think I've seen engineers have difficulty learning - folks too often approach this from the perspective of it being a competition between "my" vs "their" idea.  It is a clash in a sense, though the challenge is to fundamentally understand the problem you are both attempting to solve - where those understandings differ, creating a shared understanding and then assessing the ideas presented (solutions, designs, implementations).

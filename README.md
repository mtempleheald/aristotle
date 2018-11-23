# Aristotle's Number Puzzle

During investigation of functional programming I encountered Haskell for the first time.  
I learn best by doing, so I selected to solve Aristotle's number puzzle as it was lying around at the time (unsolved).  
I realised that this was a pure mathematical problem, so Haskell seemed an ideal fit for the job.  

The solution is quite simple mathematically, just a set of simultaneous equations, this was obvious from the start.  
I see no point repeating effort so for details see [someone else's blog](http://hwiechers.blogspot.com/2013/03/solving-artitotles-number-puzzle.html) for a good explanation.

Before starting on an actual solution I needed to understand Haskell in general.  
I watched plenty of videos, read plenty of documents online, browsed [Hackage](https://hackage.haskell.org/) and many of the usual suspects as suggested [here](https://wiki.haskell.org/Learning_Haskell).  
I didn't implement all that much, not usually one for following instructions but I did a few investigations (no idea what state they are currently in):
* parallel_maths - when playing with parallelism it made sense to use the classic fibonacci functional solution
* using_monads - shouldn't need much for this pure problem, I realised that but still worth knowing about, possibly missing something
* todo - practical example with monads
* qe - data structures I was working with at the time, wondered how best to represent records etc in Haskell  

I've left wip.hs here so that I can go back over my thought processes at the time, this is already 2 years on and I'm struggling, but in a sense this is more important than the actual solution.  I do remember a few key attempts/ discoveries/ investigations:  

* Lists versus tuple data structures and syntax, ultimately settling on list of tuples containing tuples
* Relying on lazy loading so not having to worry about the 121,645,100,408,832,000 possible permutations
* Defining pure functions which can be tested with a property-based testing approach (QuickCheck)
* Higher order functions; map, filter...
* Pattern matching - I love this, especially on the left hand side of the statement, this was new to me
* List comprehensions, this was when it became obvious what the solution would look like
* Parallelism, when the approach using list comprehensions failed to return anything in over a day I figured [parallelism](https://wiki.haskell.org/Parallelism) might help (that was some rabbithole!)  I was happy with annotations and commandline parameters, I played around with par/pseq but got nowhere.
* Eureka!  Functional programming as an approach is described as telling the computer what to do, not how to do it.  However there are limitations!  
  I come from a database background, specifically Oracle, so I'm used to query rewrite and optimisation, I kind of figured based on the statement above that the Haskell compiler would do something similar.  
  When I reordered my set of simultaneous list comprehensions, it worked, in under 5 seconds (I haven't bothered measuring it!)  
  
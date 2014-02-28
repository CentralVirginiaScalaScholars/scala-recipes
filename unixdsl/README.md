UNIX DSL
========

Example implementation of an internal and external domain specific
language in Scala.  UNIX text processing is itself a domain specific
language perfectly suited for quick and dirty data wrangling.  Our
DSL reproduces a tiny bit of the compositional power of UNIX pipes
as a means of demonstrating Scala features that facilitate DSL 
construction.  The result is an internal DSL of a DSL (UNIX) hosted 
in Scala and an external DSL of a DSL implemented using a Scala 
DSL (parser combinators).  Did I mention this is about DSLs? 

The domain model is not perfect.  In particular, the split between
GEN and SPROC is unnecessary and could be improved.  Unifying the
two concepts is left as a challenge to CVSS members.

The data comes from http://data.gdeltproject.org/events/index.html

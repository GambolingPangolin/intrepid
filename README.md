# Intrepid

Intrepid is a library for building user interfaces, inspired by
[concur][concur].  Every user interface has to produce a stream of interface
descriptions which update based on events.  In our model we provide two primitives:

* `match`, which binds a user interface description to an event filter and
  expresses the concept of an interface element related to an event which yields
  a value when that event occurs.
* `combine`, which merges two sub-interfaces by explicitly combining their
  interface descriptions and racing their event handlers.

[concur]: https://github.com/ajnsit/concur

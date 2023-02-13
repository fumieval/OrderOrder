OrderOrder
----

OrderOrder is an application that prevents jumbling of module prefices.
This is based on a hypothesis that module prefices should ideally form a directed acyclic graph given a notion of
dependencies between module prefices; that is, a module prefix depends on another module prefix if one of the children depends on any other child of the other module prefix.

* If Foo.X imports Bar.Y
* None of Bar.P imports Foo.Q

If this were violated, there would be no real distinction between Foo and Bar.

Usage
----

Pass one or more source directories to OrderOrder:

```sh
orderorder /path/to/src
```

It enumerates a [feedback arc set](https://en.wikipedia.org/wiki/Feedback_arc_set) with respect to module prefices. The feedback arc set can be thought of as exception rules with respect to dependencies.

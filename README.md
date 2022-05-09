OrderOrder
----

OrderOrder is a GHC plugin that prevents jumbling of module prefices, namely:

* If Foo.X imports Bar.Y
* None of Bar.P imports Foo.Q

If this were violated, there would be no real distinction between Foo and Bar.

Usage
----

Add this option to your cabal file:

```
  ghc-options: -fplugin=OrderOrder -fplugin-opt=OrderOrder:export:summary
```

It dumps the current module organisation grouped by module prefices as well as a Dot representation for graphviz.

```yaml
'':
- Config
- Domain
- Env
- Lib
```

If there is one or more cyclic relationships among module prefices, it enumerates a [feedback arc set](https://en.wikipedia.org/wiki/Feedback_arc_set) in `summary.trims.txt`.
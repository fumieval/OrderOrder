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
  ghc-options: -fplugin=OrderOrder
```

It dumps the current module organisation to `import-summary.yaml` which looks like

```yaml
API:
- Config
- Domain
- Env
- Lib
Config:
- Lib.Types
Domain:
- Lib
Env:
- Config
- Lib
Lib:
- Lib
```

Subsequent compilations validate module dependencies using the generated file.

```
src/Lib/Violation.hs:3:1: error:
    Lib.Violation imports Domain.Foo
  |
3 | import Domain.Foo
```
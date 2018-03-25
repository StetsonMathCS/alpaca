# ALPACA: BUILDING DYNAMIC CYBER RANGES WITH PROCEDURALLY-GENERATED VULNERABILITY LATTICES

![Logo](logo.png)

## Running Alpaca

### Prolog

Run main.pl to find/generate lattices and create ansible files

```
swipl main.pl shortestPath '[server_access_root]' '[]'
```

Possible predicate names include:

+ shortestPath
+ longestPath
+ fullPath
+ allPossiblePaths (will not create ansible files; no arguments required, i.e. `swipl main.pl allPossiblePaths`)

### Vagrant

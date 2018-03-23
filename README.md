# ALPACA: BUILDING DYNAMIC CYBER RANGES WITH PROCEDURALLY-GENERATED VULNERABILITY LATTICES

![Logo](logo.png)

## Issues
Current result:
```prolog
checkConfigs([ssh-[users-(exists, root)]], [ssh-[users-(exists, user)]], Result).
false.
```

Expected:
```prolog
checkConfigs([ssh-[users-(exists, root)]], [ssh-[users-(exists, user)]], Result).
Result=[ssh-[users-(exists, [root, user])]]
```

When using the same Role and Key with the ***exists*** predicate in both configurations, we expect the values to merge under the same key. 

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
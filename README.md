# ALPACA: BUILDING DYNAMIC CYBER RANGES WITH PROCEDURALLY-GENERATED VULNERABILITY LATTICES

![Logo](logo.png)

## Running Alpaca

### Prolog

Run main.pl to find/generate lattices and create ansible files

<pre>
swipl main.pl <i>predicate</i> <i>args</i>
</pre>

#### Examples:

Generate full lattice showing all possible paths

<pre>
swipl main.pl allPossiblePaths
</pre>

Find all possible paths for scenario, group paths by compatible configurations, and separate into differnt directories. 

Directories include:
+ Lattice
+ Variable file (contains configurations)
+ playbook.yml

<pre>
swipl main.pl createAllPaths <i>'[Goal]'</i> <i>'[InitialState]'</i> <i>'Name'</i>
</pre>
<pre>
swipl main.pl createAllPaths '[server_access_root]' '[]' 'server_access_root'
</pre>

Select which scenario you want to create and instantiate the cyber range.

<pre>
swipl main.pl createRange <i>'Directory_Name'</i>
</pre>
<pre>
swipl main.pl createRange 'server_access_root1'
</pre>

Heyley Gatewood

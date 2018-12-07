# Documentation for ALPACA features
Specifically for the features written by Brandon.

## MySQL Integration
ALPACA now is able to utilize a MySQL database in place of vulnDatabase.pl. Simply call
```
initialSetup(DB_USERNAME, DB_PASSWORD)
```
when starting ALPACA. ALPACA will then collect all the data in the database and assert it
into the Prolog knowledge base. You can then just simply use ALPACA.

## Permutations
I incorporated a new permutations feature into ALPACA. This feature only currently
creates random passwords and usernames, however it is easy to implement new permutations
features into ALPACA.

### Where does ALPACA 'permute'?
Permutations are done in listVals(). When ALPACA is writing data to the Ansible files,
it checks to see if the specific parameter for an Ansible config is a Prolog predicate.
If it is, it runs it, and puts the output of that predicate into the Ansible config.

### Can I pass arguments into a permutation predicate?
Yes! This is accomplished via Params, which is an argument required by every predicate
that generates a VM. The format for Params is as follows:

```
[paramaterName1-parameter1, paramaeterName2-parameter2, ...]
```

For example, the generatePasswordOfLength() predicate accepts an argument, paramPasswordLength, for password length.
If you wanted your random passwords to be 15 characters in length, you would pass:
```
[paramPasswordLength-15]
```

into a VM generation predicate.

### How do I add permutation predicates?
Easily! Just create a new predicate in alpaca.pl. The predicate must be of the following format:
```
newPermuationPredicate(Params, Output)
```

When adding this predicate to ALPACA's database, simply input it as newPermutationPredicate().

## Lattice generation predicates
ALPACA now incorporates some more lattice generation procedures.

### Generating a lattice w/ specific complexity
A lattice can now be generated w/ a specific complexity, where complexity is defined
in the original ALPACA paper. Simply call:
```
createLatticeWithComplexityIGS(Goal, InitialState, Lower, Upper, Name, Params)
```
Where Lower, Upper are the Lower and Upper bounds for complexity.

### Generating a lattice w/ a specific vulnerability
A lattice can also now be generated w/ a specific vulnerability. Simply call:
```
createLatticeWithVulnIGS(Goal, InitialState, Name, Vuln, Params)
```
where Vuln is the name of the vulnerability.

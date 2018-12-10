# Documentation for ALPACA features

These features were created/updated by Heyley Gatewood during CSCI 321.

<i> 
We use the terms cliques and subnetworks interchangeably.
We also use the terms nodes and machines (as in virtual machines) interchangeably.

You can have a maximum of 8 machines in your network.
</i>


## Running ALPACA predicates

### Prolog

Run cliques.pl to find/generate the network cliques for your network of virtual machines. 
<pre>
swipl cliques.pl 
</pre>

#### Inside of the prolog script, you can:

1.) Find/Generate a list of all cliques given a graph using:
<pre>
all_cliques(Graph, Cliques, AllCliques).
</pre>

Each new section of a graph holds the format:
<pre>
[g(_,[ ])]
</pre>
where the value in the x-coordinate position of the pair within the graph is the current node in question and the value in the y-coordinate position of the pair within the graph is a list containing the nodes that the current node is connected to.

Example:
<pre>
all_cliques([g(a,[b,c]),g(b,[a,c,d]),g(c,[a,b]),g(d,[b,e]),g(e,[d])], Cliques, AllCliques).
</pre>

In the graph in the example above:
* Node <i>a</i> is connected to nodes <i>b</i> and <i>c</i>;
* Node <i>b</i> is connected to nodes <i>a</i>, <i>c</i>, and <i>d</i>; 
* and so on...

If you were to create an visual undirected graph from this information, the graph would look like:

      A----B
      |   /|
      |  / |
      | /  |
      |/   |
      C    D
           |
           |
           |
           |
           E

The output <i>AllCliques</i> is a list contains all possible cliques size 1 through size 8 implied by the given graph.

2.) Determine the list of all unique machines in your network given a graph.
<pre>
getMachines([g(M,_)|Tail], Ms, Machines).
</pre>

Example:
<pre>
getMachines([g(a,[b,c]),g(b,[a,c,d]),g(c,[a,b]),g(d,[b,e]),g(e,[d])], [], Machines).
</pre>

This will output a the list of machines: a,b,c,d,e.

3.) Determine whether a clique is a subset of another, and remove cliques that are subsets of other cliques.
<pre>
removeSubsets([List|Rest], UniqueNonSubsets, Result).
</pre>

Example:
<pre>
removeSubsets(AllCliques, [], Result).
</pre>

In this example, we use the list of all possible cliques <i> AllCliques </i>, which was created from running the <i> all_cliques() </i> function, as input. <i> Result </i> is a list of all unique cliques that exist in <i> AllCliques </i>, which further represents the number of subnetworks that will exist in the network that are creating.

4.) Assign each machine in your network its own IP address such that the machines in each clique are members of the same private subnet and thus share the same network card/adapter when instantiate in VirtualBox.
<pre>
assignAddrToNodesInClique(Machines, Cliques, MachineIps, Final).
</pre>

Example:
<pre>
assignAddrToNodesInClique([a,b,c], [[a,b],[b,c]], _, Final).
</pre>

In the example above, 
* <i>a</i> would be assigned the addresses: <i>192.168.75.75</i>, because it is only a member of noe subnet;
* <i>b</i> would be assigned the addresses: <i>192.168.75.76</i> and <i>192.168.76.75</i>, because it is a member of two separate subnets;
* and so on...

5.) Create/Edit the anisble inventory files in according to yous network design.
<pre>
writeInventoryFile(List).
</pre>

Example:
<pre>
writeInventoryFile([[a,b,c], [d,e], [b,d]]).
</pre>

6.) Create/Edit a Vagrantfile from a list of the machines and their addresses. (This list can be created by using the <i>assignAddrToNodesInClique()</i> function.)
<pre>
editVagrantfile(List).
</pre>

Example:
<pre>
editVagrantfile([[a, ["192.168.75.75"]], [b, ["192.168.75.76", "192.168.77.75"]], [c, ["192.168.75.77"]]]).
</pre>
where <i>a</i>, <i>b</i>, and <i>c</i> are machines followed by a list of the addresses that belong to them.

##### All-in-One Function
###### 7.) Run all of the above functions and more for an easy and quick way to accomplish all the tasks that need to be accomplished to create a network of Virtual Machines in VitualBox is:
<pre>
runAll(Graph, Cliques, AllCliques).
</pre>

This function takes in a given graph, and:
* determines all possible cliques
* determines all unique machines in the network
* deterimine the unique cliques by eliminating any clique that is a subset of another
* assigns each machine in the unique cliques one or more IP addresses depending on how many cliques each machine is a member of 
* re-writes the Vagrant and Ansible files needed to create the network of Virtual Machines

Example:
<pre>
runAll([g(a,[b,c]),g(b,[a,c,d]),g(c,[a,b]),g(d,[b,e]),g(e,[d])], Cliques, AllCliques).
</pre>

##### <i> After running runAll(Graph, Cliques, AllCliques), the Ansible and Vagrant files needed to create your network of Virtual Machines are ready. </i>

8.) Create the range, including playbook files, for each VM.
<pre>
createRangeFromIGS(Goal, InitialState, DirectoryName).
</pre>

Example:
<pre>
createRangeFromIGS([server_access_root], [], 'server_access_root').
</pre>

9.) Create/Start one of the VM's in your network.
<pre>
createStartRangeFromIGS(VMname).
</pre>

Example:
<pre>
createStartRangeFromIGS(a).
</pre>

#### Important Note:
#### As of now, one playbook files is specified for all VM's. This means that every VM, when started, will have the same inital states. That is also a reason why you have to create/start each VM in your network individaully.
#### There is not yet a function implemented that can allow you to adjust/customize the vulnerabilites per machine in your network.



:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- [alpaca].
/*
 * Maximum clique
 *   A graph is represented by a list, each item of which is of the form
 *   g(V,Ns), where V is a vertex, and Ns is a list of its neighbours.
 *   A clique of a graph is a set of vertices such that every pair of vertices
 *   is joined by an edge.
 *   A maximum clique of a graph is a clique for which the number of vertices
 *   is maximum.
 */

/* all_max_cliques(Graph, Cliques) is true if Cliques is a list of all the   */
/*   maximum cliques of the Graph.                                           */
/* e.g. all_max_cliques([g(1,[2,5]),g(2,[1,3,4]),g(3,[2,4]),g(4,[2,3,5]),    */
/*                       g(5,[1,4])], [[2,3,4]]).                            */



/* runAll([g(a,[b,c]),g(b,[a,c,d]),g(c,[a,b]),g(d,[b,e]),g(e,[d])], Cliques, AllCliques) */

runAll(Graph, Cliques, AllCliques):-
    all_cliques(Graph, Cliques, AllCliques),
    getMachines(Graph, [], Machines),
    removeSubsets(AllCliques, [], Result),
    assignAddrToNodesInClique(Machines, Result, _, Final),
    writeInventoryFile(Result),
    editVagrantfile(Final),
    endVagrantfile.

/* getMachines([g(a,[b,c]),g(b,[a,c,d]),g(c,[a,b]),g(d,[b,e]),g(e,[d])], [], R) */

getMachines([], Machines, Machines).
getMachines([g(M,_)|Tail], Ms, Machines):-
    append([M], Ms, List),
    getMachines(Tail, List, Machines).

isSubsetAny([],[]).
isSubsetAny(List, [H|_]):-
    subset(List, H), !.
isSubsetAny(List, [_|T]):-
    isSubsetAny(List, T).

removeSubsets([], Result, Result).
removeSubsets([List|Rest], UniqueNonSubsets, Result):-
    append(Rest, UniqueNonSubsets, AllLists),
    isSubsetAny(List, AllLists), !,
    removeSubsets(Rest, UniqueNonSubsets, Result).
removeSubsets([List|Rest], UniqueNonSubsets, Result):-
    removeSubsets(Rest, [List|UniqueNonSubsets], Result).

indexOf([Elem|_], Elem, 1).
indexOf([_|Tail], Elem, Index):-
    indexOf(Tail, Elem, Index1),
    Index #= Index1 + 1.

/* [[a,b,c], [d,e], [b,d]] */
/* [a,b,c,d,e] */
/* assignAddrToNodesInClique([a,b,c,d,e], [[a,b,c], [d,e], [b,d]], MachineIps, Final). */

assignAddrToNodesInClique(Machines, Cliques, MachineIps, Final):-
    assignAddrToNodesInClique(Machines, Machines, Cliques, Cliques, MachineIps),
    findall([Machine, Group], group_by(Machine, Ip, member((Machine, Ip), MachineIps), Group), Final), !.
assignAddrToNodesInClique(_,_,_,[],[]):- !.
assignAddrToNodesInClique(AllMachines, [Machine|Rest1], AllCliques, [Clique|Rest2], RestIps):-
    not(member(Machine, Clique)), !,
    assignAddrToNodesInClique(AllMachines, Rest1, AllCliques, [Clique|Rest2], RestIps).
assignAddrToNodesInClique(AllMachines, [Machine|Rest1], AllCliques, [Clique|Rest2], [(Machine, Ip)|RestIps]):-
    indexOf(AllCliques, Clique, CliqueNum), CliqueNum1 is CliqueNum + 74,
    indexOf(Clique, Machine, MachineNum), MachineNum1 is MachineNum + 74,
    number_string(MachineNum1, X),
    number_string(CliqueNum1, Y),
    string_concat("192.168.", Y, Step1), string_concat(Step1, ".", Step2), string_concat(Step2, X, Ip),
    assignAddrToNodesInClique(AllMachines, Rest1, AllCliques, [Clique|Rest2], RestIps).
assignAddrToNodesInClique(AllMachines, [], AllCliques, [_|Rest2], RestIps):-
    assignAddrToNodesInClique(AllMachines, AllMachines, AllCliques, Rest2, RestIps).

/* writeInventoryFile([[a,b,c], [d,e], [b,d]]). */
writeInventoryFile(List):-
    open('ansible/inventories/dev', write, Stream),
    write(Stream, ''),
    close(Stream),
    length(List, N),
    foreach(between(1,N,X), writeInventoryFile(List, X)).
writeInventoryFile(List, X):-
    indexOf(List, Group, X),
    length(Group, M),
    number_string(X, Num),
    append('ansible/inventories/dev'),
    format(atom(String), "~s~s~s", ["[net", Num,"]"]),
    nl, write(String), nl,
    told,
    foreach(between(1,M,Z), addMachinesToInventoryFile(Group, Z)).
addMachinesToInventoryFile(Group, Z):-
    indexOf(Group, Machine, Z),
    append('ansible/inventories/dev'),
    write(Machine), nl,
    told.


editVagrantfile(List):-
    instantiateVagrantfile,
    length(List, N),
    foreach(between(1,N,X), editVagrantfile(List, X)).
editVagrantfile(List, X):-
    indexOf(List, Net, X),
    writeVagrantfile(Net).



instantiateVagrantfile :-
    open('Vagrantfile', write, Stream),
    write(Stream, 'Vagrant.configure("2") do |config|'),
    close(Stream).

writeVagrantfile(List) :-
    indexOf(List, Name, 1),
    write(Name), nl,
    indexOf(List, IpList, 2),
    append('Vagrantfile'),
    nl,
    tab(4), write('config.vm.define "'), write(Name), write('" do |'), write(Name), write('|'), nl,
    tab(8), write(Name), write('.vm.box = "ubuntu/trusty64"'), nl,
    tab(8), write(Name), write('.vm.hostname = "sr"'), nl,
    told,
    addPrivateNetwork(IpList, Name),
    append('Vagrantfile'),
    tab(8), write(Name), write('.vm.provision "ansible" do |ansible|'), nl,
    tab(16), write('ansible.playbook = "ansible/playbook.yml"'), nl,
    tab(16), write('ansible.inventory_path = "ansible/inventories/dev"'), nl,
    tab(16), write('ansible.limit = "all"'), nl,
    tab(8), write('end'), nl,
    tab(4), write('end'), nl,
    told.

endVagrantfile :-
    append('Vagrantfile'),
    write('end'),
    told.

addPrivateNetwork(List, Name):-
    length(List, N),
    foreach(between(1,N,X), makeLine(List, Name, X)).

makeLine(List, Name, X):-
    indexOf(List, Ip, X),
    append('Vagrantfile'),
    tab(8), write(Name), write('.vm.network :private_network, ip: "'), write(Ip), write('"'), nl,
    told.


all_cliques(Graph, Cliques, AllCliques):-
	findall(Clique, clique(2, Graph, Clique), Cliques),
	findall(Clique, clique(3, Graph, Clique), Cliques1),
	append(Cliques, Cliques1, Cliques2),
	findall(Clique, clique(4, Graph, Clique), Cliques3),
	append(Cliques2, Cliques3, Cliques4),
	findall(Clique, clique(5, Graph, Clique), Cliques5),
	append(Cliques4, Cliques5, Cliques6),
	findall(Clique, clique(6, Graph, Clique), Cliques7),
	append(Cliques6, Cliques7, Cliques8),
	findall(Clique, clique(7, Graph, Clique), Cliques9),
	append(Cliques8, Cliques9, Cliques10),
	findall(Clique, clique(8, Graph, Clique), Cliques11),
	append(Cliques10, Cliques11, Cliques12),
    findall(Clique, clique(1, Graph, Clique), Cliques13),
    append(Cliques12, Cliques13, Cliques14),
	AllCliques = Cliques14.


all_max_cliques(Graph, Cliques):-
  max_clique(Graph, OneClique),
  length(OneClique, Length),
  findall(Clique, clique(Length, Graph, Clique), Cliques).

/* max_clique(Graph, Clique) is true if Clique is a maximum clique of the    */
/*   Graph. Only one solution will be found.                                 */
/* e.g. max_clique([g(1,[2,5]),g(2,[1,3,4]),g(3,[2,4]),g(4,[2,3,5]),         */
/*                  g(5,[1,4])], [2,3,4]).                                   */
max_clique(Graph, Clique):-
  max_clique_1(Graph, [], Clique).

/* max_clique_1(Graph, Clique0, Clique) is true if Clique is a maximum       */
/*   clique of the Graph, and Clique contains more vertices than Clique0.    */

max_clique_1([], Clique, Clique):-!.
max_clique_1(Graph, Clique0, Clique):-
  max_clique_2(Graph, Clique1),
  is_shorter(Clique0, Clique1), !,
  Graph=[_|RestOfGraph],
  max_clique_1(RestOfGraph, Clique1, Clique).

max_clique_1([_|Graph], Clique0, Clique):-
  max_clique_1(Graph, Clique0, Clique).
  
/* max_clique_2(Graph, Clique) is true if Clique is a maximum clique         */
/*   starting at the first vertex of the Graph.                              */
max_clique_2([], []).
max_clique_2([g(Vertex,Neighbours)|Graph], [Vertex|Clique]):-
  sub_graph(Graph, Neighbours, SubGraph),
  max_clique_1(SubGraph, [], Clique).

/* sub_graph(Xs, Ys, Zs) is true if Zs is the list of those elements g(V,Ns) */
/*   of Xs for which V is also an element of the list Ys.                    */
sub_graph([], _, []).
sub_graph([g(V,_)|Xs], Ys, Zs):-
  \+ member(V, Ys), !, sub_graph(Xs, Ys, Zs).
sub_graph([X|Xs], Ys, [X|Zs]):-
  sub_graph(Xs, Ys, Zs).

/* max_clique_p(Graph, Clique) is true if Clique is a maximum clique of the  */
/*   Graph. Only one solution will be found. This version implements pruning */
/*   and can be much faster than max_clique/2.                               */
/* e.g. max_clique_p([g(1,[2,5]),g(2,[1,3,4]),g(3,[2,4]),g(4,[2,3,5]),       */
/*                    g(5,[1,4])], [2,3,4]).                                 */
max_clique_p(Graph, Clique):-
  max_clique_p_1(Graph, 0, c(0,[]), c(_,Clique)).

/* max_clique_p_1(Graph, 0, c(N0,[]), c(N,Clique)) is true if Clique is a    */
/*   maximum clique, with N (greater than N0) vertices, of the Graph.        */

max_clique_p_1([], _, Clique, Clique):-!.
max_clique_p_1(Graph, M, c(N0,_), Clique):-
  max_clique_p_2(Graph, N0, M, c(N1,Clique1)),
  N0 < N1, !,
  Graph=[_|RestOfGraph],
  max_clique_p_1(RestOfGraph, M, c(N1,Clique1), Clique).
max_clique_p_1([_|Graph], M, Clique0, Clique):-
  max_clique_p_1(Graph, M, Clique0, Clique).
  
/* max_clique_p_2(Graph, Max, N0, c(N,Clique)) is true if the order of the   */
/*   Graph is greater than Max minus N0, and Clique is a maximum clique,     */
/*   with N vertices, starting at the first vertex of the Graph.             */
max_clique_p_2([], _, N, c(N,[])).
max_clique_p_2([g(Vertex,Neighbours)|Graph], Max, N0, c(N,[Vertex|Clique])):-
  length(Graph, L), 
  L + 1 > Max - N0,
  sub_graph(Graph, Neighbours, SubGraph),
  N1 is N0 + 1,
  max_clique_p_1(SubGraph, N1, c(N1,[]), c(N,Clique)).

/* clique(N, Graph, Clique) is true if Clique is a clique with N vertices of */
/*   the Graph. On backtracking, all solutions will be found.                */
/* e.g. clique(3, [g(1,[2,5]),g(2,[1,3,4]),g(3,[2,4]),g(4,[2,3,5]),          */
/*                 g(5,[1,4])], [2,3,4]).                                    */
clique(0, _, []):-!.
clique(N, Graph, [Vertex|Clique]):-
  N > 0,
  length(Graph, L), 
  L >= N,
  rest(g(Vertex,Neighbours), Graph, Graph1),
  sub_graph(Graph1, Neighbours, Graph2),
  N1 is N - 1,
  clique(N1, Graph2, Clique).

/* rest(X, Ys, Zs) is true if X is a member of the list Ys, and the list Zs  */
/*   is the rest of the list following X. On backtracking, all solutions     */
/*   will be found.                                                          */
rest(X, [X|Ys], Ys).
rest(X, [_|Ys], Zs):-rest(X, Ys, Zs).

/* is_shorter(Xs, Ys) is true if the list Xs contains fewer elements than    */
/*   the list Ys.                                                            */
is_shorter([], [_|_]).
is_shorter([_|Xs], [_|Ys]):-is_shorter(Xs, Ys).

/* length(Xs, L) is true if L is the number of elements in the list Xs.      */
%length(Xs, L):-length_1(Xs, 0, L).

/* length_1(Xs, L0, L) is true if L is equal to L0 plus the number of        */
/*   elements in the list Xs.                                                */
%length_1([], L, L).
%length_1([_|Xs], L0, L):-L1 is L0 + 1, length_1(Xs, L1, L).

/* member(X, Xs) is true if the element X is contained in the list Xs.       */
%member(X, [X|_]).
%member(X, [_|Xs]):-member(X, Xs).

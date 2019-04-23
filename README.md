# Alpaca: Building Dynamic Cyber Ranges with Procedurally-Generated Vulnerability Lattices

<img align="right" src="logo.png">

## Publications

- J. Eckroth, K. Chen, H. Gatewood, B. Belna. "Alpaca: Building Dynamic Cyber Ranges with Procedurally-Generated Vulnerability Lattices," Proceedings of the Annual ACM Southeast Conference, 2019. [PDF](publications/acmse-2019-alpaca.pdf)

## Requirements

- [SWI-Prolog](http://www.swi-prolog.org/)
- [Packer](https://packer.io/)
- [Ansible](https://www.ansible.com/)
- [Graphviz](https://www.graphviz.org/)

## Running Alpaca

### Step 0 (optional): Visualize the vulnerabilities

Generate an image of the vulnerabilities defined in the system:

<pre>
$ swipl prolog/main.pl graphAllVulns vulns.dot
$ open vulns.dot.png
</pre>

### Step 1: Generate range configuration files

In order to build a range, one must first find/generate lattices and create Packer and Ansible files. The first `[...]` argument is the starting state, the second argument is the goal state, and the third is any required parameters.

<pre>
$ swipl prolog/main.pl createRangeFromIGS '[]' '[root_shell]' '[paramPasswordLength-5]'
</pre>

Or,

<pre>
$ swipl prolog/main.pl createRangeFromIGS '[db_access]' '[root_shell]' '[paramPasswordLength-5]'
</pre>

The system will generate a subfolder and set of files in the `ranges/` folder. The generated range will have a unique ID that is reported by the system. A ZIP file will contain all the range configuration files.

Information about the range and its lattices are found in the `range_metadata.json` file and the lattice subfolders.

### Step 2: Generate a virtual machine for a lattice in the range

Switch to a specific lattice in a range:

<pre>
$ cd ranges/64374c93-697f-46eb-9f3f-58cf6c48e676/e38d2277-6f1d-4b22-a9aa-c93781da1c39/
</pre>

Then run the Packer script:

<pre>
$ bash run_packer.sh
</pre>


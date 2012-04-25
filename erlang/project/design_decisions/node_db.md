Node database
=============

**Note:** *Suspended: new nodes will just be spawned from a single erlang node at
startup*

Our system is based on nodes, which correspond to the ATMs.
Nodes can crash, nodes can join.

Multiple nodes can run on a single erlang node, and can crash individually.

To allow new nodes to join the system, they must be able to establish a
connection with at least a node present in the system.

It is assumed that the user nows at least one alive erlang node.

...

Beware: nodes are ATMs, not erlang nodes. Multiple nodes (ATMs) can run on a single
erlang node.

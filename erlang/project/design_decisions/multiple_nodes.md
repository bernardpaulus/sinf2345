We should have first a script, or whatever that starts up the erlang nodes and the ATMs
at the right place

Note about node discovery
=========================

Nodes are not aware of other nodes at startup. 

However, when a node sends a message to another, it receives, as side effect,
the names of the other nodes the target node know of.

Those list, accessible by

    nodes().

is also updated when the target node is aware of a new node.

This is at least what happen on a local computer :)

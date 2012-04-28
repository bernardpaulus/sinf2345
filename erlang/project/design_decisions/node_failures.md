Node failure simulation
=======================

How to simulate node (ATM) failure?

Erlang provides

    exit(Pid, kill)

to kill an arbitrary process. To be sure all processes belonging to a single
node are killed along with the targeted process, we can link them together.

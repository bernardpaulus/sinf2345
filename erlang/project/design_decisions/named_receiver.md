Inter Node communication
========================

Different possibilities exists:

* socket-based implementation
* rpc:call() function
* {P_Name, Node} ! Msg

if we use a socket-based implementation, then there is no interest in doing this
in erlang.

If we use rpc:call(), we lose the concept of exchanging messages

So, we will use the send construct since erlang offers it :D

Inter Node Communication constraints
====================================

To send a message to another erlang node, we must use the registered name of the
receiver process.

    {P_Name, Node} ! Msg

The low-level process should thus at least have a name known by every other
low-level process.

Despite the fact that this would be convenient, this name cannot be constant: we wish, for debugging purposes, to be
able to run the full distributed program on a single erlan node.

*Each ATM will thus have a name.*
The low level process will thus be a registered with the name

    list_to_atom(lists:concat([ATM_Name, "_low"))



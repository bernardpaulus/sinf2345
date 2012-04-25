Inter Node communication
========================

Different possibilities exists:

* socket-based implementation
* rpc:call() function
* {P_Name, Node} ! Msg
* Pid ! Msg

if we use a socket-based implementation, then there is no interest in doing this
in erlang.

If we use rpc:call(), we lose the concept of exchanging messages

So, we will use the send construct since Erlang offers it :D

The first idea, due to a lack of understanding of Erlang, was to use

    {P_Name, Node} ! Msg

However, it is not necessary: pid() are renamed so that a pid() sent over a
link is still valid on the other side.

Since it greatly simplifies the code, pid() will be used instead of {Name,
Node}. The node hosting the pid() can simply be found by

    node(Pid).


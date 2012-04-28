Format of the messages
======================

The messages must be unique, as required in the Introduction to Reliable and
Secure Distributed Programming.

Additionally, it is a good practice to include the sender of the message,
according to jaerlang.

Plus, a message must find it's way through all the communication protocol
layers.

A good thing would be that each instance of each layer got the illusion it's
talking with the other just through the lower-level-one.

Combining this, any lower-level should be able to process requests of form

    {send, From, To, Seq_Num, Msg}

And deliver the same tuple to the process on the other end of the channel.
[this is valid for P2P links...]

Should include the name of the node (! beware: node is a node of the system, as
opposed to erlang nodes!) on which the target process is. 



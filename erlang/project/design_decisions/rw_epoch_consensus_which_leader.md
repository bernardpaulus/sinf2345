Which leader for read-write epoch consensus ?
=============================================

since we don't have a global name for group of processes, the most simple way to
discover it is to simply wait for a {propose }. Indeed, when a {propose } is
sent you know who is the leader.

The leader then broadcasts {start} messages and wait for acks, sending and
waiting in an exponential backoff fashion.

Once the leader gets at least half acknowledgments, it first waits the same amount of
time of the last broadcast + ack round, to allow slower nodes to still get
started. Only then it begins the core algorithm.

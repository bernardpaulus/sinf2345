About
=====

This is the project of the course SINF2345

Project statement
=================

Introduction
------------

The DistCity bank is ashamed of it's current centralized banking system. You
have been chosen to implement the new, completely distributed, system.

In DistCity, the rules are slightly different than they are on Earth. A
transaction as always one origin account, one destination account and an amount
to transfer. There is no execution date or comment (you can add comment if you
want). It is very bad style to have a negative balance on your account, and to
punish this, the bank takes one percent of the value of the transaction from any
account found to be negative after the transaction. This special transaction is
of course not subject to the same rule.

As making the system distributed is a very big change, you have been granted a
flag day for the installation of the new system, so you won't have to
inter-operate with the old centralized system.

The VPN being very good, you don't have to care about security, it's all handled
outside of your application.


Mission
-------

You have to build a distributed application of moderate scale (around a few tens
of nodes) that will be connected in a full mesh and replicate the status of all
bank accounts. People need to know when the transaction they submitted is
complete. They also need to know what's on their account. Of course, the system
being concurrent, the amount shown should have been valid only at some point
between the time they made the request and the time they received the answer.
DistCity is small enough that every node can handle the full stream of
transactions but big enough that they can't transfer the full state of all
accounts all the time.

The bank doesn't want to buy expensive machines so you'll have to deal with them
crashing sometimes. At least they are deep enough under the ground and the
programmers (yourself included!) are so good that the only possible crashes are
fatal (but silent) crash of the machine and network failures (which are not
distinguishable from machine crashes) no fail-restart, no byzantine failures,
etc. New machines will be added to the network at the same rate that they crash
so that the number of live nodes remain almost constant. Failure rates
(including network) are not so high that we can't make complex distributed
algorithms to recover from them but we can't rule out a few concurrent failures.

Hints
-----

The rule about negative accounts is important. What effect does it have on the
ordering of transactions ?

The rule about being able to see the balance of the account is important too.
What simple solution is prevented by that rule ?

Make a set of live nodes and keep it up-to-date. A new node can ask an existing
(bootstrap) node to be added and everyone can ask for the removal of a
(potentially) failed node. If you have been (wrongly) removed, restart yourself
under a new identity.

Timeline
--------

March 31: statement is put online.

Passover vacation: Nothing too difficult. Think of what algorithms to use ; make
groups of two students. Study for the exams and make other projects clin d’oeil

Next two weeks: Design & implementation (& testing & debugging...)

April 27 at 20h00 UTC: project due

Some time later... : interview

Rules
-----

The project is to be done by group of two students. If necessary groups made of
a single student will be tolerated (but not recommended). You are to submit the
source code and usage instructions together with a design document describing
the links between the statement, the types of protocols used (broadcast,
register, consensus, etc.) and the properties they have to guarantee to justify
your algorithmic choices. All non source documents will be either simple text or
in pdf format, in French or English. Practical submission details will be added
later.

You can make the program in any language that you want. A small sample framework
in Oz will be proposed but is in no way mandatory to use. You have to understand
how the system you use is working.

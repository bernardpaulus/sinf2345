Subscribe with or without sequence number?
==========================================

There are currently two versions:

    {subscribe, Pid}

(link.erl)
and

    {subscribe, Pid, Seq_Num}

(post_office.erl)

I would prefer {subscribe, Pid}, since it's shorter, Pid is unique in the node, 
and it is never transmitted over links that suffer message duplication.

However, since we agreed that messages should include a sequence number, I'll go
with this, for the moment.

**PROPOSAL: remove the Seq_Num for all the suscribe messages**

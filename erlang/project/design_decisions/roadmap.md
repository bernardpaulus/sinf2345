Roadmap
=======

We need Total Order Broadcast. We must only take algorithms that assume a
Fail-Noisy (or, worst, Fail-Silent) model, since our system ispartially
asynchronous, and because of this, it doesn't possess the
Perfect Detector.

                            (Module) Total order broadcast
                                           ^
                                           |
                      (algo) Consensus-based Total order Broadcast
                                           ^                                  
                                           |                                  
      (Module) Reliable Broacast ------------+--------- (Module) Consensus      
                  ^                                              ^              
                  |                                              |                
    (algo) Eager Reliable Broadcast                             ???             
                  ^                                            ?   ?            
                  |                                               ?             
     (Module) Best-Effort Broadcast                              ?              
                                                                 ?              
                                                                                
                                                                 ?              
                                                                         
                                                                     
                                                                     
We must read the book, at least (Uniform Consensus in the Fail-Noisy Model,
p.284) because we have no idea what are the algorithms needed to do the
consensus.

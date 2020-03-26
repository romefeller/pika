A small (untyped) pi-calculus interpreter.

x(y).p => Waits on channel x, bind a value to y
and proceed as p;

x<y>.p => Sends a value y on channel x and proceeds as p;

(v x). p => Create a channel x and proceeds as p.

par ( p | q ) => Executes p and q in parallel;

!n. p => Replicate p n times;

0 => An inert process.

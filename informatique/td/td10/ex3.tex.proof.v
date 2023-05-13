Variables s q p r : Prop.

Theorem test1 : (~((p)\/(q))) -> ((~(p))/\(~(q))).
Proof.
intros H1.
split.
intro A.
cut ((p\/q)).
assumption.
left.
assumption.
intro A.
cut ((p\/q)).
right.
assumption.
assumption.
Qed.



Theorem test2 : ((~(p))/\(~(q))) -> (~((p)\/(q))).
Proof.
intros H1.
intro A.
cut (p\/q). intro b. destruct b as [U|V].
cut (p).
assumption.
destruct H1 as [M N].
assumption.
cut (q).
assumption.
destruct H1 as [M N].
assumption.
assumption.
Qed.




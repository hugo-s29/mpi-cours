Variables p : Prop.

Theorem test1 : ((p)->(p)).
Proof.
intro A.
assumption.
Qed.



Theorem test2 : (p) -> (~(p)) -> False.
Proof.
intros H1 H2.
cut (p).
assumption.
assumption.
Qed.



Variables q : Prop.

Theorem test3 : (p) -> (q) -> ((p)/\(q)).
Proof.
intros H1 H2.
split.
assumption.
assumption.
Qed.



Theorem test4 : ((p)/\(q)) -> ((q)/\(p)).
Proof.
intros H1.
split.
destruct H1 as [A B].
assumption.
destruct H1 as [A B].
assumption.
Qed.



Theorem test5 : ((p)\/(q)) -> ((q)\/(p)).
Proof.
intros H1.
cut (p\/q). intro i. destruct i as [A|B].
right.
assumption.
left.
assumption.
assumption.
Qed.



Theorem test6 : (~((p)/\(~(p)))).
Proof.
intro A.
cut (p).
destruct A as [M N].
assumption.
destruct A as [M N].
assumption.
Qed.




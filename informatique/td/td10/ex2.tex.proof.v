Variables q p : Prop.

Theorem test1 : ((p)\/((p)/\(q))) -> (p).
Proof.
intros H1.
cut (p\/(p/\q)). intro R. destruct R as [A|B].
assumption.
destruct B as [U V].
assumption.
assumption.
Qed.



Variables s r : Prop.

Theorem test2 : ((p)/\(q)) -> ((r)/\(s)) -> ((p)/\(r)).
Proof.
intros H1 H2.
split.
destruct H1 as [U V].
assumption.
destruct H2 as [U V].
assumption.
Qed.



Theorem test3 : (p) -> ((q)/\(r)) -> ((p)/\(q)).
Proof.
intros H1 H2.
split.
assumption.
destruct H2 as [U V].
assumption.
Qed.



Theorem test4 : (p) -> (~(~(p))).
Proof.
intros H1.
intro .
cut (p).
assumption.
assumption.
Qed.



Theorem test5 : (~(~(~(p)))) -> (~(p)).
Proof.
intros H1.
intro .
cut (~~p).
intro .
cut (p).
assumption.
assumption.
assumption.
Qed.




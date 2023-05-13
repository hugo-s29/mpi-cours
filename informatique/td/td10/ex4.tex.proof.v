Variables s q p r : Prop.

Theorem test1 : ((p)/\((q)\/(r))) -> (((p)/\(q))\/((p)/\(r))).
Proof.
intros H1.
cut (q\/r). intro u. destruct u as [A|B].
left.
split.
destruct H1 as [U V].
assumption.
assumption.
right.
split.
destruct H1 as [U V].
assumption.
assumption.
destruct H1 as [U V].
assumption.
Qed.



Variables \varphi : Prop.

Theorem test2 : (((p)/\(q))\/((p)/\(r))) -> ((p)/\((q)\/(r))).
Proof.
intros H1.
split.
cut ((p/\q)\/(p/\r)). intro X. destruct X as [A|B].
destruct A as [M N].
assumption.
destruct B as [M N].
assumption.
assumption.
cut ((p/\q)\/(p/\r)). intro j. destruct j as [A|B].
left.
destruct A as [U V].
assumption.
right.
destruct B as [U V].
assumption.
assumption.
Qed.




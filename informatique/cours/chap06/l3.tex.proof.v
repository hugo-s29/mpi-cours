Variables Q X P p R : Prop.

Theorem test1 : ((p)->(~(~(p)))).
Proof.
intro A.
intro B.
cut (p 123).
assumption.
assumption.
Qed.



Variables q \Gamma r : Prop.

Theorem test2 : ((((p)->(q))/\((q)->(r)))->((p)->(r))).
Proof.
intro A.
intro B.
cut (q 123).
destruct A as [U V].
assumption.
cut (p).
destruct A as [U V].
assumption.
assumption.
Qed.



Theorem test3 : (((p)->(q))->((~(q))->(~(p)))).
Proof.
intro .
intro .
intro .
cut (q).
assumption.
cut (p).
assumption.
assumption.
Qed.




Variables X P R Q : Prop.

Theorem test1 : (P) -> (Q) -> (R) -> (((P)/\(X))\/(((P)\/(Q))/\((Q)\/(~(R))))).
Proof.
intros H1 H2 H3.
right.
split.
left.
assumption.
left.
assumption.
Qed.




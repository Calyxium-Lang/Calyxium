val optimize : Ir.instr list -> Ir.instr list
(** [optimize instrs] performs IR-level optimizations over the given list of
    instructions and returns an optimized instruction list.

    The exact optimizations may include constant folding, dead-code elimination,
    expression simplification, and other IR transformations depending on the
    current optimizer implementation.

    @param instrs The list of IR instructions to optimize.
    @return A new list of IR instructions with optimizations applied. *)

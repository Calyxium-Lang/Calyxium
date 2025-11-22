val typecheck_program : Calyxium_ir.Ir.instr list -> bool
(** [typecheck_program instrs] performs type-checking on a full IR program.

    The function walks all instructions, collects function declarations, checks
    expressions and statements, and reports whether the program is well-typed.

    @param instrs The list of IR instructions representing the program.
    @return [true] if the program passes type-checking, otherwise [false]. *)

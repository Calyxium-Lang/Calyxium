open Calyxium_parser

let enum_variants = Hashtbl.create 10
let stdlib_used = ref false

let built_in_modules =
  [
    ( "Math",
      [
        ("pi", Ast.Type.SymbolType { value = "float" });
        ("e", Ast.Type.SymbolType { value = "float" });
        ("tau", Ast.Type.SymbolType { value = "float" });
        ("nan", Ast.Type.SymbolType { value = "float" });
        ("inf", Ast.Type.SymbolType { value = "float" });
        ("neg_inf", Ast.Type.SymbolType { value = "float" });
        ( "sin",
          Ast.Type.FunctionType
            ( [ Ast.Type.SymbolType { value = "float" } ],
              Ast.Type.SymbolType { value = "float" } ) );
      ] );
  ]

let builtins =
  let mk_builtin name overloads =
    let schemes =
      List.map
        (fun (params, ret) ->
          let func_type = Ast.Type.FunctionType (params, ret) in
          Unify.generalize [] func_type)
        overloads
    in
    (name, schemes)
  in
  [
    mk_builtin "print"
      [ ([ Ast.Type.Any ], Ast.Type.SymbolType { value = "unit" }) ];
    mk_builtin "input"
      [
        ( [ Ast.Type.SymbolType { value = "string" } ],
          Ast.Type.SymbolType { value = "unit" } );
      ];
    mk_builtin "to_bytes"
      [
        ( [ Ast.Type.SymbolType { value = "string" } ],
          Ast.Type.ArrayType
            { element_type = Ast.Type.SymbolType { value = "byte" } } );
      ];
    mk_builtin "to_float"
      [
        ( [ Ast.Type.SymbolType { value = "*" } ],
          Ast.Type.SymbolType { value = "float" } );
        ( [ Ast.Type.SymbolType { value = "string" } ],
          Ast.Type.SymbolType { value = "float" } );
        ( [ Ast.Type.SymbolType { value = "int" } ],
          Ast.Type.SymbolType { value = "float" } );
      ];
    mk_builtin "to_int"
      [
        ( [ Ast.Type.SymbolType { value = "*" } ],
          Ast.Type.SymbolType { value = "int" } );
        ( [ Ast.Type.SymbolType { value = "byte" } ],
          Ast.Type.SymbolType { value = "int" } );
        ( [ Ast.Type.SymbolType { value = "string" } ],
          Ast.Type.SymbolType { value = "int" } );
        ( [ Ast.Type.SymbolType { value = "float" } ],
          Ast.Type.SymbolType { value = "int" } );
      ];
    mk_builtin "length"
      [ ([ Ast.Type.Any ], Ast.Type.SymbolType { value = "int" }) ];
    mk_builtin "to_string"
      [ ([ Ast.Type.Any ], Ast.Type.SymbolType { value = "string" }) ];
    mk_builtin "assert"
      [
        ( [ Ast.Type.SymbolType { value = "bool" } ],
          Ast.Type.SymbolType { value = "unit" } );
      ];
    mk_builtin "panic"
      [
        ( [ Ast.Type.SymbolType { value = "string" } ],
          Ast.Type.SymbolType { value = "unit" } );
      ];
    mk_builtin "to_byte"
      [
        ( [
            Ast.Type.ArrayType
              { element_type = Ast.Type.SymbolType { value = "int" } };
          ],
          Ast.Type.SymbolType { value = "byte" } );
        ( [ Ast.Type.SymbolType { value = "int" } ],
          Ast.Type.SymbolType { value = "byte" } );
        ( [ Ast.Type.SymbolType { value = "string" } ],
          Ast.Type.SymbolType { value = "byte" } );
      ];
    mk_builtin "of_type"
      [ ([ Ast.Type.Any ], Ast.Type.SymbolType { value = "string" }) ];
    mk_builtin "head"
      [ ([ Ast.Type.ArrayType { element_type = Ast.Type.Any } ], Ast.Type.Any) ];
    mk_builtin "tail"
      [ ([ Ast.Type.ArrayType { element_type = Ast.Type.Any } ], Ast.Type.Any) ];
    mk_builtin "reverse"
      [ ([ Ast.Type.ArrayType { element_type = Ast.Type.Any } ], Ast.Type.Any) ];
    mk_builtin "fst"
      [
        ( [
            Ast.Type.TupleType
              ([ Ast.Type.VarType 0 ], Some (Ast.Type.VarType 1));
          ],
          Ast.Type.VarType 0 );
      ];
    mk_builtin "snd"
      [
        ( [
            Ast.Type.TupleType
              ([ Ast.Type.VarType 0 ], Some (Ast.Type.VarType 1));
          ],
          Ast.Type.VarType 1 );
      ];
  ]

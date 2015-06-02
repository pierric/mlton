signature ELABORATE_PROGRAMS_STRUCTS = sig
  structure Ast: AST
  structure Core: CORE_LANG
  structure TyAtom: TYPE_ATOM
  structure Env: ELABORATE_ENV
  sharing Ast = Env.Ast
  sharing Ast.Tyvar = Core.Tyvar = Env.Tyvar
  sharing Core.Con = Env.Con
  sharing Core.Var = Env.Var
  sharing TyAtom = Core.TyAtom = Env.TyAtom
end

signature ELABORATE_PROGRAM = sig
    include ELABORATE_PROGRAMS_STRUCTS
    val elaborateProgram: Ast.Program.t * Env.t -> Core.Dec.t vector
end

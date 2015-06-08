signature ELABORATE_PROGRAMS_STRUCTS = sig
  structure Ast     : AST
  structure Core    : CORE_LANG
  structure Decs    : DECS
  structure Env     : ELABORATE_ENV
  structure ElabCore: ELABORATE_CORE
  sharing Ast = Env.Ast = ElabCore.Ast
  sharing Ast.Tyvar = Env.Tyvar
  sharing Core.TyAtom = Env.TyAtom = ElabCore.TyAtom = Decs.TyAtom
  sharing Env = ElabCore.Env
  sharing Decs = ElabCore.Decs
  sharing Core.Dec = ElabCore.Core.Dec
  sharing type Decs.dec = Core.Dec.t
end

signature ELABORATE_PROGRAM = sig
    include ELABORATE_PROGRAMS_STRUCTS
    val elaborateProgram: Ast.Program.t * Env.t -> Decs.t * Env.t
end

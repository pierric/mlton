signature FLAT_PARSER_STRUCTS =
sig
  structure Ast   : AST
  structure Env   : ELABORATE_ENV
  sharing Ast       = Env.Ast
  sharing Ast.Tyvar = Env.Tyvar
end

signature FLAT_PARSER =
sig
  include FLAT_PARSER_STRUCTS
  val parseExps       : Env.t * Ast.Exp.t vector -> Ast.Exp.t
  val parsePats       : Env.t * Ast.Pat.t vector -> Ast.Pat.t
  val parsePatsFunSig : Env.t * Ast.Pat.t vector -> Ast.Pat.t * Ast.Pat.t vector
end

signature ELABORATE_TYPE_STRUCTS = sig
  structure Ast : AST
  structure TyAtom : TYPE_ATOM
  structure Env : ELABORATE_ENV
  sharing Env.TyAtom = TyAtom
  sharing Env.Ast = Ast
end

signature ELABORATE_TYPE = sig
  include ELABORATE_TYPE_STRUCTS

  val elaborateT : Env.t * Ast.Type.t -> TyAtom.Type.t
end

signature ELABORATE_CORE_STRUCTS = sig
  structure Ast: AST
  structure CoreML: CORE_ML
  structure Env: ELABORATE_ENV
  structure TyAtom: TYPE_ATOM
  sharing Ast = Env.Ast
  sharing Ast.Tyvar = CoreML.Tyvar
  sharing TyAtom = Env.TyAtom
end

signature ELABORATE_CORE = sig
  include ELABORATE_CORE_STRUCTS

  (*
  val elaborateDec : Env.t * Ast.Dec.t -> CoreML.Dec.t * Env.t * TyAtom.Subst.t
  *)
  val elaboratePat : Env.t * Ast.Pat.t -> CoreML.Pat.t * Env.t
end

   

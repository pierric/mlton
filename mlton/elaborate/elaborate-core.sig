signature ELABORATE_CORE_STRUCTS = sig
  structure Ast   : AST
  structure Core  : CORE_LANG
  structure Env   : ELABORATE_ENV
  structure TyAtom: TYPE_ATOM
  structure ElabTy: ELABORATE_TYPE
  sharing Ast = Env.Ast
  sharing Ast.Tyvar = Core.Tyvar = Env.Tyvar
  sharing Core.Con = Env.Con
  sharing Core.Var = Env.Var
  sharing TyAtom = Core.TyAtom = Env.TyAtom 
  sharing ElabTy.Ast = Ast
  sharing ElabTy.Env = Env
  sharing ElabTy.TyAtom = TyAtom
end

signature ELABORATE_CORE = sig
  include ELABORATE_CORE_STRUCTS

  val elaborateDec : Env.t * Ast.Dec.t -> Core.Dec.t list * Env.t * TyAtom.Subst.t
end

   

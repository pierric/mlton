signature ELABORATE_CORE_STRUCTS = sig
  structure Ast   : AST
  structure Core  : CORE_LANG
  structure Env   : ELABORATE_ENV
  structure TyAtom: TYPE_ATOM
  structure Decs  : DECS
  structure ElabTy: ELABORATE_TYPE
  sharing Ast = Env.Ast
  sharing Ast.Tyvar = Env.Tyvar
  sharing TyAtom = Core.TyAtom = Env.TyAtom 
  sharing ElabTy.Ast = Ast
  sharing ElabTy.Env = Env
  sharing ElabTy.TyAtom = Decs.TyAtom = TyAtom
  sharing Core.CoreML.Atoms = Env.Atoms
  sharing type Decs.dec = Core.Dec.t
end

signature ELABORATE_CORE = sig
  include ELABORATE_CORE_STRUCTS

  val elaborateDec : Env.t * Ast.Dec.t -> Decs.t * Env.t * TyAtom.Subst.t
end

   

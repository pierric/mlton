signature ELABORATE_ENV_STRUCTS = sig
  structure Ast : AST
  structure TyAtom : TYPE_ATOM
end

signature ELABORATE_ENV = sig 
  include ELABORATE_ENV_STRUCTS

  type t

  val free : t -> TyAtom.Tyvar.t list

  val lookupTycon : t * Ast.Longtycon.t -> (TyAtom.TypFun.t * Ast.Longvid.t list) option
end

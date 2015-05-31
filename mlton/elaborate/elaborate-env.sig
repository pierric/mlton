signature ELABORATE_ENV_STRUCTS = sig
  include ATOMS
  structure Ast : AST
  structure TyAtom : TYPE_ATOM
end

signature ELABORATE_ENV = sig 
  include ELABORATE_ENV_STRUCTS

  structure TypDef : sig
    type t
    val arity  : t -> int
    val typfun : t -> TyAtom.TypFun.t
    val cons   : t -> Ast.Longvid.t list
  end

  structure ValDef : sig
    type t
    datatype v = VCON of Con.t | VVAR of Var.t
    val isCon : v -> bool
    val isVar : v -> bool
    val deCon : v -> Con.t
    val deVar : v -> Var.t
    val value : t -> v
    val scheme: t -> TyAtom.Scheme.t
    val make  : v * TyAtom.Scheme.t -> t 
  end

  type t

  val empty : t
  val free  : t -> TyAtom.VarSet.t
  val gen   : TyAtom.VarSet.t * t -> t
  val subst : TyAtom.Subst.t * t -> t

  val append : t * t -> t

  val lookupTycon : t * Ast.Longtycon.t -> TypDef.t option
  val lookupVid   : t * Ast.Longvid.t   -> ValDef.t option
  val lookupCon   : t * Ast.Longcon.t   -> ValDef.t option

  (* extend current Type  Environment *)
  val extendTycon : Ast.Tycon.t * TypDef.t -> t -> t
  (* extend current Value Environment *)
  val extendVid   : Ast.Vid.t   * ValDef.t -> t -> t
  (* extend rigid free variables *)
  val extendRgdFV : TyAtom.VarSet.t -> t -> t
end

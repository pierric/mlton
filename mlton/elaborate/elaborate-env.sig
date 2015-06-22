signature ELABORATE_ENV_STRUCTS = sig
  include ATOMS
  structure Ast : AST
  structure TyAtom : TYPE_ATOM
  sharing TyAtom.Atoms = Atoms
end

signature ELABORATE_ENV = sig
  include ELABORATE_ENV_STRUCTS

  structure Basis : sig
    type t
  end

  structure TypDef : sig
    type t
    val arity  : t -> TyAtom.TypFun.arity
    val typfun : t -> TyAtom.TypFun.t
    val cons   : t -> Ast.Longvid.t list
    val make   : { typfun: TyAtom.TypFun.t, cons  : Ast.Longvid.t list } -> t
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

  structure Fixity : sig
    datatype t = datatype Ast.Fixity.t
  end

  val empty : t
  val free  : t -> TyAtom.VarSet.t
  val gen   : TyAtom.VarSet.t * t -> t
  val subst : TyAtom.Subst.t * t -> t

  val append : t * t -> t

  val lookupTycon : t * Ast.Longtycon.t -> TypDef.t option
  val lookupVid   : t * Ast.Longvid.t   -> ValDef.t option
  val lookupCon   : t * Ast.Longcon.t   -> ValDef.t option
  val lookupFixity: t * Ast.Longvid.t   -> Fixity.t option

  (* extend current Type  Environment *)
  val extendTycon : Ast.Tycon.t * TypDef.t -> t -> t
  (* extend current Value Environment *)
  val extendVid   : Ast.Vid.t   * ValDef.t -> t -> t
  (* extend current Infix Op environment *)
  val extendFixity : Ast.Vid.t   * Fixity.t -> t -> t
  (* extend rigid free variables *)
  val extendRgdFV : TyAtom.VarSet.t -> t -> t

  val makeBasis : t * (t -> 'a) * ('a -> t) -> 'a * Basis.t
  val openBasis : t * Basis.t -> t

  val layoutCurrentScope : t -> Layout.t
end

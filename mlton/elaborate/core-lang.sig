signature CORE_LANG_STRUCTS = sig
  include ATOMS
  structure TyAtom : TYPE_ATOM
  sharing TyAtom.Tyvar = Tyvar
end

signature CORE_LANG = sig

  include CORE_LANG_STRUCTS

  structure Pat: sig
    datatype t =
       Var      of Var.t
     | Const    of Const.t
     | Con      of Con.t * t option
     | Layered  of Var.t * t
     | Wild

    val layout: t -> Layout.t

    val isWild: t -> bool
    val truee: t
    val falsee: t
  end

  structure Exp: sig
    type t
    type lambda
    type dec
    datatype node = App  of t * t
                  | Case of t * ((Pat.t * t) vector)
                  | Constructor of Con.t
                  | Constant    of Const.t
                  | Var         of Var.t
                  | Lambda      of lambda
                  | Let         of dec vector * t 
                  | Seq         of t * t
                  | CasePar     of t vector * ((Pat.t vector * t) vector)

    val make : node * TyAtom.Type.t -> t
    val ty   : t -> TyAtom.Type.t
    val node : t -> node

    val var  : Var.t   * TyAtom.Type.t -> t
    val con  : Con.t   * TyAtom.Type.t -> t
    val const: Const.t * TyAtom.Type.t -> t
    val seq  : t * t -> t
    val ifthen : t * t * t -> t
    val lambda : lambda -> t
    val app    : t * t -> t
    val casee  : t * (Pat.t * t) vector ->  t
    val casepar: t vector * (Pat.t vector * t) vector -> t

    val truee  : t
    val falsee : t

    val subst : TyAtom.Subst.t * t -> t
  end

  structure Lambda: sig
    type t

    val make : Var.t * TyAtom.Type.t * Exp.t -> t
    val ty : t -> TyAtom.Type.t * TyAtom.Type.t

    val subst : TyAtom.Subst.t * t -> t
  end

  structure Dec: sig
    type t

    val subst   : TyAtom.Subst.t * t -> t

    val valbind : { vars      : TyAtom.VarSet.t
                  , valbind   : (Pat.t * Exp.t) vector
                  , recvalbind: (Var.t * Lambda.t) vector
                  } -> t
    val funbind : { vars      : TyAtom.VarSet.t
                  , name      : Var.t
                  , body      : Lambda.t 
                  } vector -> t
  end

  sharing type Exp.lambda = Lambda.t
  sharing type Exp.dec    = Dec.t

end

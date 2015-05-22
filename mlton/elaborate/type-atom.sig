signature TYPE_ATOM = sig

  structure Tyvar : sig
    eqtype t
    val make    : unit -> t
    val layout  : t -> Layout.t
  end

  structure Tycon : sig
    eqtype t
    val layout  : t -> Layout.t
  end

  type tyvar = Tyvar.t
  type tycon = Tycon.t

  structure Type : sig
    datatype t = FlexTyvar of tyvar
               | RigdTyvar of tyvar
               | Cons of tycon * (t list)
    val layout: t -> Layout.t
    val free  : t -> tyvar list
  end

  type typ = Type.t

  structure Scheme : sig
    type t
    val make  : tyvar list * typ -> t
    val layout: t -> Layout.t
  end

  structure TypFun : sig
    type t
    val apply : t * typ list -> typ
  end

  structure Subst  : sig
    type t
    val empty   : t
    val make    : tyvar * typ -> t
    val merge   : t * t -> t
    val compose : t * t -> t
    val layout  : t -> Layout.t
  end

  val unify : typ * typ -> Subst.t
  val subst : Subst.t * typ -> typ
  val gen   : tyvar list * typ -> Scheme.t
  val inst  : Scheme.t -> typ

end

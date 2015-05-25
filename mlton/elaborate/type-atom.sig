signature TYPE_ATOM_STRUCTS = sig
  include ATOMS
end

signature TYPE_ATOM = sig
  include TYPE_ATOM_STRUCTS

  structure VarSet : sig
    type t
    val empty     : t
    val singleton : Tyvar.t -> t
    val append    : t * t -> t
    val subtract  : t * t -> t
    val disjoint  : t * t -> bool
    val unions    : t list -> t
    val layout    : t -> Layout.t
  end

  structure Type : sig
    datatype t = FlexTyvar of Tyvar.t
               | RigdTyvar of Tyvar.t
               | Cons of Tycon.t * (t list)
    val layout: t -> Layout.t
    val free  : t -> VarSet.t

    val unit      : t
    val newNoname : unit -> t
    val deArrow   : t -> (t * t) option
  end
  
  structure Subst  : sig
    type t
    val empty   : t
    val make    : Tyvar.t * Type.t -> t
    val merge   : t * t -> t
    val compose : t * t -> t
    val minus   : t * VarSet.t -> t
    val layout  : t -> Layout.t
  end

  structure Scheme : sig
    type t
    val make  : VarSet.t * Type.t -> t
    val fromType : Type.t -> t
    val free  : t -> VarSet.t
    val subst : Subst.t * t -> t
    val layout: t -> Layout.t
  end

  structure TypFun : sig
    type t
    val apply : t * Type.t list -> Type.t
  end

  val unify : Type.t * Type.t -> Subst.t
  val unifyL: Type.t list * Type.t list -> Subst.t
  val subst : Subst.t * Type.t -> Type.t
  val gen   : VarSet.t * Type.t -> Scheme.t
  val inst  : Scheme.t -> Type.t

end

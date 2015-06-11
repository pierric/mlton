signature TYPE_ATOM_STRUCTS = sig
  include ATOMS
end

signature TYPE_ATOM = sig
  include TYPE_ATOM_STRUCTS

  structure VarSet : sig
    type t
    val empty     : t
    val singleton : Tyvar.t -> t
    val fromL     : Tyvar.t list   -> t
    val fromV     : Tyvar.t vector -> t
    val toV       : t-> Tyvar.t vector
    val append    : t * t -> t
    val subtract  : t * t -> t
    val disjoint  : t * t -> bool
    val equals    : t * t -> bool
    val isEmpty   : t -> bool
    val unions    : t list -> t
    val layout    : t -> Layout.t
  end

  structure Type : sig
    datatype t = FlexTyvar of Tyvar.t
               | RigdTyvar of Tyvar.t
               | Cons of Tycon.t * (t vector)
    val equals: t * t -> bool
    val layout: t -> Layout.t
    val free  : t -> VarSet.t

    val bool      : t
    val intInf    : t
    val var       : Tyvar.t -> t
    val con       : Tycon.t * (t vector) -> t
    val arrow     : t * t -> t
    val newNoname : unit -> t
    val deArrow   : t -> (t * t) option

    val deArgs    : t -> (t list * t)
    val args      : t list * t -> t

    val deConOpt: t -> (Tycon.t * t vector) option
    val deRecord: t -> (Record.Field.t * t) vector
    val isCharX: t -> bool
    val isInt: t -> bool
    val makeHom: {con: Tycon.t * 'a vector -> 'a,
                  var: Tyvar.t -> 'a} -> {destroy: unit -> unit,
                                          hom: t -> 'a}
    val tuple: t vector -> t
    val unit: t

    (* simplify the primitive types during the translation from Core-ML to XML *)
    val synonym : t -> t
  end
  
  structure Subst  : sig
    type t
    val empty   : t
    val make    : Tyvar.t * Type.t -> t
    val merge   : t * t -> t
    val compose : t * t -> t
    val composeL: t list -> t
    val minus   : t * VarSet.t -> t
    val layout  : t -> Layout.t
  end

  structure Scheme : sig
    type t
    val make  : VarSet.t * Type.t -> t
    val fromType : Type.t -> t
    val free  : t -> VarSet.t
    val subst : Subst.t * t -> t
    val gen   : VarSet.t * t -> t
    val layout: t -> Layout.t
  end

  structure TypFun : sig
    type t
    datatype arity = Arity of int | NaryCon

    val apply : t * Type.t vector -> Type.t
    val arity : t -> arity

    val fromTycon : Tycon.t * TyconKind.t -> t
    val fromType  : Type.t  -> t
  end

  val unify : Type.t * Type.t -> Subst.t
  val unifyV: Type.t vector * Type.t vector -> Subst.t
  val unifyL: Type.t list   * Type.t list   -> Subst.t
  val unifyS: Type.t list -> Subst.t
  val subst : Subst.t * Type.t -> Type.t
  val gen   : VarSet.t * Type.t -> Scheme.t
  val inst  : Scheme.t -> Type.t * Type.t vector
end

signature CORE_LANG_STRUCTS = sig
  include ATOMS
  structure TyAtom : TYPE_ATOM
  structure CoreML : CORE_ML
  sharing CoreML.Tyvar = TyAtom.Tyvar = Tyvar
  sharing CoreML.Var   = Var
  sharing CoreML.Con   = Con
  sharing CoreML.Const = Const
  sharing CoreML.Type  = TyAtom.Type
end

signature CORE_LANG = sig

  include CORE_LANG_STRUCTS

  structure Pat: sig
    type t
    datatype node = 
       Var      of Var.t
     | Const    of Const.t
     | Con      of { con  : Con.t
                   , arg  : t option
                   , targs: TyAtom.Type.t vector  (* type args during inst *)
                   }
     | Layered  of Var.t * t
     | Wild

    val make : node * TyAtom.Type.t -> t
    val ty   : t -> TyAtom.Type.t
    val node : t -> node
    val layout: t -> Layout.t

    val subst : TyAtom.Subst.t * t -> t
    
    val isWild: t -> bool
    val truee: t
    val falsee: t

    val toCoreML : t -> CoreML.Pat.t
  end

  structure Exp: sig
    type t
    type lambda
    type dec
    type caserule    = { pat: Pat.t       , exp: t, lay: Layout.t option }
    type caseparrule = { pat: Pat.t vector, exp: t, lay: Layout.t option }
    datatype noMatch = datatype CoreML.Exp.noMatch
    datatype node = App         of t * t
                  | Case        of { info : { kind   : string
                                            , nest   : string list
                                            , lay    : unit -> Layout.t
                                            , region : Region.t
                                             }
                                   , abnm : noMatch * Control.Elaborate.DiagDI.t  *
                                                      Control.Elaborate.DiagEIW.t *
                                                      Control.Elaborate.DiagEIW.t
                                   , test : t
                                   , rules: caserule vector
                                   }
                  | Constructor of { con: Con.t, targs: TyAtom.Type.t vector}
                  | Constant    of Const.t
                  | Var         of { var: Var.t, targs: TyAtom.Type.t vector }
                  | Lambda      of lambda
                  | Let         of dec vector * t 
                  | Seq         of t vector
                  | CasePar     of { test: t vector, rules: caseparrule vector }

    val make : node * TyAtom.Type.t -> t
    val ty   : t -> TyAtom.Type.t
    val node : t -> node

    val var0 : Var.t * TyAtom.Type.t -> t
    val var  : {var: Var.t, targs: TyAtom.Type.t vector} * TyAtom.Type.t -> t
    val con  : {con: Con.t, targs: TyAtom.Type.t vector} * TyAtom.Type.t -> t
    val const: Const.t * TyAtom.Type.t -> t
    val seq  : t vector -> t
    val lete : dec vector * t -> t
    val ifthen : t * t * t -> t
    val lambda : lambda -> t
    val app    : t * t -> t
    val casee  : { test : t
                 , rules: caserule vector
                 } ->  t
    val casepar: { test : t vector
                 , rules: caseparrule vector
                 } -> t

    val truee  : t
    val falsee : t

    val subst : TyAtom.Subst.t * t -> t

    val toCoreML : t -> CoreML.Exp.t
  end

  structure Lambda: sig
    type t

    val make : Var.t * TyAtom.Type.t * Exp.t -> t
    val ty : t -> TyAtom.Type.t * TyAtom.Type.t

    val subst : TyAtom.Subst.t * t -> t

    val toCoreML : t -> CoreML.Lambda.t
  end

  structure Dec: sig
    type t

    val subst   : TyAtom.Subst.t * t -> t

    val valbind : { vars      : TyAtom.VarSet.t
                  , valbind   : (Pat.t * Region.t * Exp.t) vector
                  , recvalbind: (Var.t * Lambda.t) vector
                  } -> t
    val funbind : { vars      : TyAtom.VarSet.t
                  , decs      : (Var.t * Lambda.t) vector 
                  } -> t

    val toCoreML : t -> CoreML.Dec.t
  end

  sharing type Exp.lambda = Lambda.t
  sharing type Exp.dec    = Dec.t

end

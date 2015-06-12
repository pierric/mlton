(* 
  An adaptive Core language between AST and Core-ML, for we are dealing with a subset of Standard ML.
  And especially for now we don't have tuples and record. Core is easier to deal with in elaboration phase.
*)   

signature CORE_LANG_STRUCTS = sig
  structure TyAtom : TYPE_ATOM
  structure CoreML : CORE_ML
  sharing CoreML.Atoms = TyAtom.Atoms
  sharing CoreML.Type  = TyAtom.Type
end

signature CORE_LANG = sig

  include CORE_LANG_STRUCTS

  structure Pat: sig
    type t
    datatype node = 
       Var      of CoreML.Var.t
     | Const    of CoreML.Const.t
     | Con      of { con  : CoreML.Con.t          (* constructor *)
                   , arg  : t option              (* argument    *)
                   , targs: TyAtom.Type.t vector  (* type args during instantiation for polymorphic constructors *)
                   }
     | Layered  of CoreML.Var.t * t
     | Wild

    val make : node * TyAtom.Type.t -> t
    val ty   : t -> TyAtom.Type.t
    val node : t -> node

    val subst : TyAtom.Subst.t * t -> t
    
    val isWild: t -> bool
    val truee: t
    val falsee: t

    val layout: t -> Layout.t
    val toCoreML : t -> CoreML.Pat.t
  end

  structure Exp: sig
    type t
    type lambda
    type dec
    datatype noMatch = datatype CoreML.Exp.noMatch
    datatype node = App         of t * t
                  | Case        of { info : { kind   : string
                                            , nest   : string list
                                            , lay    : unit -> Layout.t
                                            , region : Region.t
                                             }
                                   , abnm : noMatch                     * 
                                            Control.Elaborate.DiagDI.t  *
                                            Control.Elaborate.DiagEIW.t *
                                            Control.Elaborate.DiagEIW.t
                                   , test : t
                                   , rules: { pat: Pat.t, exp: t, lay: Layout.t option } vector
                                   }
                  | Constructor of { con: CoreML.Con.t
                                   , targs: TyAtom.Type.t vector  (* type args during instantiation for polymorphic constructors *)
                                   }
                  | Constant    of CoreML.Const.t
                  | Var         of { var: CoreML.Var.t
                                   , targs: TyAtom.Type.t vector  (* type args during instantiation for polymorphic functions *)
                                   }
                  | Lambda      of lambda
                  | Let         of dec vector * t 
                  | Seq         of t vector
                  | CasePar     of                                (* when elaborating a function, we need to scrutinize a tuple of 
                                                                     arguments at the same time without tuple type. Therefore 
                                                                     CasePar is invented for this purpose. *)
                                   { test: t vector
                                   , rules: { pat: Pat.t vector, exp: t, lay: Layout.t option } vector 
                                   }
                  | PrimApp     of { prim: TyAtom.Type.t CoreML.Prim.t, args : t vector, targs: TyAtom.Type.t vector}

    val make : node * TyAtom.Type.t -> t
    val ty   : t -> TyAtom.Type.t
    val node : t -> node

    (* Make a monophic variable*)
    val var0 : CoreML.Var.t * TyAtom.Type.t -> t

    (* Make a polymorphic variable *)
    val var  : {var: CoreML.Var.t, targs: TyAtom.Type.t vector} * TyAtom.Type.t -> t

    (* Make a polymorphic constructor *)
    val con  : {con: CoreML.Con.t, targs: TyAtom.Type.t vector} * TyAtom.Type.t -> t

    (* Make a constant *)
    val const: CoreML.Const.t * TyAtom.Type.t -> t
    
    val seq  : t vector -> t
    val lete : dec vector * t -> t
    val ifthen : t * t * t -> t
    val lambda : lambda -> t
    val app    : t * t -> t
    val casee  : { test : t
                 , rules: { pat: Pat.t       , exp: t, lay: Layout.t option } vector
                 , kind : string
                 } ->  t
    val casepar: { test : t vector
                 , rules: { pat: Pat.t vector, exp: t, lay: Layout.t option } vector
                 } -> t

    val truee  : t
    val falsee : t

    val subst : TyAtom.Subst.t * t -> t

    val layout   : t -> Layout.t
    val toCoreML : t -> CoreML.Exp.t
  end

  structure Lambda: sig
    type t

    val make : CoreML.Var.t * TyAtom.Type.t * Exp.t -> t
    val ty : t -> TyAtom.Type.t * TyAtom.Type.t

    val subst : TyAtom.Subst.t * t -> t

    val layout   : t -> Layout.t
    val toCoreML : t -> CoreML.Lambda.t
  end

  structure Dec: sig
    type t

    val subst   : TyAtom.Subst.t * t -> t

    val valbind : { vars      : TyAtom.VarSet.t
                  , valbind   : (Pat.t * Region.t * Exp.t) vector
                  , recvalbind: (CoreML.Var.t * Lambda.t) vector
                  } -> t
    val funbind : { vars      : TyAtom.VarSet.t
                  , decs      : (CoreML.Var.t * Lambda.t) vector 
                  } -> t

    val toCoreML : t -> CoreML.Dec.t

    val layout : t -> Layout.t
  end

  sharing type Exp.lambda = Lambda.t
  sharing type Exp.dec    = Dec.t

end

functor CoreLang (S: CORE_LANG_STRUCTS) : CORE_LANG = 
struct
  open S

  structure Pat = 
  struct
    datatype t = P of node * TyAtom.Type.t
    and node   = Var      of Var.t
               | Const    of Const.t
               | Con      of { con  : Con.t
                             , arg  : t option
                             , targs: TyAtom.Type.t vector
                             }
               | Layered  of Var.t * t
               | Wild
    fun make nt        = P  nt
    fun ty   (P (n,t)) = t
    fun node (P (n,t)) = n
    fun layout pat     = Layout.empty

    fun isWild pat = 
      case pat of
        Wild => true
      | _    => false

    val truee  = P (Con { con   = Con.truee
                        , arg   = NONE
                        , targs = Vector.new0 ()}, 
                    TyAtom.Type.bool)
    val falsee = P (Con { con   = Con.falsee
                        , arg   = NONE
                        , targs = Vector.new0 ()},
                    TyAtom.Type.bool)

    fun subst (rho, P (n, t)) = 
      let 
        val substT = fn t => TyAtom.subst (rho, t)
        val t = substT t
        val n = case n of
                  Con {con = con, arg = p, targs = ts} => 
                    Con { con   = con
                        , arg   = Option.map (p, fn p => subst (rho, p))
                        , targs = Vector.map (ts, substT) 
                        }
                | Layered (v, p) =>
                    Layered (v, subst (rho, p))
                | x => x
      in
        P (n, t)
      end
    

    structure CP = CoreML.Pat
    fun toCoreML pat = 
      let 
        val typat = ty pat
      in
        case node pat of
          Var   v       => CP.var (v, typat)
        | Const c       => CP.make (CP.Const (fn () => c), typat)
        | Con   a       => CP.make (CP.Con { con = #con a
                                           , arg = Option.map (#arg a, toCoreML)
                                           , targs = #targs a},
                                    typat)
        | Layered (v,p) => CP.make (CP.Layered (v, toCoreML p), typat)
        | Wild          => CP.wild typat
      end
  end

  datatype exp  = E of expNode * TyAtom.Type.t
  and   expNode = App  of exp * exp
                | Case of exp * ((Pat.t * exp) vector)
                | Constructor of { con: Con.t, targs: TyAtom.Type.t vector }
                | Constant    of Const.t
                | Var         of Var.t
                | Lambda      of lambda
                | Let         of dec vector * exp 
                | Seq         of exp vector
                | CasePar     of exp vector * ((Pat.t vector * exp) vector)
  and    lambda = L of Var.t * TyAtom.Type.t * exp
  and    dec    = Val of { vars       : TyAtom.VarSet.t
                         , valbind    : (Pat.t * exp) vector
                         , recvalbind : (Var.t * lambda) vector
                         }
                | Fun of { vars       : TyAtom.VarSet.t
                         , decs       : (Var.t * lambda) vector
                         } 

 
  fun tyExp    (E (_,t))     = t
  fun tyLambda (L (_, t, e)) = (t, tyExp e)

  fun substExp (rho, E (e,t)) = 
      let
        val e = case e of 
                  App (e1,e2) =>
                    App  (substExp (rho, e1), substExp (rho, e2))
                | Case (e,ps) => 
                    Case (substExp (rho, e),
                          Vector.map (ps, substPatExp rho))
                | Lambda lam  =>
                    Lambda (substLam (rho ,lam))
                | Let (ds, e) => 
                    Let (ds, substExp (rho, e))
                | Seq es      =>
                    Seq (Vector.map (es, fn e => substExp (rho, e)))
                | x           => x
      in
        E (e, TyAtom.subst (rho, t)) 
      end
  and substLam (rho, L (v, t, e)) =
      L (v, TyAtom.subst (rho, t), substExp (rho, e)) 
  and substDec (rho, Val {vars = v, valbind = b, recvalbind = r}) =
      Val { vars       = v, 
            valbind    = Vector.map (b, substPatExp rho),
            recvalbind = Vector.map (r, substVarLam rho) }
  and substPatExp rho (p, e) = (p, substExp (rho, e))
  and substVarLam rho (v, l) = (v, substLam (rho ,l))

  structure CE = CoreML.Exp
  structure CL = CoreML.Lambda
  fun toCoreMLExp (E (exp, tyexp)) = 
    case exp of
      App (e1, e2)  => CE.make (CE.App (toCoreMLExp e1, toCoreMLExp e2), tyexp)
    | Constructor c => CE.make (CE.Con (#con c, #targs c), tyexp)
    | Constant c    => CE.make (CE.Const (fn ()=>c), tyexp)
    | Var v         => CE.var (v, tyexp)
    | Lambda l      => CE.make (CE.Lambda (toCoreMLLam l), tyexp)
    | Let (ds, e)   => CE.make (CE.Let (Vector.map (ds, toCoreMLDec),
                                        toCoreMLExp e),
                                tyexp)
    | Seq es        => CE.make (CE.Seq (Vector.map (es, toCoreMLExp)), tyexp)
    | Case (e,cs)     => let
                           val casebdy = 
                             { test   = toCoreMLExp e
                             , rules  = Vector.map (cs, fn (p,e) => 
                                          { pat = Pat.toCoreML p
                                          , exp = toCoreMLExp e            
                                          , lay = ?
                                          })                  
                             , kind   = ?                     
                             , lay    = ?
                             , nest   = ?
                             , region = ?
                             , noMatch                 = ?
                             , nonexhaustiveExnMatch   = ?
                             , nonexhaustiveMatch      = ?
                             , redundantMatch          = ?
                             }
                         in
                           CE.make (CE.Case casebdy, tyexp)
                         end
    | CasePar (es,cs) => let
                           val casebdy = 
                             { test   = CE.tuple (Vector.map (es, toCoreMLExp))
                             , rules  = Vector.map (cs, fn (ps, e) => 
                                          { pat = Vector.map (ps, Pat.toCoreML)
                                          , epx = toCoreMLExp e
                                          , lay = ?
                                          })
                             , kind   = ?
                             , lay    = ?
                             , nest   = ?
                             , region = ?
                             , noMatch                 = ?
                             , nonexhaustiveExnMatch   = ?
                             , nonexhaustiveMatch      = ?
                             , redundantMatch          = ?
                             }
                         in
                           CE.make (CE.Case casebdy, tyexp)
                         end
  and toCoreMLDec dec = 
    case dec of
      Val vbind => 
        { tyvars = TyAtom.VarSet.toV (#vars vbind)
        , vbs    = Vector.map (#valbind vbind, 
                     fn (pat, exp) => 
                       { pat = Pat.toCoreML pat
                       , exp = toCoreMLExp exp
                       , patRegion = ?
                       , nest      = ?
                       , lay       = ?
                       })
        , rvbs   = Vector.map (#recvalbind vbind,
                     fn (var, lam) => 
                       { var    = var
                       , lambda = Lambda.toCoreML lam 
                       })
        , nonexhaustiveMatch    = ?
        , nonexhaustiveExnMatch = ?
        }
    | Fun fbind => 
        { tyvars = TyAtom.VarSet.toV (#vars fbind)
        , decs   = Vector.map (#decs fbind, 
                     fn (var, lam) =>
                       { var    = var
                       , lambda = Lambda.toCoreML lam
                       })
        }
  and toCoreMLLam (L (v, t, e)) = 
    CL.make { arg = v
            , argType = t
            , body = toCoreMLExp e
            , mayInline = ? 
            }

  structure Exp = 
  struct
    type t      = exp
    type lambda = lambda
    type dec    = dec
    datatype node = datatype expNode
    
    fun make ndty     = E  ndty
    val ty            = tyExp
    fun node (E ndty) = #1 ndty

    val subst = substExp

    fun var   (c,t) = E (Var c, t)
    fun con   (c,t) = E (Constructor c, t)
    fun const (c,t) = E (Constant    c, t)
    fun seq es      = if Vector.length es < 1 then 
                        Error.bug "expression sequence of length 0"
                      else
                        E (Seq es, ty (Vector.last es))

    fun ifthen (e1,e2,e3) = 
      let 
        val n = Case (e1, 
                  Vector.new2 
                    ((Pat.truee , e2),
                     (Pat.falsee, e3)))
      in
        E (n, ty e3)
      end

    fun lambda lam  = E (Lambda lam, TyAtom.Type.arrow (tyLambda lam)) 

    fun app (e1,e2) = 
      let
        val ty1 = ty e1
        val ty2 = ty e2
        exception CompilerBugCannotMakeAppExp
      in 
        case TyAtom.Type.deArrow ty1 of
          SOME (ty11, ty12) => 
            if TyAtom.Type.equals (ty11, ty2) then
              E (App (e1,e2), ty2)
            else
              raise CompilerBugCannotMakeAppExp 
        | NONE =>
            raise CompilerBugCannotMakeAppExp
      end

    fun lete (d, e) = E (Let (d, e), ty e)


    exception CompilerBugEmptyCaseClause
    fun casee (e, cs) = 
      let
        val ty = if Vector.length cs = 0 then
                   raise CompilerBugEmptyCaseClause
                 else
                   (ty o #2 o Vector.sub) (cs, 0)
      in
        E (Case (e, cs), ty)
      end

    fun casepar (es, cs) = 
      let
        val ty = if Vector.length cs = 0 then
                   raise CompilerBugEmptyCaseClause
                 else
                   (ty o #2 o Vector.sub) (cs, 0)
      in
        E (CasePar (es, cs), ty)
      end

    val truee  = E (Constructor { con   = Con.fromBool true
                                , targs = Vector.new0 () },
                    TyAtom.Type.bool)
    val falsee = E (Constructor { con   = Con.fromBool false
                                , targs = Vector.new0 () },
                    TyAtom.Type.bool)

    val toCoreML = toCoreMLExp
  end

  structure Lambda = 
  struct
    datatype t = datatype lambda

    fun make (v, t, c) = L (v, t, c)
    val ty = tyLambda
    val subst = substLam

    val toCoreML = toCoreMLLam
  end

  structure Dec =
  struct
    datatype t = datatype dec

    val subst = substDec
    fun valbind x = Val x
    fun funbind x = Fun x

    val toCoreML = toCoreMLDec
  end

end

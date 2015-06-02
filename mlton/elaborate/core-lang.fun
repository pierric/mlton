functor CoreLang (S: CORE_LANG_STRUCTS) : CORE_LANG = 
struct
  open S

  structure Pat = 
  struct
    datatype t = Var      of Var.t
               | Const    of Const.t
               | Con      of Con.t * t option
               | Layered  of Var.t * t
               | Wild

    fun layout pat = Layout.empty

    fun isWild pat = 
      case pat of
        Wild => true
      | _    => false

    val truee  = Con (Con.truee,  NONE)
    val falsee = Con (Con.falsee, NONE)
  end

  datatype exp  = E of expNode * TyAtom.Type.t
  and   expNode = App  of exp * exp
                | Case of exp * ((Pat.t * exp) vector)
                | Constructor of Con.t
                | Constant    of Const.t
                | Var         of Var.t
                | Lambda      of lambda
                | Let         of dec vector * exp 
                | Seq         of exp * exp
                | CasePar     of exp vector * ((Pat.t vector * exp) vector)
  and    lambda = L of Var.t * TyAtom.Type.t * exp
  and    dec    = Val of { vars       : TyAtom.VarSet.t
                         , valbind    : (Pat.t * exp) vector
                         , recvalbind : (Var.t * lambda) vector
                         }
                | Fun of { vars      : TyAtom.VarSet.t
                         , decs      : (Var.t * lambda) vector
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
                | Let (ds, e)  => 
                    Let (ds, substExp (rho, e))
                | Seq (e1,e2) =>
                    Seq (substExp (rho, e1), substExp (rho, e2))
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
    fun seq (e1,e2) = E (Seq (e1,e2), ty e2)

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

    val truee  = E (Constructor (Con.fromBool true ), TyAtom.Type.bool)
    val falsee = E (Constructor (Con.fromBool false), TyAtom.Type.bool)
  end

  structure Lambda = 
  struct
    datatype t = datatype lambda

    fun make (v, t, c) = L (v, t, c)
    val ty = tyLambda
    val subst = substLam
  end

  structure Dec =
  struct
    datatype t = datatype dec

    val subst = substDec
    fun valbind x = Val x
    fun funbind x = Fun x
  end

end

structure TypeAtom : TYPE_ATOM = 
struct

  structure Tyvar = 
  struct
    type t = int
    val current = ref 0
    fun make () = 
      let
        val tyvar = !current
        val _     = current := tyvar + 1; 
      in 
        tyvar
      end
    fun layout tyvar = 
      Layout.str ("`a" ^ Int.toString tyvar)
  end

  structure Tycon = 
  struct
    datatype t = Tycon
    fun layout tycon = 
      Layout.str "tycon"
  end
   
  type tyvar = Tyvar.t
  type tycon = Tycon.t
  exception NotUnifiable
  exception NotMergeable

  val { singleton = singleton
      , unions    = unions
      , -         = subtract
      , ...}
      = List.set {equals = op=, layout = Tyvar.layout}

  structure Type = 
  struct
    datatype t = FlexTyvar of tyvar
               | RigdTyvar of tyvar
               | Cons of tycon * (t list)

    fun layout typ = 
      case typ of
        FlexTyvar tyvar    => Tyvar.layout tyvar
      | RigdTyvar tyvar    => Tyvar.layout tyvar
      | Cons (tycon, typs) => Layout.seq
                                [ Tycon.layout tycon
                                , List.layout layout typs ]
    fun free typ = 
      case typ of
        RigdTyvar x    => singleton x
      | FlexTyvar x    => singleton x
      | Cons (_, typs) => unions (List.map (typs, free))
  end
 
  type typ = Type.t

  structure Scheme =
  struct
    datatype t = Scheme of tyvar list * Type.t
    fun make s = Scheme s 
    fun layout (Scheme (tyvars, typ)) =
      Layout.seq
        [ Layout.str "forall "
        , List.layout Tyvar.layout tyvars
        , Type.layout typ]
  end

  structure Subst = 
  struct
    type t = (tyvar * Type.t) list 

    local
      fun layoutOne (tyvar, typ) = 
        Layout.seq 
          [ Tyvar.layout tyvar
          , Layout.str "=>"
          , Type.layout typ]
      fun eq (s1, s2) = (#1 s1) = (#1 s2)
      val { empty= empty
          , singleton = singleton
          , + = union
          , areDisjoint = disjoint 
          , ...}
          = List.set {equals = eq , layout = layoutOne}
    in
      val empty  = empty
      fun make s = 
        singleton s
      fun merge ss =
        if disjoint ss then
          union ss
        else 
          raise NotMergeable
      fun compose (rho1, rho2) = 
        raise NotUnifiable
      fun layout s =
        Layout.align (List.map (s, layoutOne))
    end
  end


  local
    open Type
    fun substOne (s as (tyvar0, typ0), t) =
      case t of
        FlexTyvar tyvar    => 
          if tyvar0 = tyvar then typ0 else t
      | RigdTyvar tyvar    => 
          if tyvar0 = tyvar then typ0 else t
      | Cons (tycon, typs) => 
          Cons (tycon, List.map (typs, fn t => substOne (s, t)))
  in
    fun subst (rho, typ) = List.fold (rho, typ, substOne)

    fun unify (typ1, typ2) = 
      case (typ1, typ2) of
        (RigdTyvar x, RigdTyvar y) => if x = y then 
                                        Subst.empty 
                                      else
                                        Error.bug "Not possible"
      | (RigdTyvar _, _)           => raise NotUnifiable
      | (_, RigdTyvar _)           => raise NotUnifiable
      | (FlexTyvar x, FlexTyvar y) => if x = y then 
                                        Subst.empty 
                                      else
                                        Subst.make (x, typ2)
      | (FlexTyvar x, _)           => Subst.make (x, typ2)
      | (_, FlexTyvar y)           => Subst.make (y, typ1)
      | (Cons (con1, typs1), Cons (con2, typs2)) => 
          if con1 = con2 then
            unifyV (typs1, typs2) 
          else 
            raise NotUnifiable

    and unifyV (vt1, vt2) = List.fold2 (vt1, vt2, Subst.empty, 
                              fn (ty1, ty2, rho) => 
                                let
                                  val ty1' = subst (rho,ty1)
                                  val ty2' = subst (rho,ty2)
                                  val rho' = unify (ty1', ty2')
                                in
                                  Subst.compose (rho', rho)
                                end)

    fun gen (env, typ) = 
      let 
        val qfv = subtract (Type.free typ, env)
      in
        Scheme.make (qfv, typ)
      end

    fun inst (Scheme.Scheme (tyvars, typ)) = 
      let
        val len   = List.length tyvars
        val ntyps = List.duplicate (len, FlexTyvar o Tyvar.make)
      in
        List.fold2 (tyvars, ntyps, typ,
          fn (tyvar, ntyp, typ) =>
            subst (Subst.make (tyvar, ntyp), typ))
      end
  end

  structure TypFun = struct
    datatype t = TypFun of tyvar list * Type.t
    fun apply (TypFun (parms, body), args) = 
      let
        val _    = if List.length parms <> List.length args then
                     Error.bug "TyFun: arity does not match"
                   else
                     ()
      in
        List.fold2 (parms, args, body, 
          fn (tyvar, typ, body) =>
            subst (Subst.make (tyvar, typ), body))
      end
  end

end


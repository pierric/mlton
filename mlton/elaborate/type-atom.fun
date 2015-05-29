functor TypeAtom (S : TYPE_ATOM_STRUCTS) : TYPE_ATOM = 
struct
  open S

  type tyvar  = Tyvar.t
  type tycon  = Tycon.t

  structure VarSet = 
  struct
  type t = tyvar list
  val { empty       = empty
      , singleton   = singleton
      , +           = append
      , -           = subtract
      , areDisjoint = disjoint
      , unions      = unions
      , layout      = layout
      , ...}
      = List.set {equals = Tyvar.equals, layout = Tyvar.layout}
  end

  exception NotUnifiable
  exception NotMergeable


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
        RigdTyvar x    => VarSet.singleton x
      | FlexTyvar x    => VarSet.singleton x
      | Cons (_, typs) => VarSet.unions (List.map (typs, free))

    fun newNoname () = FlexTyvar (Tyvar.newNoname {equality = false})

    fun equals (t1, t2) = 
      case (t1, t2) of
        (FlexTyvar v1, FlexTyvar v2) =>Tyvar.equals (v1, v2)
      | (RigdTyvar v1, RigdTyvar v2) => Tyvar.equals (v1, v2)
      | (Cons (con1, args1), Cons (con2, args2)) =>
          if Tycon.equals (con1, con2) then
            List.forall2 (args1, args2, equals)
          else
            false

    val bool = Cons (Tycon.bool, [])
    fun arrow (t1,t2) = Cons (Tycon.arrow, [t1,t2])
    
    fun deArrow typ = 
      case typ of
        Cons (con, targs) =>
           if Tycon.equals (con, Tycon.arrow) then
             let
               val _ = if List.length targs <> 2 then
                         Error.bug "Arrow used as non-binary operator"
                       else 
                         ()
               val [ta1, ta2] = targs
             in
               SOME (ta1, ta2)
             end
           else
             NONE
      | _ => NONE
  end
 
  type typ = Type.t

  structure Subst = 
  struct
    type t = (tyvar * typ) list 

    local
      fun layoutOne (tyvar, typ) = 
        Layout.seq 
          [ Tyvar.layout tyvar
          , Layout.str "=>"
          , Type.layout typ]
      fun eq (s1, s2) = Tyvar.equals ((#1 s1), (#1 s2))
      val { empty= empty
          , singleton = singleton
          , + = union
          , - = subtract
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
      fun composeL [] = empty
        | composeL (rho :: rhos) = compose (rho, composeL rhos)
      
      fun minus (rho, bound) = 
        subtract (rho, List.map (bound, fn v => (v, Type.bool)))

      fun layout s =
        Layout.align (List.map (s, layoutOne))
    end
  end

  structure Scheme =
  struct
    datatype t = Scheme of tyvar list * typ
    fun make s = Scheme s 

    fun fromType t = make (VarSet.empty, t) 

    fun free (Scheme (bound, typ)) = 
      VarSet.subtract (Type.free typ, bound)

    fun layout (Scheme (bound, typ)) =
      Layout.seq
        [ Layout.str "forall "
        , VarSet.layout bound
        , Type.layout typ]
  end


  local
    open Type
    fun substOne (s as (tyvar0, typ0), t) =
      case t of
        FlexTyvar tyvar    => 
          if Tyvar.equals (tyvar0, tyvar) then typ0 else t
      | RigdTyvar tyvar    => 
          if Tyvar.equals (tyvar0, tyvar) then typ0 else t
      | Cons (tycon, typs) => 
          Cons (tycon, List.map (typs, fn t => substOne (s, t)))
  in
    fun subst (rho, typ) = List.fold (rho, typ, substOne)

    fun unify (typ1, typ2) = 
      case (typ1, typ2) of
        (RigdTyvar x, RigdTyvar y) => if Tyvar.equals (x, y) then 
                                        Subst.empty 
                                      else
                                        Error.bug "Not possible"
      | (RigdTyvar _, _)           => raise NotUnifiable
      | (_, RigdTyvar _)           => raise NotUnifiable
      | (FlexTyvar x, FlexTyvar y) => if Tyvar.equals (x, y) then 
                                        Subst.empty 
                                      else
                                        Subst.make (x, typ2)
      | (FlexTyvar x, _)           => Subst.make (x, typ2)
      | (_, FlexTyvar y)           => Subst.make (y, typ1)
      | (Cons (con1, typs1), Cons (con2, typs2)) => 
          if Tycon.equals (con1, con2) then
            unifyL (typs1, typs2) 
          else 
            raise NotUnifiable

    and unifyL (vt1, vt2) = List.fold2 (vt1, vt2, Subst.empty, 
                              fn (ty1, ty2, rho) => 
                                let
                                  val ty1' = subst (rho,ty1)
                                  val ty2' = subst (rho,ty2)
                                  val rho' = unify (ty1', ty2')
                                in
                                  Subst.compose (rho', rho)
                                end)


    local 
      fun loop (ty, rho, typs) =
        case typs of
          []      => rho
        | x :: xs =>
            let
              val x    = subst (rho, x)
              val rho' = unify (ty, x)
              val ty'  = subst (rho', ty)
              val rho' = Subst.compose (rho', rho)
            in 
              loop (ty', rho', xs)
            end
    in
      fun unifyS typs = 
        case typs of
          []      => Subst.empty
        | x :: xs => loop (x, Subst.empty, xs)
    end
    
    fun gen (env, typ) = 
      let 
        val qfv = VarSet.subtract (Type.free typ, env)
      in
        Scheme.make (qfv, typ)
      end

    fun inst (Scheme.Scheme (tyvars, typ)) = 
      let
        val len   = List.length tyvars
        val ntyps = List.duplicate (len, fn () => FlexTyvar (Tyvar.newNoname {equality = false}))
      in
        List.fold2 (tyvars, ntyps, typ,
          fn (tyvar, ntyp, typ) =>
            subst (Subst.make (tyvar, ntyp), typ))
      end
  end

  local
    val substType = subst
  in
    structure Scheme = 
    struct
      open Scheme
      fun subst (rho, Scheme (bound, typ)) = 
        Scheme (bound, substType (Subst.minus (rho, bound), typ))
    end
  end

  structure TypFun = struct
    datatype t = TypFun of tyvar list * typ 
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


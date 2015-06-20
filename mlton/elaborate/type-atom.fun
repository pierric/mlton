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
        , equals      = equals
        , ...}
        = List.set {equals = Tyvar.equals, layout = Tyvar.layout}
    fun fromL x    = x
    fun fromV vars = Vector.toList   vars
    fun toV   vars = Vector.fromList vars
    fun isEmpty [] = true
      | isEmpty _  = false
  end

  exception NotUnifiable
  exception NotMergeable


  structure Type =
  struct
    datatype t = FlexTyvar of tyvar
               | RigdTyvar of tyvar
               | Cons of tycon * (t vector)

    fun layout typ =
      case typ of
        FlexTyvar tyvar    => Tyvar.layout tyvar
      | RigdTyvar tyvar    => Tyvar.layout tyvar
      | Cons (tycon, typs) => Layout.seq
                                [ Tycon.layout tycon
                                , Vector.layout layout typs ]

    fun free typ =
      case typ of
        RigdTyvar x    => VarSet.singleton x
      | FlexTyvar x    => VarSet.singleton x
      | Cons (_, typs) => VarSet.unions (Vector.toListMap (typs, free))

    fun newNoname () = FlexTyvar (Tyvar.newNoname {equality = false})
    fun var v = FlexTyvar v
    fun equals (t1, t2) =
      case (t1, t2) of
        (FlexTyvar v1, FlexTyvar v2) =>Tyvar.equals (v1, v2)
      | (RigdTyvar v1, RigdTyvar v2) => Tyvar.equals (v1, v2)
      | (Cons (con1, args1), Cons (con2, args2)) =>
          if Tycon.equals (con1, con2) then
            Vector.forall2 (args1, args2, equals)
          else
            false
      | _  => false

    fun con x = Cons x

    fun arrow (t1,t2) = Cons (Tycon.arrow, Vector.new2 (t1,t2))

    fun deArrow typ =
      case typ of
        Cons (con, targs) =>
           if Tycon.equals (con, Tycon.arrow) then
             let
               val _ = if Vector.length targs <> 2 then
                         Error.bug "Arrow used as non-binary operator"
                       else
                         ()
               val ta1 = Vector.sub (targs, 0)
               val ta2 = Vector.sub (targs, 1)
             in
               SOME (ta1, ta2)
             end
           else
             NONE
      | _ => NONE

    fun deArgs typ =
      let
        val ret  = ref typ
        val args = List.unfold (typ, fn typ =>
                     case deArrow typ of
                       x as SOME (_, r) => (ret := r; x)
                     | NONE             => NONE)
      in
        (args, !ret)
      end


    fun args (args, ret) =
      List.fold (args, ret, arrow)

    fun deConOpt typ : (tycon * t vector) option =
      case typ of
        Cons conargs => SOME conargs
      | _            => NONE

    fun isCharX typ  : bool =
      case typ of
        Cons (c, _)  =>  Tycon.isCharX c
      | _            => false

    fun isInt   typ  : bool =
      case typ of
        Cons (c, _)  => Tycon.isIntX c
      | _            => false

    fun tuple   typs = Cons (Tycon.tuple, typs)

    val bool   = Cons (Tycon.bool,   Vector.new0 ())
    fun isBool t =
      case t of
        Cons (tycon, args) => Tycon.equals (tycon, Tycon.bool) andalso
                              Vector.length args = 0
      | _ => false

    val unit = tuple (Vector.new0 ())
    fun isUnit t =
      case t of
        Cons (tycon, args) => Tycon.equals (tycon, Tycon.tuple) andalso
                              Vector.length args = 0
      | _ => false

    fun deStringX t =
      case t of
        Cons (tycon, args) => if Tycon.equals (tycon, Tycon.vector) andalso
                                 Vector.length args = 1 then
                                case Vector.sub (args, 0) of
                                  Cons (tycon, _) => if Tycon.isCharX (tycon) then
                                                       SOME (Tycon.deCharX tycon)
                                                     else
                                                       NONE
                                | _ => NONE
                              else
                                NONE
      | _ => NONE


    fun makeHom' {con = mapcon, rvar = maprvar, fvar = mapfvar} =
      let
        fun hom typ =
          case typ of
            Cons (con, args) => mapcon (con, Vector.map (args, hom))
          | RigdTyvar tyvar  => maprvar tyvar
          | FlexTyvar tyvar  => mapfvar tyvar
      in
        {hom = hom}
      end

    val synonym = let
                    fun mapcon (tycon, args) =
                      if Tycon.isCharX tycon then
                        con (Tycon.word ((WordSize.fromBits o CharSize.bits o Tycon.deCharX) tycon), args)
                      else if Tycon.isIntX tycon then
                        case Tycon.deIntX tycon of
                          SOME size => con (Tycon.word ((WordSize.fromBits o IntSize.bits) size), args)
                        | NONE      => con (tycon, args)
                      else
                        con (tycon, args)
                    fun maprvar x = RigdTyvar x
                    fun mapfvar x = FlexTyvar x
                    val {hom, ...} = makeHom' {con = mapcon, rvar = maprvar, fvar = mapfvar}
                  in
                    hom
                  end

    fun makeHom {con = mapcon, var = mapvar} =
      let
        val {hom, ...} = makeHom' {con = mapcon, rvar = mapvar, fvar = mapvar}
      in
        {hom = hom, destroy = fn () => ()}
      end

  end

  (* Include the basic primitive types in TypeOps, but we ensure Type overrides TypeOps *)
  structure Ops = TypeOps (structure Tycon = Tycon
                           open Type)
  structure Type =
  struct
    open Ops Type
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

      fun minus (rho, bound) =
        subtract (rho, List.map (bound, fn v => (v, Type.bool)))

      fun layout s =
        Layout.align (List.map (s, layoutOne))
    end
  end

  structure Scheme =
  struct
    datatype t = Scheme of VarSet.t * typ
    fun make s = Scheme s

    fun bound (Scheme (bd, _)) = bd

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
          Cons (tycon, Vector.map (typs, fn t => substOne (s, t)))
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
            unifyV (typs1, typs2)
          else
            raise NotUnifiable

    and unifyV (vt1, vt2) = Vector.fold2 (vt1, vt2, Subst.empty,
                              fn (ty1, ty2, rho) =>
                                let
                                  val ty1' = subst (rho,ty1)
                                  val ty2' = subst (rho,ty2)
                                  val rho' = unify (ty1', ty2')
                                in
                                  compose (rho', rho)
                                end)

    and compose (rho1, rho2) =
        Subst.merge (rho1, List.map (rho2, fn (v, t) => (v, subst (rho1, t))))

    fun unifyL (vt1, vt2) = unifyV (Vector.fromList vt1, Vector.fromList vt2)


    local
      fun loop (ty, rho, typs) =
        case typs of
          []      => rho
        | x :: xs =>
            let
              val x    = subst (rho, x)
              val rho' = unify (ty, x)
              val ty'  = subst (rho', ty)
              val rho' =compose (rho', rho)
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
        val ret   = List.fold2 (tyvars, ntyps, typ,
                      fn (tyvar, ntyp, typ) =>
                        subst (Subst.make (tyvar, ntyp), typ))
      in
        (ret, Vector.fromList ntyps)
      end
  end

  local
    val substType = subst
    val genType   = gen
    val composeT  = compose
    exception CompilerBugGenBoundedScheme
  in
    structure Scheme =
    struct
      open Scheme

      fun subst (rho, Scheme (bound, typ)) =
        Scheme (bound, substType (Subst.minus (rho, bound), typ))

      fun gen (env, scheme as Scheme (bound, typ)) =
        if VarSet.isEmpty bound then
          genType (env, typ)
        else
          raise CompilerBugGenBoundedScheme
    end

    structure Subst =
    struct
      open Subst

      val compose = composeT

      fun composeL []            = empty
        | composeL (rho :: rhos) = compose (rho, composeL rhos)
    end
  end

  structure TypFun =
  struct

    datatype arity = Arity of int | NaryCon
    datatype t = TypCon of tycon * TyconKind.t
               | TypFun of tyvar vector * typ
    fun apply (typfun, args) =
      let
        val n = Vector.length args
      in
        case typfun of
          TypFun (parms, body) =>
            let
              val _ = if Vector.length parms <> n then
                        Error.bug "TyFun: arity does not match"
                      else
                        ()
            in
              Vector.fold2 (parms, args, body,
                fn (tyvar, typ, body) =>
                  subst (Subst.make (tyvar, typ), body))
            end
        | TypCon (con, kind) =>
          let
            val _ = case kind of
                      TyconKind.Arity n' =>
                        if n <> n' then
                          Error.bug "TyFun: arity does not match"
                        else
                          ()
                    | _ => ()
          in
            Type.Cons (con, args)
          end
      end

    fun arity (TypCon (_, TyconKind.Arity n)) = Arity n
      | arity (TypCon (_, TyconKind.Nary   )) = NaryCon
      | arity (TypFun (parms, _))             = Arity (Vector.length parms)

    fun fromTycon (tycon, kind) =
        TypCon (tycon, kind)

    fun fromType t = TypFun (Vector.new0 (), t)
  end

end

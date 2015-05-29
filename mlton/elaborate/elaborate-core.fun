functor ElaborateCore (S: ELABORATE_CORE_STRUCTS) = 
struct
  open S

  exception UnsupportedPatternSyntax
  exception UnsupportedExpressionSyntax
  local
    structure Apat = Ast.Pat
    structure Cpat = Core.Pat
    structure Aexp = Ast.Exp
    structure Alam = Ast.Match
    structure Cexp = Core.Exp
    structure Clam = Core.Lambda
    open Control
  in

    fun elaboratePat (env, apat) =
      case Apat.node apat of
        Apat.App (longcon, pat) =>
          let 
            val optval = Env.lookupCon (env, longcon)
            val _      = if Option.isNone optval then
                           error (Apat.region apat,
                             Layout.str "Unknown constructor",
                             Layout.empty)
                         else
                           ()
            val valdef = Option.valOf optval
            val contyp = TyAtom.inst (Env.ValDef.scheme valdef)
            val optarr = TyAtom.Type.deArrow contyp
            val _      = if Option.isNone optarr then
                           error (Ast.Longcon.region longcon,
                             Layout.str "",
                             Layout.empty)
                         else
                           ()
            val (contyp1, contyp2) = Option.valOf optarr
            val (argcpat, argtyp, argenv') = elaboratePat (env, pat)
            val rho = TyAtom.unify (contyp1, argtyp)
          in
            ( Cpat.Con ((Env.ValDef.deCon o Env.ValDef.value) valdef,
                        SOME argcpat),
              TyAtom.subst (rho, contyp2), 
              Env.subst (rho, argenv') )
          end
      | Apat.Var {name=longvid, ...}  => 
          let
            val optvd = Env.lookupVid (env, longvid)
            open Env.ValDef
          in
            if Option.isSome optvd andalso 
               (isCon o value) (Option.valOf optvd) then
              let 
                val valdef   = Option.valOf optvd
                val VCON con = value valdef
                val contyp = TyAtom.inst (scheme valdef)
                val _  = if Option.isSome (TyAtom.Type.deArrow contyp) then
                            error (Ast.Longvid.region longvid,
                              Layout.str "Constructor expects its argument",
                              Layout.empty)
                         else
                           ()
              in
                (Cpat.Con ((deCon o value) valdef, NONE),
                 contyp,
                 Env.empty)
              end
            else
              let 
                val typ = TyAtom.Type.newNoname ()
                val var = Core.Var.newNoname () 
              in
                (Cpat.Var var,
                 typ,
                 Env.extendVid
                   ((#2 o Ast.Longvid.split) longvid,
                    make (VVAR var, TyAtom.Scheme.fromType typ))
                   Env.empty)
              end
          end
      | Apat.Constraint (pat, typ) =>
          let 
            val (cpat, typ1, env') = elaboratePat (env, pat)
            val typ2               = ElabTy.elaborateT (env, typ)
            val rho                = TyAtom.unify (typ1, typ2)
          in
            (cpat, TyAtom.subst (rho, typ1), Env.subst (rho, env'))
          end
      | Apat.Layered {var=var, pat=pat, constraint=typ,...} =>
          let 
            val (cpat, typ1, env') = elaboratePat (env, pat)
            val cvar               = Core.Var.newNoname () 
            fun bindvar vartyp     = let 
                                       open Env.ValDef
                                       val s = TyAtom.Scheme.fromType vartyp
                                     in
                                       Env.extendVid (Ast.Vid.fromVar var, make (VVAR cvar, s))
                                     end
          in
            case typ of 
              NONE     => 
                (cpat, typ1, bindvar typ1 env')
            | SOME typ => 
                let 
                  val typ2 = ElabTy.elaborateT (env, typ)
                  val rho  = TyAtom.unify (typ1, typ2)
                  val ctyp = TyAtom.subst (rho, typ1)
                in
                  (cpat, ctyp , bindvar ctyp (Env.subst (rho, env')))
                end
          end
      | Apat.Tuple pats =>
          if Vector.length pats > 1 then
            (error (Apat.region apat,
               Layout.str "tuple of pattern is not supported",
               Layout.empty);
             raise UnsupportedPatternSyntax)
          else 
            elaboratePat (env, Vector.sub (pats, 0))
      | Apat.Wild =>
          let
            val typ = TyAtom.Type.newNoname ()
          in
            (Cpat.Wild, typ, Env.empty)
          end
      | Apat.FlatApp pats =>
          let
            val _   = if Vector.length pats <> 1 then
                        error (Apat.region apat,
                          Layout.str "",
                          Layout.empty)
                      else
                        ()
            val pat = Vector.sub (pats, 0)
          in
            elaboratePat (env, pat)
          end
      | Apat.List _   => 
          (error (Apat.region apat,
             Layout.str "list of pattern is not supported",
             Layout.empty);
           raise UnsupportedPatternSyntax)
      | Apat.Record _ =>
          (error (Apat.region apat,
             Layout.str "record of pattern is not supported",
             Layout.empty);
           raise UnsupportedPatternSyntax)
    and elaborateExp (env, aexp) = 
      case Aexp.node aexp of
        Aexp.Var {name=longvid,...} =>
          let 
            val vd = Env.lookupVid (env,longvid)
            val _  = if Option.isNone vd then
                       error (Ast.Longvid.region longvid,
                         Layout.str "unknown symbol",
                         Layout.empty)
                     else
                       ()
            val vd = Option.valOf vd
            open Env.ValDef
            val ty = TyAtom.inst (scheme vd)
            val cexp = case value vd of
                         VVAR cvar => Cexp.var (cvar, ty) 
                       | VCON ccon => Cexp.con (ccon, ty)
          in 
            (cexp, TyAtom.Subst.empty)
          end
      | Aexp.Seq aexps =>
          if Vector.length aexps = 1 then
            elaborateExp (env, Vector.sub (aexps, 0))
          else
            let
              open Vector
              val ae0 = sub (aexps, 0)
              val aes = dropPrefix (aexps, 1)
              val aesregion = Region.append (Aexp.region (sub (aes, 0)), 
                                             Aexp.region (last aes))
              val aes = Aexp.makeRegion (Aexp.Seq aes, aesregion)
              val (cexp0, rho0) = elaborateExp (env, ae0)
              val (cexp1, rho1) = elaborateExp (Env.subst (rho0, env), aes)
            in
              (Cexp.seq (cexp0, cexp1), TyAtom.Subst.compose (rho1, rho0))
            end
      | Aexp.If (aexp0, aexp1, aexp2) =>
          let
            val (cexp0,rho0) = elaborateExp (env, aexp0)
            val env = Env.subst (rho0, env)
            val (cexp1,rho1) = elaborateExp (env, aexp1)
            val env = Env.subst (rho1, env)
            val (cexp2,rho2) = elaborateExp (env, aexp2)
            val cexp0 = Cexp.subst (rho2, Cexp.subst (rho1, cexp0))
            val cexp1 = Cexp.subst (rho2, cexp1)
            val rho3 = TyAtom.unifyL ([Cexp.ty cexp0   , Cexp.ty cexp1],
                                      [TyAtom.Type.bool, Cexp.ty cexp2])
            val rho4 = TyAtom.Subst.composeL [rho3,rho2,rho1,rho0]
            val cexp0 = Cexp.subst (rho3, cexp0)
            val cexp1 = Cexp.subst (rho3, cexp1)
            val cexp2 = Cexp.subst (rho3, cexp2)
          in 
            (Cexp.ifthen (cexp0, cexp1, cexp2), rho4)
          end
      (*
      | Aexp.Let (dec, t) =>
      *)
      | Aexp.Constraint (aexp, aty) =>
          let 
            val (cexp, rho0) = elaborateExp (env, aexp)
            val env          = Env.subst (rho0, env)
            val cty          = ElabTy.elaborateT (env, aty)
            val rho1         = TyAtom.unify (Cexp.ty cexp, cty)
            val cexp         = Cexp.subst (rho1, cexp)
          in
            (cexp, TyAtom.Subst.compose (rho1, rho0))
          end
      | Aexp.Case (aexp, match) =>
          let
            val (cexp, rho0) = elaborateExp (env, aexp)
            val (clam, rho1) = elaborateLam (Env.subst (rho0, env), match)
            val cexp         = Cexp.subst (rho1, cexp)
            val (ty0,ty1)    = Clam.ty clam
            val rho2         = TyAtom.unify (Cexp.ty cexp, ty0)
            val cexp         = Cexp.subst (rho2, cexp)
            val clam         = Clam.subst (rho2, clam)
            val cexpcase     = Cexp.app (Cexp.lambda clam, cexp)
          in 
            (cexpcase, TyAtom.Subst.composeL [rho2, rho1, rho0])
          end
      | Aexp.Fn match =>
          let
            val (clam, rho) = elaborateLam (env, match)
          in
            (Cexp.lambda clam, rho)
          end
      | Aexp.FlatApp aexps =>
          if (Vector.length aexps = 1) then
            elaborateExp (env, Vector.sub (aexps, 0))
          else
            let 
              val aexp0 = Vector.sub (aexps, 0)
              val aexp1 = Vector.sub (aexps, 1)
              val apply = fn (a, b) => Aexp.app (b, a)
              val aexp  = Vector.foldFrom (aexps, 2, Aexp.app (aexp0, aexp1), apply)
            in
              elaborateExp (env, aexp)
            end
      | Aexp.Andalso (aexp1, aexp2) =>
          let
            val (cexp0, rho0) = elaborateExp (env, aexp1)
            val (cexp1, rho1) = elaborateExp (Env.subst (rho0, env), aexp2)
            val cexp0         = Cexp.subst (rho1, cexp0)
            val rho2 = TyAtom.unifyS [Cexp.ty cexp0, Cexp.ty cexp1, TyAtom.Type.bool]
            val cexp0 = Cexp.subst (rho2, cexp0)
            val cexp1 = Cexp.subst (rho2, cexp1)
            val rho   = TyAtom.Subst.composeL [rho2, rho1, rho0]
          in
            (Cexp.ifthen (cexp0, cexp1, Cexp.falsee), rho)
          end
      | Aexp.Orelse  (aexp1, aexp2) =>
          let
            val (cexp0, rho0) = elaborateExp (env, aexp1)
            val (cexp1, rho1) = elaborateExp (Env.subst (rho0, env), aexp2)
            val cexp0         = Cexp.subst (rho1, cexp0)
            val rho2 = TyAtom.unifyS [Cexp.ty cexp0, Cexp.ty cexp1, TyAtom.Type.bool]
            val cexp0 = Cexp.subst (rho2, cexp0)
            val cexp1 = Cexp.subst (rho2, cexp1)
            val rho   = TyAtom.Subst.composeL [rho2, rho1, rho0]
          in
            (Cexp.ifthen (cexp0, Cexp.truee, cexp1), rho)
          end
      | _ => raise UnsupportedExpressionSyntax

    and elaborateLam (env : Env.t, match : Alam.t) = 
      let 
        val patexps      = case Alam.node match of Alam.T x => x
        val cpattypeenvs = Vector.map (patexps, 
                             fn (apat, _) => elaboratePat (env, apat))
        val rho   = ref (TyAtom.unifyS (Vector.toListMap (cpattypeenvs, #2)))
        val cexps = Vector.map2 (cpattypeenvs, patexps, 
                      fn ((_,_, envadd), (_, aexp)) => 
                        let 
                          val env = Env.subst (!rho, Env.append (env, envadd))
                          val (cexp, rho') = elaborateExp (env, aexp)
                          val _   = rho := TyAtom.Subst.compose (rho', !rho)
                        in
                          cexp
                        end)
        val rho'  = TyAtom.unifyS (Vector.toListMap
                      (cexps, (fn t => TyAtom.subst (!rho, t)) o  Cexp.ty))
        val rho'  = TyAtom.Subst.compose (rho', !rho)       
        val cexps = Vector.map (cexps, fn e => Cexp.subst (rho', e))
        val ty1   = TyAtom.subst (rho', #2 (Vector.sub (cpattypeenvs, 0)))
        val cvar  = Core.Var.newNoname ()
        val cpatcexps = Vector.map2 (cpattypeenvs, cexps,
                          fn ((cpat, _, _), cexp) => (cpat, cexp))
        val clam  = Clam.make (cvar, ty1, 
                      Cexp.casee (Cexp.var (cvar, ty1), cpatcexps))
      in 
        (clam, rho')
      end
  end
end

functor ElaborateCore (S: ELABORATE_CORE_STRUCTS) = 
struct
  open S
  
  local 
    structure Apat = Ast.Pat
    structure Aexp = Ast.Exp
  in
    structure Flat =
    struct

      exception CompilerBugEmptyPatSeq

      fun parsePats (env, pats) = 
        case Vector.length pats of
          0 => raise CompilerBugEmptyPatSeq
        | 1 => Vector.sub (pats, 0)
        | 2 => 
            let 
              val con = Vector.sub (pats, 0)
              val arg = Vector.sub (pats, 1)
              val pat = case Apat.node con of
                          Apat.Var {name = vid, ...} =>
                            Apat.App (Ast.Longvid.toLongcon vid, arg)
                        | _ =>
                            (Control.error (Apat.region con,
                               Layout.str "misplaced pattern as constructor",
                               Layout.empty);
                             Apat.Wild)
              val reg = Region.append (Apat.region con, Apat.region arg)
            in
              Apat.makeRegion (pat, reg)
            end
        | _ => 
            let 
              val con = Vector.sub (pats, 0)
              val arg = Vector.dropPrefix (pats, 1)
              val reg = Vector.fold (arg, Apat.region con,
                          fn (p, r) => Region.append (r, Apat.region p))
              val _ = Control.error (reg,
                        Layout.str "pattern sequence do not form a valid pattern",
                        Layout.empty)
            in
              con
            end

      fun parseExps (env, exps) = 
        if (Vector.length exps = 1) then
          Vector.sub (exps, 0)
        else
          let 
            val exp0 = Vector.sub (exps, 0)
            val exp1 = Vector.sub (exps, 1)
            val apply = fn (a, b) => Aexp.app (b, a)
          in
            Vector.foldFrom (exps, 2, Aexp.app (exp0, exp1), apply)
          end

      fun parsePatsFunSig (env, pats) = 
        let
          val fname = Vector.sub (pats, 0)
          val args  = Vector.dropPrefix (pats, 1)
        in 
          (fname, args)
        end
    end
  end
 
  exception UnsupportedPatternSyntax
  exception UnsupportedExpressionSyntax
  exception UnsupportedDeclarationSyntax
  exception CompilerBugFunTypeNotArrow
  exception CompilerBugFunNameInvalid
  local
    structure VS = TyAtom.VarSet
    fun collectv (f: 'a -> VS.t, v: 'a vector): VS.t = 
      VS.unions (Vector.toListMap (v, f))
    fun collectp (f: 'a -> VS.t, p: 'a * 'a)  : VS.t = 
      VS.append (f (#1 p), f (#2 p))
    fun collecto (f: 'a -> VS.t, t: 'a option): VS.t = 
      Option.fold (t, VS.empty, fn (a, set) => f a)
    fun collectExplicitTyvarsType (t: Ast.Type.t) : VS.t = 
      case Ast.Type.node t of
        Ast.Type.Record _    => VS.empty
      | Ast.Type.Var v       => VS.singleton v
      | Ast.Type.Con (_, ts) => collectv (collectExplicitTyvarsType, ts)
    structure Apat = 
    struct
      open Ast.Pat
      fun collectExplicitTyvars (pat : t) = 
        case node pat of 
          App (_, p)            => collectExplicitTyvars p
        | Const _               => VS.empty
        | Constraint (_, ty)    => collectExplicitTyvarsType ty
        | FlatApp ps            => collectv (collectExplicitTyvars, ps)
        | Layered {constraint, pat, ...}
                                => let
                                     val s1 = collecto (collectExplicitTyvarsType, constraint)
                                     val s2 = collectExplicitTyvars pat
                                   in
                                     VS.append (s1, s2)
                                   end
        | List _                => VS.empty
        | Record _              => VS.empty
        | Tuple _               => VS.empty
        | Var _                 => VS.empty
        | Wild                  => VS.empty
    end
    structure Cpat = Core.Pat
    structure Aexp = 
    struct
      open Ast.Exp
      fun collectExplicitTyvars exp = 
        case node exp of 
          Andalso es   => collectp (collectExplicitTyvars, es)
        | Orelse  es   => collectp (collectExplicitTyvars, es)
        | App es       => collectp (collectExplicitTyvars, es)
        | FlatApp es   => collectv (collectExplicitTyvars, es)
        | Let (_, e)   => collectExplicitTyvars e
        | Seq es       => collectv (collectExplicitTyvars, es)
        | While {expr, test}       => collectp (collectExplicitTyvars, (expr, test))
        | If (e1,e2,e3)=> 
            let 
              val s1 = collectExplicitTyvars e1
              val s2 = collectExplicitTyvars e2
              val s3 = collectExplicitTyvars e3
            in
              VS.unions [s1,s2,s3]
            end
        | Prim (Ast.PrimKind.Prim {ty,...}) => collectExplicitTyvarsType ty
        | Case (e1, m) =>
            let 
              val s1 = collectExplicitTyvars e1
              val s2 = case Ast.Match.node m of Ast.Match.T pes => 
                         Vector.toListMap (pes, fn (p, e) => 
                           VS.append (Apat.collectExplicitTyvars p, collectExplicitTyvars e))
              val s2 = VS.unions s2
            in
              VS.append (s1,s2)
            end

        | Constraint (e, t) =>
            let
              val s1 = collectExplicitTyvars e
              val s2 = collectExplicitTyvarsType t
            in
              VS.append (s1,s2)
            end
        | Fn m =>
            let
              val s1 = case Ast.Match.node m of Ast.Match.T pes => 
                         Vector.toListMap (pes, fn (p, e) => 
                           VS.append (Apat.collectExplicitTyvars p, collectExplicitTyvars e))
            in
              VS.unions s1
            end
        | Const    _ => VS.empty
        | Var      _ => VS.empty
        | List     _ => VS.empty
        | Handle   _ => VS.empty
        | Raise    _ => VS.empty
        | Record   _ => VS.empty
        | Selector _ => VS.empty
    end
    structure Cexp = Core.Exp
    structure Alam = Ast.Match
    structure Clam = Core.Lambda
    structure Adec = Ast.Dec
    structure Cdec = Core.Dec
    structure Atoms  = Core.CoreML.Atoms
    structure Cvar   = Atoms.Var
    structure Aconst = Ast.Const
    structure Cconst = Atoms.Const
    open Control

    val int32 = TyAtom.Type.con (Atoms.Tycon.int (Atoms.IntSize.fromBits (Bits.fromInt 32)), Vector.new0 ())
    fun word32 (i:IntInf.t) : Cconst.t = Cconst.Word (Atoms.WordX.fromIntInf (i, Atoms.WordSize.word32))
  in

    fun elaboratePat (env, apat) : Cpat.t * Env.t =
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
            val (contyp, targs) = TyAtom.inst (Env.ValDef.scheme valdef)
            val optarr = TyAtom.Type.deArrow contyp
            val _      = if Option.isNone optarr then
                           error (Ast.Longcon.region longcon,
                             Layout.str "Nullary constructor applied with arg",
                             Layout.empty)
                         else
                           ()
            val (contyp1, contyp2) = Option.valOf optarr
            val (argcpat, argenv') = elaboratePat (env, pat)
            val rho                = TyAtom.unify (contyp1, Cpat.ty argcpat)
            val cpat = Cpat.make ( Cpat.Con { con   = (Env.ValDef.deCon o Env.ValDef.value) valdef
                                            , arg   = SOME argcpat
                                            , targs = targs
                                            },
                                   contyp2 )
          in
            ( Cpat.subst (rho, cpat)
            , Env.subst (rho, argenv') )
          end
      | Apat.Var {name=longvid, ...}  => 
          let
            val optvd = Env.lookupVid (env, longvid)
            open Env.ValDef
          in
            if Option.exists (optvd, isCon o value) then
              let 
                val valdef = Option.valOf optvd
                val con    = deCon (value valdef)
                val (contyp, targs) = TyAtom.inst (scheme valdef)
                val _  = if Option.isSome (TyAtom.Type.deArrow contyp) then
                            error (Ast.Longvid.region longvid,
                              Layout.str "Constructor expects its argument",
                              Layout.empty)
                         else
                           ()
              in
                ( Cpat.make ( Cpat.Con { con   = (deCon o value) valdef
                                       , arg   = NONE
                                       , targs = targs
                                       },
                              contyp ),
                  Env.empty )
              end
            else
              let 
                val typ = TyAtom.Type.newNoname ()
                val var = Cvar.newNoname () 
              in
                ( Cpat.make ( Cpat.Var var, typ ),
                  Env.extendVid
                    ((#2 o Ast.Longvid.split) longvid,
                     make (VVAR var, TyAtom.Scheme.fromType typ))
                    Env.empty )
              end
          end
      | Apat.Const const =>
          let 
            val pat = case Aconst.node const of
                        Aconst.Bool b => if b then Cpat.truee else Cpat.falsee
                      | Aconst.Int  i => Cpat.make (Cpat.Const (word32 i), int32)
                      | _             => (Control.error (Apat.region apat,
                                            Layout.str "Only bool and int constant are supported",
                                            Layout.empty);
                                          Cpat.make (Cpat.Wild, TyAtom.Type.newNoname ()))
          in
            (pat, Env.empty)
          end
      | Apat.Constraint (pat, typ) =>
          let 
            val (cpat, env') = elaboratePat (env, pat)
            val typ1         = Cpat.ty cpat
            val typ2         = ElabTy.elaborateT (env, typ)
            val rho          = TyAtom.unify (typ1, typ2)
          in
            (Cpat.subst (rho, cpat) , Env.subst (rho, env') )
          end
      | Apat.Layered {var=var, pat=pat, constraint=typ,...} =>
          let 
            val (cpat, env')   = elaboratePat (env, pat)
            val typ1           = Cpat.ty cpat
            val cvar           = Cvar.newNoname ()
            fun bindvar vartyp = let 
                                   open Env
                                   val s = TyAtom.Scheme.fromType vartyp
                                 in
                                   extendVid (Ast.Vid.fromVar var, ValDef.make (ValDef.VVAR cvar, s))
                                 end
          in
            case typ of 
              NONE     => 
                (cpat, bindvar typ1 env')
            | SOME typ => 
                let 
                  val typ2 = ElabTy.elaborateT (env, typ)
                  val rho  = TyAtom.unify (typ1, typ2)
                in
                  ( Cpat.subst (rho, cpat), Env.subst  (rho, bindvar typ1 env') )
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
            ( Cpat.make (Cpat.Wild, typ), Env.empty )
          end
      | Apat.FlatApp pats =>
          let
            val pat = Flat.parsePats (env, pats)
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
            val vd   = Env.lookupVid (env,longvid)
            val cexp = case vd of
                         NONE    => (error (Ast.Longvid.region longvid,
                                       Layout.str "unknown symbol",
                                       Layout.empty);
                                    Cexp.var ({var = Cvar.newNoname (), targs = Vector.new0 ()},
                                              TyAtom.Type.newNoname ()))
                       | SOME vd => 
                           let
                             open Env
                             val (ty, targs) = TyAtom.inst (ValDef.scheme vd) 
                           in 
                             case ValDef.value vd of
                               ValDef.VVAR cvar => Cexp.var ({var = cvar, targs = targs}, ty) 
                             | ValDef.VCON ccon => Cexp.con ({con = ccon, targs = targs}, ty)
                           end
          in 
            (cexp, TyAtom.Subst.empty)
          end
      | Aexp.Seq aexps =>
          let
            val state = (env, TyAtom.Subst.empty, [])
            val (_, rho, cexps) = Vector.fold (aexps, state, 
                                    fn (aexp, (env, rho, cexps)) =>
                                      let
                                        val (cexp, rho') = elaborateExp (env, aexp)
                                        val env = Env.subst (rho', env)
                                        val rho = TyAtom.Subst.compose (rho', rho)
                                      in
                                        (env, rho, cexp::cexps)
                                      end)
            val cexps = Vector.rev (Vector.fromList cexps)
            val cexps = Vector.map (cexps, fn cexp => Cexp.subst (rho, cexp))
          in
            (Cexp.seq cexps, rho)
          end
      | Aexp.If (aexp0, aexp1, aexp2) =>
          let
            val (cexp0,rho0) = elaborateExp (env, aexp0)
            val env   = Env.subst (rho0, env)
            val (cexp1,rho1) = elaborateExp (env, aexp1)
            val env   = Env.subst (rho1, env)
            val cexp0 = Cexp.subst (rho1, cexp0)
            val (cexp2,rho2) = elaborateExp (env, aexp2)
            val cexp0 = Cexp.subst (rho2, cexp0)
            val cexp1 = Cexp.subst (rho2, cexp1)
            val rho3  = TyAtom.unifyL ([Cexp.ty cexp0   , Cexp.ty cexp1],
                                       [TyAtom.Type.bool, Cexp.ty cexp2])
            val rho4  = TyAtom.Subst.composeL [rho3,rho2,rho1,rho0]
            val cexp0 = Cexp.subst (rho3, cexp0)
            val cexp1 = Cexp.subst (rho3, cexp1)
            val cexp2 = Cexp.subst (rho3, cexp2)
          in 
            (Cexp.ifthen (cexp0, cexp1, cexp2), rho4)
          end
      | Aexp.Let (adec, aexp) =>
          let
            val (cdecs, env', rho) = elaborateDec (env, adec)
            val env = Env.append (Env.subst (rho, env), env')
            val (cexp, rho')       = elaborateExp (env, aexp)
            val rho = TyAtom.Subst.compose (rho', rho)
          in
            (Cexp.lete (Decs.toVector cdecs, cexp), rho)
          end
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
            val env          = Env.subst (rho0, env)
            val (clam, rho1) = elaborateLam (env, match, "case")
            val cexp         = Cexp.subst (rho1, cexp)
            val (ty0,_)      = Clam.ty clam
            val rho2         = TyAtom.unify (Cexp.ty cexp, ty0)
            val cexp         = Cexp.subst (rho2, cexp)
            val clam         = Clam.subst (rho2, clam)
            val cexpcase     = Cexp.app (Cexp.lambda clam, cexp)
          in 
            (cexpcase, TyAtom.Subst.composeL [rho2, rho1, rho0])
          end
      | Aexp.Fn match =>
          let
            val (clam, rho) = elaborateLam (env, match, "fn")
          in
            (Cexp.lambda clam, rho)
          end
      | Aexp.App (aexp1, aexp2) =>
          let
            val (cexp0, rho0) = elaborateExp (env, aexp1)
            
            val _ = Control.message (Control.Detail, fn _ =>
                      Layout.seq [Layout.str "DEBUG: cexp0 ", Cexp.layout cexp0])

            val env           = Env.subst (rho0, env)
            val (cexp1, rho1) = elaborateExp (env, aexp2)
            val cexp0         = Cexp.subst (rho1, cexp0)

            val _ = Control.message (Control.Detail, fn _ =>
                      Layout.seq [Layout.str "DEBUG: cexp0 ", Cexp.layout cexp0])

            val ty            = TyAtom.Type.arrow (Cexp.ty cexp1, TyAtom.Type.newNoname ())

            val _ = Control.message (Control.Detail, fn _ =>
                      Layout.seq [Layout.str "DEBUG: ty    ", TyAtom.Type.layout ty])

            val rho2          = TyAtom.unify (Cexp.ty cexp0, ty)

            val _ = Control.message (Control.Detail, fn _ =>
                      Layout.seq [Layout.str "DEBUG: rho2  ", TyAtom.Subst.layout rho2])
            
            val cexp0         = Cexp.subst (rho2, cexp0)

            val _ = Control.message (Control.Detail, fn _ =>
                      Layout.seq [Layout.str "DEBUG: cexp0 ", Cexp.layout cexp0])

            val cexp1         = Cexp.subst (rho2, cexp1)

            val _ = Control.message (Control.Detail, fn _ =>
                      Layout.seq [Layout.str "DEBUG: cexp1 ", Cexp.layout cexp1])
          in
            (Cexp.app (cexp0, cexp1), TyAtom.Subst.composeL [rho2, rho1, rho0])
          end
      | Aexp.FlatApp aexps =>
          elaborateExp (env, Flat.parseExps (env, aexps))
      | Aexp.Andalso (aexp1, aexp2) =>
          let
            val (cexp0, rho0) = elaborateExp (env, aexp1)
            val env           = Env.subst (rho0, env)
            val (cexp1, rho1) = elaborateExp (env, aexp2)
            val cexp0         = Cexp.subst (rho1, cexp0)
            val rho2  = TyAtom.unifyS [Cexp.ty cexp0, Cexp.ty cexp1, TyAtom.Type.bool]
            val cexp0 = Cexp.subst (rho2, cexp0)
            val cexp1 = Cexp.subst (rho2, cexp1)
            val rho   = TyAtom.Subst.composeL [rho2, rho1, rho0]
          in
            (Cexp.ifthen (cexp0, cexp1, Cexp.falsee), rho)
          end
      | Aexp.Orelse  (aexp1, aexp2) =>
          let
            val (cexp0, rho0) = elaborateExp (env, aexp1)
            val env           = Env.subst (rho0, env)
            val (cexp1, rho1) = elaborateExp (env, aexp2)
            val cexp0         = Cexp.subst (rho1, cexp0)
            val rho2  = TyAtom.unifyS [Cexp.ty cexp0, Cexp.ty cexp1, TyAtom.Type.bool]
            val cexp0 = Cexp.subst (rho2, cexp0)
            val cexp1 = Cexp.subst (rho2, cexp1)
            val rho   = TyAtom.Subst.composeL [rho2, rho1, rho0]
          in
            (Cexp.ifthen (cexp0, Cexp.truee, cexp1), rho)
          end
      | Aexp.Const const =>
          let 
            val exp = case Aconst.node const of
                        Aconst.Bool b => if b then Cexp.truee else Cexp.falsee
                      | Aconst.Int  i => (Cexp.make (Cexp.Constant (word32 i), int32))
                      | _             => (Control.error (Aexp.region aexp,
                                            Layout.str "Only bool and int constant are supported",
                                            Layout.empty);
                                          Cexp.var0 (Cvar.newNoname (), TyAtom.Type.newNoname ()))
          in
            (exp, TyAtom.Subst.empty)
          end
      | Aexp.Prim (Ast.PrimKind.Prim {name = name, ty = ty}) =>
          let
            fun mkPrimApp {prim, args, retty} = 
              let
                exception UnsupportedPrimitive
                fun unsupported x = raise UnsupportedPrimitive
                val targs = Core.CoreML.Prim.extractTargs (prim, 
                              { args    = Vector.map (args, Cexp.ty)
                              , result  = retty 
                              , typeOps = { deArrow = unsupported
                                          , deArray = unsupported
                                          , deRef   = unsupported
                                          , deVector= unsupported
                                          , deWeak  = unsupported
                                          }
                              })
              in
                Cexp.make (Cexp.PrimApp {prim = prim, args = args, targs = targs}, retty)
              end
            val ty = ElabTy.elaborateT (env, ty)
            val (args, ret) = TyAtom.Type.deArgs ty
            (*
            val _ = Control.message (Control.Detail, fn _ =>
                      Layout.seq [Layout.str "DEBUG: ", Layout.align [List.layout TyAtom.Type.layout args, TyAtom.Type.layout ret]])
            *)
            val prim  = case Core.CoreML.Prim.fromString name of
                          NONE => (Control.error
                                     (Aexp.region aexp,
                                      Layout.str (concat ["unknown primitive: ", name]),
                                      Layout.empty);
                                   Core.CoreML.Prim.bogus)
                        | SOME prim => prim
            val cexp  = case args of
                          []    =>
                            mkPrimApp {prim = prim, args = Vector.new0 (), retty = ret}
                        | _     =>
                            let
                              val vars = List.map (args, fn argty => (Cvar.newNoname (), argty))
                              val body = mkPrimApp {prim = prim, args = Vector.fromListMap (vars, Cexp.var0), retty = ret}
                            in
                              List.foldr (vars, body, fn ((v,t), e) => (Cexp.lambda o Clam.make) (v,t,e))
                            end
            (*
            val _ = Control.message (Control.Detail, fn _ =>
                      Layout.seq [Layout.str "DEBUG: ", TyAtom.Type.layout (Cexp.ty cexp)])
            *)
          in
            (cexp, TyAtom.Subst.empty)
          end

      | _ => raise UnsupportedExpressionSyntax

    and elaborateLam (env : Env.t, match : Alam.t, kind : string) = 
      let 
        val patexps  = case Alam.node match of Alam.T x => x
        val cpatenvs = Vector.map (patexps, 
                         fn (apat, _) => elaboratePat (env, apat))
        val rho   = ref (TyAtom.unifyS (Vector.toListMap (cpatenvs, Cpat.ty o #1)))
        val cexps = Vector.map2 (cpatenvs, patexps, 
                      fn ((_, envadd), (_, aexp)) => 
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
        val cpats = Vector.map (cpatenvs, fn p => Cpat.subst (rho', #1 p))
        val cexps = Vector.map (cexps,    fn e => Cexp.subst (rho', e))
        val cvar  = Cvar.newNoname ()
        val cpatcexps = Vector.map2 (cpats, cexps,
                          fn (cpat, cexp) => { pat = cpat 
                                             , exp = cexp
                                             , lay = NONE
                                             })
        val ty1   = (Cpat.ty o Vector.sub) (cpats, 0)
        val clam  = Clam.make (cvar, ty1, 
                      Cexp.casee { test = Cexp.var0 (cvar, ty1), rules = cpatcexps, kind = kind})
      in 
        (clam, rho')
      end

    and elaborateDec (env : Env.t, adec : Adec.t) : Decs.t * Env.t *
    TyAtom.Subst.t =
      case Adec.node adec of
        Adec.SeqDec decs   => 
          let 
           val rho0  = TyAtom.Subst.empty
           val state = ( env         (*base environment*)
                       , Env.empty   (*new  environment*)
                       , Decs.empty  (*all core decs *)
                       , rho0        (*composed substitution*)
                       )
           val state = Vector.fold (decs, state,
                         fn (dec, (e0, en, cdecs, rho)) => 
                           let 
                             val env = Env.append (e0, en)
                             val (cdecs1, en', rho1) = elaborateDec (env, dec)
                             val cdecs = Decs.subst (rho1, cdecs) 
                           in
                             ( Env.subst  (rho1, e0)
                             , Env.append (Env.subst (rho1, en), en')
                             , Decs.append (cdecs, cdecs1)
                             , TyAtom.Subst.compose (rho1, rho) 
                             )
                           end)
          in
            (#3 state, #2 state, #4 state)
          end
      | Adec.Local (d1,d2) =>
          let
            val (cdecs1, en1, rho1) = elaborateDec (env, d1)
            val env = Env.append (Env.subst (rho1, env), en1)
            val (cdecs2, en2, rho2) = elaborateDec (env, d2)
            val en1    = Env.subst (rho2, en1)
            val cdecs1 = Decs.subst (rho2, cdecs1)
            val rho    = TyAtom.Subst.compose (rho2, rho1)
          in
            (Decs.append (cdecs1, cdecs2), Env.append (en1, en2), rho)
          end
      | Adec.Val {tyvars = vars, vbs = vbs, rvbs = rvbs } =>
          let 
            val vars = TyAtom.VarSet.fromV vars

            fun elabVB (env, pat, exp) = 
              let
                val _ = if TyAtom.VarSet.disjoint (Env.free env, vars) then
                          ()
                        else
                          error (Adec.region adec,
                            Layout.str "rebind a type variable",
                            Layout.empty)
                val (cpat, en) = elaboratePat (env, pat)
                val (cexp, rho)= elaborateExp
                                   (Env.extendRgdFV vars env,
                                    exp)
                val en   = Env.subst (rho, en)
                val env  = Env.subst (rho, env)
                val ty1  = TyAtom.subst (rho, Cpat.ty cpat)
                val rho' = TyAtom.unify (ty1, Cexp.ty cexp)
                val en   = Env.subst  (rho', en)
                val env  = Env.subst  (rho', env)
                val cpat = Cpat.subst (rho', cpat)
                val cexp = Cexp.subst (rho', cexp)
                val rho''= TyAtom.Subst.compose (rho', rho)
              in
                ((cpat, Apat.region pat, cexp), Env.gen (Env.free env, en), rho'')
              end

            fun elabRVB (env, pat, lam) = 
              let
                val _ = if TyAtom.VarSet.disjoint (Env.free env, vars) then
                          ()
                        else
                          error (Adec.region adec,
                            Layout.str "rebind a type variable",
                            Layout.empty)
                val (cpat, en) = elaboratePat (env, pat)
                val ty1  = Cpat.ty cpat
                val cvar = case Cpat.node cpat of 
                             Cpat.Var v => v
                           | _          =>
                               (error (Apat.region pat,
                                  Layout.str "only vid is allowed in recursive binding",
                                  Layout.empty);
                                Cvar.newNoname ())
                val (clam, rho) = elaborateLam 
                                    (Env.extendRgdFV vars 
                                      (Env.append (env, en)),
                                     lam,
                                     "function")
                val ty1  = TyAtom.subst (rho, ty1)
                val rho' = TyAtom.unify (ty1, TyAtom.Type.arrow (Clam.ty clam))
                val ty1  = TyAtom.subst (rho', ty1)
                val clam = Clam.subst (rho', clam)
                val rho''= TyAtom.Subst.compose (rho', rho)
                val en   = Env.subst (rho'', en)
                val env  = Env.subst (rho'', env)
              in
                ((cvar, clam), Env.gen (Env.free env, en), rho'')
              end

            val state = (env, Env.empty, [], TyAtom.Subst.empty) 
            val (e0, en, cs, rho) =
              Vector.fold (vbs, state,
                fn ({pat=p, exp=b}, (e0, en, cs, rho)) => 
                  let 
                    val (clause1, en1, rho1) = elabVB (e0, p, b)
                    val cs = List.map (cs,
                               fn (p, r, e) =>
                                 (p, r, Cexp.subst (rho1, e)))
                  in
                    ( Env.subst (rho1, e0)
                    , Env.append (Env.subst (rho1, en), en1)
                    , clause1 :: cs
                    , TyAtom.Subst.compose (rho1, rho)
                    )
                  end)

             val state = (e0, Env.empty, [], rho)
             val (e0', en', cs', rho') =
               Vector.fold (rvbs, state, 
                 fn ({pat=p,match=m}, (e0, en, cs, rho)) =>
                 let
                   val (clause1, en1, rho1) = elabRVB (e0, p, m)
                   val cs = List.map (cs,
                              fn (v, l) => 
                                (v, Clam.subst (rho1, l)))
                 in
                   ( Env.subst (rho1, e0)
                   , Env.append (Env.subst (rho1, en), en1)
                   , clause1 :: cs
                   , TyAtom.Subst.compose (rho1, rho)
                   )
                 end)

          in
            (Decs.single (Cdec.valbind { vars       = vars
                                       , valbind    = Vector.rev (Vector.fromList cs )
                                       , recvalbind = Vector.rev (Vector.fromList cs')
                                       }),
             Env.append (Env.subst (rho', en), en'),
             rho')
          end
      | Adec.Fun (vars, fundef) => 
          let
            val implicit = (TyAtom.VarSet.unions o Vector.toListMap) (Vector.concatV fundef, 
                              fn {pats, body, ...} => 
                                let 
                                  val s1 = collectv (Apat.collectExplicitTyvars, pats)
                                  val s2 = Aexp.collectExplicitTyvars body
                                in
                                  TyAtom.VarSet.append (s1, s2)
                                end)

            val explicit = TyAtom.VarSet.fromV vars

            val bound = TyAtom.VarSet.append (explicit, implicit)

            val _ = if TyAtom.VarSet.disjoint (Env.free env, bound) then
                      ()
                    else
                      error (Adec.region adec,
                        Layout.str "rebind a type variable",
                        Layout.empty)
            val cnt   = Vector.length fundef
            val cvars = Vector.tabulate (cnt, fn _ => Cvar.newNoname ())

            type funsig = Ast.Vid.t * int * TyAtom.Type.t * (Cpat.t vector * Env.t) vector
            fun elabFunSig fbs : funsig = 
              let 
                val fname = ref NONE
                val arity = ref NONE
                val sigs  = ref []
                val rets  = ref []
                val _ = Vector.foreach (fbs, 
                          fn {pats = ps, body = e, resultType = rt} => 
                            let
                              (* By the grammar of parser, the ps contains at
                               * least one element. *)
                              val (namepat, argpats) = Flat.parsePatsFunSig (env, ps)
                              val argnum = Vector.length argpats
                              val _ = if argnum = 0 then
                                        Control.error (Apat.region namepat,
                                          Layout.str "",
                                          Layout.empty)
                                      else
                                        ()
                              val name = case Apat.node namepat of
                                           Apat.Var {name = vid,...} => 
                                             let 
                                               val (strs, vid) = Ast.Longvid.split vid
                                               val _ =  if not (List.isEmpty strs) then
                                                          Control.warning (Apat.region namepat,
                                                            Layout.str "function name should be short",
                                                            Layout.empty)
                                                        else 
                                                          ()
                                             in
                                               vid
                                             end
                                          | _ =>
                                              let
                                                val _ = 
                                                  Control.error (Apat.region namepat,
                                                    Layout.str "misplaced pattern",
                                                    Layout.empty)
                                              in
                                                Ast.Vid.bogus
                                              end
                              val _ = case !fname of 
                                        NONE       => fname := SOME name
                                      | SOME fname => if Ast.Vid.equals (fname, name) then
                                                        Control.error (Apat.region namepat,
                                                          Layout.str "",
                                                          Layout.empty)
                                                      else
                                                        ()
                              val _ = case !arity of
                                        NONE       => arity := SOME argnum
                                      | SOME arity => if arity <> argnum then
                                                        Control.error (Apat.region namepat,
                                                          Layout.str "",
                                                          Layout.empty)
                                                      else
                                                        ()
                              val sig_ = Vector.map (argpats, 
                                           fn pat => elaboratePat (env, pat))
                              val rett = case rt of
                                           NONE     => TyAtom.Type.newNoname ()
                                         | SOME typ => ElabTy.elaborateT (env,typ)

                              val _   = sigs := sig_ :: !sigs
                              val _   = rets := rett :: !rets
                            in
                              ()
                            end)

                (* By the grammar, there are at least one fun binding *)
                val fname = Option.valOf (!fname)
                val arity = Option.valOf (!arity)
                (* (cpat, type, env) vector for each fun binding *)
                val sigs  = Vector.rev (Vector.fromList (!sigs))
                val rets  = !rets
                
                (* types of each arg*)
                val argstypss = Vector.tabulate (arity,
                                  fn idx => 
                                    Vector.map (sigs, 
                                      fn sig_ => 
                                        (Cpat.ty o #1) (Vector.sub (sig_,idx))))
                (* unify types of each arg *)
                val argsrhos  = Vector.map  (argstypss, TyAtom.unifyS o Vector.toList)
                (* form the final type of each arg *)
                val argstyps  = Vector.map2 (argsrhos, argstypss, fn (rho, typs) => 
                                  TyAtom.subst (rho, Vector.sub (typs, 0)))
                val rettyp    = TyAtom.subst (TyAtom.unifyS rets, List.first rets)
                val funtyp    = Vector.foldr (argstyps, rettyp, TyAtom.Type.arrow)
                
                val cpatsenv = Vector.map (sigs, 
                                 fn argv =>
                                   let
                                     val cpats = Vector.tabulate (arity,
                                                   fn idx => 
                                                     #1 (Vector.sub (argv, idx)))
                                     val envs  = Vector.tabulate (arity,
                                                   fn idx =>
                                                     let 
                                                       val env = #2 (Vector.sub (argv, idx))
                                                       val rho = Vector.sub (argsrhos, idx)
                                                     in
                                                       Env.subst (rho, env)
                                                     end)
                                     (* concat envs of each argument *)
                                     val env = Vector.fold (envs, Env.empty, Env.append)
                                   in
                                     (cpats, env)
                                   end)
              in
                (fname, arity, funtyp, cpatsenv)
              end

            type funbind = {body: Aexp.t,
                            pats: Apat.t vector,
                            resultType: Ast.Type.t option}
            fun elabFunBdy (env, rho, bound,
                            (fname, arity, typ, argsenv): funsig,
                            fbs                         : funbind vector) :
                           Clam.t * TyAtom.Type.t * TyAtom.Subst.t * Env.t = 
              let
                val (argtyps, rettyp) = TyAtom.Type.deArgs typ
                val state = (env, rho, [], [])
                val (env, rho, cexps, typs) =
                  Vector.fold2 (argsenv, fbs, state, 
                    fn ((_, env'), fb, (env, rho, cexps, typs)) =>
                      let
                        val env  = Env.subst (rho, Env.append (env, env'))
                        val env' = Env.extendRgdFV bound env
                        val (cexp, rho1) = elaborateExp (env', #body fb)
                      in
                        ( env                            (* bound are passed out *)
                        , TyAtom.Subst.compose (rho1, rho)
                        , cexp :: cexps
                        , Cexp.ty cexp :: typs
                        )
                      end)
                val typs  = List.revMap (rettyp::typs, fn typ => TyAtom.subst (rho, typ))
                val rho'  = TyAtom.unifyS typs
                val rettyp= TyAtom.subst (rho', rettyp)
                val rho   = TyAtom.Subst.compose (rho', rho)
                val argtyps = List.map (argtyps, fn typ => TyAtom.subst (rho,typ))
                val argvars = List.tabulate (arity, fn _ => Cvar.newNoname ())
                val funtyp  = TyAtom.Type.args (argtyps, rettyp)

                val env   = Env.subst (rho, env)
                val cexps = List.revMap (cexps, fn cexp => Cexp.subst (rho, cexp))

                val body  = Cexp.casepar { test  = Vector.fromListRev  
                                                     (List.map2 (argvars, argtyps, Cexp.var0))
                                         , rules = Vector.map2 
                                                     (Vector.map (argsenv, #1), 
                                                      Vector.fromList cexps,
                                                      fn (cpats, cexp) => 
                                                        { pat = cpats
                                                        , exp = cexp
                                                        , lay = NONE
                                                        })
                                         }
                val clam = List.fold2 (argvars, argtyps, body,
                             Cexp.lambda o Clam.make)
                val clam = case Cexp.node clam of
                             Cexp.Lambda x => x
                           | _             => Error.bug "Cexp.lambda yields non-Lambda"
              in
                (clam, funtyp, rho, env)
              end

            val sigs : funsig vector = Vector.map (fundef, elabFunSig)
            val env'  = Vector.fold2 (sigs, cvars, env, 
                          fn ((fname, _, typ, _), cvar, env) => 
                            let 
                              val vd = Env.ValDef.make 
                                         (Env.ValDef.VVAR cvar,
                                          TyAtom.Scheme.fromType typ)
                            in
                              Env.extendVid (fname, vd) env
                            end)

            val state = (env', TyAtom.Subst.empty, [],[])
            val (_, rho, clams, ctyps) =
              Vector.fold2 (sigs, fundef, state,
                fn (sig_, fbs, (env, rho,clams,ctyps)) => 
                  let
                    val (clam,ctyp,rho,env) =
                      elabFunBdy (env, rho, bound, sig_, fbs)
                  in
                    (env, rho, clam::clams, ctyp::ctyps)
                  end)
            val clams = Vector.rev (Vector.fromList clams)
            val clams = Vector.map (clams, fn clam => Clam.subst (rho, clam))
            val ctyps = Vector.rev (Vector.fromList ctyps)
            val ctyps = Vector.map (ctyps, fn ctyp => TyAtom.subst (rho, ctyp))
            val basefree = Env.free (Env.subst (rho, env))

            val env'= Vector.fold3 (sigs, cvars, ctyps, Env.empty,
                        fn (sig_, cvar, typ, env) =>
                          let
                            val vd = Env.ValDef.make 
                                           (Env.ValDef.VVAR cvar,
                                            TyAtom.gen (basefree, typ))
                            in
                              Env.extendVid (#1 sig_, vd) env
                          end)

            open Layout
            val _ = Control.message (Control.Detail, fn _ =>
                      seq [ str "on generalizing a fun:"
                          , align [ seq [str "bound:     ", TyAtom.VarSet.layout bound]
                                  , seq [str "generated: ", TyAtom.VarSet.layout free]]])
          in
            ( Decs.single (Cdec.funbind { vars = free
                                        , decs = Vector.zip (cvars, clams) })
            , env'
            , rho)
          end

      | _ => raise UnsupportedDeclarationSyntax
  end
end

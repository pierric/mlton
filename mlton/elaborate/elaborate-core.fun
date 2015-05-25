functor ElaborateCore (S: ELABORATE_CORE_STRUCTS) = 
struct
  open S

  local
    structure Apat = Ast.Pat
    structure Cpat = Core.Pat
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
  end

end

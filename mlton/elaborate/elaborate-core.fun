functor ElaborateCore (S: ELABORATE_CORE_STRUCTS) = 
struct
  open S

  local
    structure Apat = Ast.Pat
    structure Cpat = CoreML.Pat
  in
    fun elaboratePat (env, apat) =
      case Apat.node apat of
        Apat.App (longvid, pat) =>
          let 
            val valdef = Env.lookupVid (env, longvid)
            val _ = if (Env.ValDef.isCON o Env.ValDef.value) valdef then
                      ()
                    else
                      Control.error (Apat.region apat, 
                        Layout.str "Only constructors are allowed in pattern application",
                        Layout.empty)
            val contyp = TyAtom.inst s
            val (contyp1, contyp2) = TyAtom.Type.deArrow contyp
            val (argcpat, argtyp, argenv') = elaboratePat (env, pat)
            val rho = TyAtom.unify (contyp1, argtyp)
          in
            (Cpat.make (Cpat.Con (), TyAtom.subst (rho, contyp2)), 
             Env.subst (rho, argenv'))
          end
  end

end

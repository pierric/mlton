functor ElaborateType (S : ELABORATE_TYPE_STRUCTS) : ELABORATE_TYPE = 
struct
  open S
  
  structure Tyvar = TyAtom.Tyvar
  structure Type  = TyAtom.Type
  structure TypFun= TyAtom.TypFun

  exception UnknownTypeCons
  exception UnsupportType

  local 
    open Ast.Type
  in
    fun elaborateT env atyp = 
      case node atyp of
        Var atyvar => 
          Type.FlexTyvar (Tyvar.make ())
      | Con (atycon, atyps) => 
          (case Env.lookupTycon (env, atycon) of
            NONE => 
              let
                val _ = Control.error (region atyp,
                          Layout.str "unknown type constructor",
                          Layout.empty)
              in
                raise UnknownTypeCons
              end
          | SOME typdef =>
              let 
                val typs = Vector.toListMap (atyps, elaborateT env)
              in
                TypFun.apply (Env.TypDef.typfun typdef, typs)
              end)
      | Record _ =>
          let
            val _ = Control.error (region atyp,
                      Layout.str "unsupported syntax",
                      Layout.empty)
          in 
            raise UnsupportType
          end
  end
end

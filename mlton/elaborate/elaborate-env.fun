functor ElaborateEnv (S: ELABORATE_ENV_STRUCTS) : ELABORATE_ENV = 
struct
  open S

  structure TypEnv = Env (structure Domain = Ast.Tycon)
  structure TypDef = 
  struct
    type t = int * TyAtom.TypFun.t * Ast.Longvid.t list 
    fun arity  (td : t) = #1 td
    fun typfun (td : t) = #2 td
    fun cons   (td : t) = #3 td
  end
  type typenv = TypDef.t TypEnv.t

  structure ValEnv = Env (structure Domain = Ast.Vid  )
  structure ValDef = 
  struct
    datatype v = VCON of Con.t | VVAR of Var.t | VEXN of Var.t
    type t = v * TyAtom.Scheme.t
    fun isCon (VCON _) = true | isCon x = false
    fun isVar (VVAR _) = true | isVar x = false
    fun isExn (VEXN _) = true | isExn x = false
    fun value  (vd : t) = #1 vd
    fun scheme (vd : t) = #2 vd
  end
  type valenv = ValDef.t ValEnv.t
  
  type t = {typenv: typenv, valenv: valenv, freergdvar: TyAtom.VarSet.t}

  fun free (env : t) = 
    let
      val fvset = ref (TyAtom.VarSet.empty)
      val _ = ValEnv.foreach (#valenv env, 
                fn (_, s) => 
                  fvset := TyAtom.VarSet.append (!fvset, TyAtom.Scheme.free (s)))
    in
      !fvset
    end

  local 
    open Control
  in
    fun lookupTycon (env : t, longtycon) =
      let
        open Ast.Longtycon
        val (strids, tycon) = split longtycon
        (* we don't support structure in Simple-ML, so report an error
         * if the strids is not empty *)
        val _ = if List.isEmpty strids 
                then
                  () 
                else 
                  Control.error (region longtycon,
                    Layout.str "Unsupported long tycon",
                    Layout.empty)
      in
        TypEnv.peek (#typenv env, tycon)
      end
  
    fun lookupVid   (env: t, longvid) =
       let
         open Ast.Longvid
         val (strids, vid) = split longvid
         (* we don't support structure in Simple-ML, so report an error
          * if the strids is not empty *)
         val _ = if List.isEmpty strids 
                 then
                   () 
                 else 
                   Control.error (region longvid,
                     Layout.str "Unsupported long vid",
                     Layout.empty)
       in
         ValEnv.peek (#valenv env, vid)
       end
  end

  fun extendTycon (atycon, def) env = 
    let 
      val {typenv = te, valenv = ve, freergdvar = vs} = env
    in
      {typenv = TypEnv.extend (te, atycon, def),
       valenv = ve,
       freergdvar = vs}  
    end

  fun extendVid (vid, def) env = 
    let 
      val {typenv = te, valenv = ve, freergdvar = vs} = env
    in
      {typenv = te,
       valenv = ValEnv.extend (ve, vid, def),
       freergdvar = vs}  
    end

  fun extendRgdFV nvs env = 
    let 
      val {typenv = te, valenv = ve, freergdvar = vs} = env
    in
      {typenv = te,
       valenv = ve,
       freergdvar = TyAtom.VarSet.append (vs, nvs)}
    end
end

functor ElaborateEnv (S: ELABORATE_ENV_STRUCTS) : ELABORATE_ENV = 
struct
  open S

  structure TypEnv = Env (structure Domain = Ast.Tycon)
  structure TypDef = 
  struct
    type t = { typfun: TyAtom.TypFun.t, cons  : Ast.Longvid.t list }
    fun arity  (td : t) = TyAtom.TypFun.arity (#typfun  td)
    fun typfun (td : t) = #typfun td
    fun cons   (td : t) = #cons   td
    fun make x = x
  end
  type typenv = TypDef.t TypEnv.t

  structure ValEnv = Env (structure Domain = Ast.Vid  )
  structure ValDef = 
  struct
    datatype v = VCON of Con.t | VVAR of Var.t
    type t = v * TyAtom.Scheme.t
    exception DeConWrong and DeVarWrong
    fun isCon (VCON _) = true | isCon x = false
    fun isVar (VVAR _) = true | isVar x = false
    fun deCon (VCON con) = con
      | deCon _          = raise DeConWrong
    fun deVar (VVAR var) = var
      | deVar _          = raise DeVarWrong
    fun value  (vd : t) = #1 vd
    fun scheme (vd : t) = #2 vd
    fun make (v, s) = (v, s)
  end
  type valenv = ValDef.t ValEnv.t
  
  type env = {typenv: typenv, valenv: valenv, freergdvar: TyAtom.VarSet.t}
  type t = env

  structure Basis =
  struct
    type t = env
  end

  val empty = { typenv = TypEnv.empty (),
                valenv = ValEnv.empty (),
                freergdvar = TyAtom.VarSet.empty }

  fun free (env : t) = 
    let
      val fvset = ref (TyAtom.VarSet.empty)
      val _ = ValEnv.foreach (#valenv env, 
                fn (_, s) => 
                  fvset := TyAtom.VarSet.append (!fvset, TyAtom.Scheme.free (s)))
    in
      !fvset
    end

  fun gen (vars, env : t) =
    let
      val valenv = ValEnv.map (#valenv env, 
                     fn valdef => 
                       let 
                         val v = ValDef.value  valdef
                         val s = ValDef.scheme valdef
                       in
                         ValDef.make (v, TyAtom.Scheme.gen (vars, s))                         
                       end)
    in
      { typenv     = #typenv env
      , valenv     = valenv
      , freergdvar = #freergdvar env
      }
    end
  
  fun subst (rho, env : t) = 
    let 
      val ve = ValEnv.map (#valenv env,
                 fn (v, s) => (v, TyAtom.Scheme.subst (rho, s)))
    in
      {typenv = #typenv env, valenv = ve, freergdvar = #freergdvar env}
    end

  fun append (env0 : t, env1 : t) = 
    { typenv     = TypEnv.+ (#typenv env0, #typenv env1)
    , valenv     = ValEnv.+ (#valenv env0, #valenv env1)
    , freergdvar = TyAtom.VarSet.append (#freergdvar env0, #freergdvar env1)
    }

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
                  error (region longtycon,
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
                  error (region longvid,
                    Layout.str "Unsupported long vid",
                    Layout.empty)
      in
        ValEnv.peek (#valenv env, vid)
      end

    fun lookupCon   (env: t, longcon) = 
      let 
        open Ast
        val (path, con) = Longcon.split longcon
        val longvid     = Longvid.long (path, Vid.fromCon con)
        val valdef      = lookupVid (env, longvid)
        val _ = Option.app (valdef, fn vd => 
                  if (ValDef.isCon o ValDef.value) vd then
                    ()
                  else
                    error (Longcon.region longcon, 
                      Layout.str "lookupCon results in a non-Con",
                      Layout.empty))
      in
        valdef
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

  fun makeBasis (env, make, getenv)  = 
    let 
      val x = make env
      val e = getenv x
    in
      (x, e)
    end

  fun openBasis (env, basis) = append (env, basis)

  fun layoutCurrentScope env = Layout.empty
end

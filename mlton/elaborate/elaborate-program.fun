functor ElaborateProgram (S : ELABORATE_PROGRAMS_STRUCTS) : ELABORATE_PROGRAM = 
struct

open S

structure CT = Control
structure CD = Core.Dec
structure ST = Env.TyAtom.Subst

type subst = Env.TyAtom.Subst.t

fun elaborateStrdec (env: Env.t, sd: Ast.Strdec.t) : Decs.t * Env.t * subst =
  case Ast.Strdec.node sd of
    Ast.Strdec.Core dec => 
      ElabCore.elaborateDec (env, dec)
  | Ast.Strdec.Local (sd1, sd2) => 
      let 
        val (cdecs1, env', rho1) = elaborateStrdec (env, sd1)
        val env = Env.append (Env.subst (rho1, env), env')
        val (cdecs2, env', rho2) = elaborateStrdec (env, sd2)
        val cdecs = Decs.append (Decs.subst (rho2, cdecs1), cdecs2)
      in
        (cdecs, env', ST.compose (rho2, rho1))
      end
  | Ast.Strdec.Seq sds =>
      let 
        val state = (Decs.empty, Env.empty, ST.empty, env)
        val state = List.fold (sds, state,
                      fn (sd, (cdecs, enva, rho, env)) =>
                        let
                          val (cdecs1, enva1, rho1) = elaborateStrdec (env, sd)
                          val env  = Env.append(Env.subst (rho1, env) , enva1)
                          val enva = Env.append(Env.subst (rho1, enva), enva1)
                          val cdecs= Decs.append (Decs.subst (rho1, cdecs), cdecs1)
                          val rho  = ST.compose (rho1, rho)
                        in
                          (cdecs, enva, rho, env)
                        end)
      in
        (#1 state, #2 state, #3 state)
      end
  | Ast.Strdec.Structure _ => 
      let
        val _ = CT.warning (Ast.Strdec.region sd,
                  Layout.str "structure not supported",
                  Layout.empty)
      in
        (Decs.empty, Env.empty, ST.empty)
      end


fun elaborateTopdec (env: Env.t, td: Ast.Topdec.t) : Decs.t * Env.t * subst =
  case Ast.Topdec.node td of
    Ast.Topdec.Strdec strdec => 
      elaborateStrdec (env, strdec)
  | Ast.Topdec.Signature _   => 
      let
        val _ = CT.warning (Ast.Topdec.region td,
                  Layout.str "signature not supported",
                  Layout.empty)
      in
        (Decs.empty, Env.empty, ST.empty)
      end
  | Ast.Topdec.Functor   _   => 
      let
        val _ = CT.warning (Ast.Topdec.region td,
                  Layout.str "functor not supported",
                  Layout.empty)
      in
       (Decs.empty, Env.empty, ST.empty)
      end

fun elaborateProgram (prog: Ast.Program.t, env: Env.t) =
  case prog of Ast.Program.T topdecss => 
    let
      val topdecs = List.concat topdecss
      val state   = (env, Env.TyAtom.Subst.empty, Decs.empty)
      val state   = List.fold (topdecs, state,
                      fn (td, (env, rho, cdecs))=> 
                        let 
                          val (cdecs', env', rho') = elaborateTopdec (env, td)
                          val env = Env.append (Env.subst (rho', env), env')
                          val rho = Env.TyAtom.Subst.compose (rho', rho)
                          val cdecs = Decs.append (Decs.subst (rho', cdecs), cdecs')
                        in
                          (env, rho, cdecs)
                        end)
      val _       = if Env.TyAtom.VarSet.isEmpty (Env.free (env)) then
                      ()
                    else
                      CT.error (Region.bogus,
                        Layout.str "unresolved free type variable",
                        Layout.empty)
    in
      (#3 state, #1 state)
    end

end

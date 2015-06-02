functor ElaborateProgram (S : ELABORATE_PROGRAMS_STRUCTS) : ELABORATE_PROGRAM = 
struct

open S

fun elaborateProgram (prog: Ast.Program.t, env: Env.t) =
  case prog of Ast.Program.T topdecss => 
    let
      val topdecs = List.concat topdecss
    in
      List.fold (topdecs, (Env.empty, TyAtom.Subst.empty, []), 
        fn (td, (env, rho, cdecs))=> 
          let 
            val (cdecs', env', rho') = elaborateTopdec state
            val env = Env.append (Env.subst (rho', env), env')
            val rho = TyAtom.Subst.compose (rho', rho)
          in
            (env, rho, cdecs ^ cdecs')
          end)
    end

end

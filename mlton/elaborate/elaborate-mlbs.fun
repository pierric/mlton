functor ElaborateMLBs (S: ELABORATE_MLBS_STRUCTS): ELABORATE_MLBS = 
struct

local
   open S.Ast
in
   structure Basexp     = Basexp
   structure Basdec     = Basdec
   structure ModIdBind  = ModIdBind

   structure BasBinds   = Env (structure Domain = Basid)
   type BasBindsT       = S.Env.Basis.t BasBinds.t
   structure BasCache   = Env (structure Domain = String)
   type BasCacheT       = S.Env.Basis.t BasCache.t
end

open S
structure ElaborateType    = ElaborateType    (structure Ast      = Ast
                                               structure Env      = Env
                                               structure TyAtom   = Env.TyAtom)
structure ElaborateCore    = ElaborateCore    (structure Ast      = Ast
                                               structure Core     = Core
                                               structure Env      = Env
                                               structure Decs     = Decs
                                               structure TyAtom   = Env.TyAtom
                                               structure ElabTy   = ElaborateType)
structure ElaborateProgram = ElaborateProgram (structure Ast      = Ast
                                               structure Core     = Core
                                               structure CoreML   = CoreML
                                               structure Decs     = Decs
                                               structure Env      = Env
                                               structure ElabCore = ElaborateCore)
structure ElabControl = Control.Elaborate
structure ElabEnv = Env

type cdecs = (CoreML.Dec.t list * bool) list

infix 3 <+>
fun a <+> b = BasBinds.+ (a,b)

fun elaborateMLB (mlb : Basdec.t, {addPrim}) =
  let 
    val emptyBinds= BasBinds.empty ()
    val emptyCache= BasCache.empty ()
    val emptyEnv  = ElabEnv.empty
    val primAdded = ref false
    val prim      = Promise.lazy (fn () => ElabEnv.makeBasis (ElabEnv.empty, addPrim, fn x => #2 x))

    fun elabBasDec (basBinds: BasBindsT, basCache: BasCacheT, elabEnv: ElabEnv.t, dec: Basdec.t) : 
                   BasBindsT * BasCacheT * ElabEnv.t * cdecs =
      case Basdec.node dec of 
        Basdec.Basis binds  => 
          Vector.fold (binds, (emptyBinds, basCache, elabEnv, []), 
            fn (x,(badd, cache1, env, cdec)) =>  
              let val ((_, cache2, env, d1), bas) = 
                      ElabEnv.makeBasis (env,
                        fn env => elabBasExp (basBinds <+> badd, cache1, env, #def x),
                        fn x   => #3 x)
              in 
                (BasBinds.extend(badd, #name x, bas), cache2, env, cdec @ d1)
              end)
      | Basdec.Defs  def       => 
          let
            val _ = Control.error (Basdec.region dec,
                      Layout.str "bas defines not supported",
                      Layout.empty) 
          in 
            (emptyBinds, basCache, elabEnv, [])
          end
      
      | Basdec.Local (dec1,dec2)  =>
          let
            val (badd1, cache1, env1, cdecs1) = elabBasDec (basBinds, basCache, elabEnv, dec1)
            val (badd2, cache2, env2, cdecs2) = elabBasDec (basBinds <+> badd1, cache1, env1, dec2)
          in
            (badd2, cache2, env2, cdecs1 @ cdecs2)
          end
      
      | Basdec.Open  basvars      => 
          let 
            val env = Vector.fold (basvars, elabEnv,
                        fn (x, env) => 
                          Option.fold (BasBinds.peek (basBinds, x), env,
                            fn (bas, env) =>
                              ElabEnv.openBasis (env, bas)))
          in (emptyBinds, basCache, env, []) end
    
      | Basdec.Seq   decs          =>
          List.fold (decs, (emptyBinds, basCache, elabEnv, []),
            fn (decl, (badd, cache, env, cdec)) =>
              let 
                val (badd1, cache1, env, cdec1) = elabBasDec (basBinds <+> badd, cache, env, decl)
              in 
                (badd <+> badd1, cache1, env, cdec @ cdec1)
              end) 
    
      | Basdec.Prim            => 
          let val ((cdecs, _), bas) = prim ()
              val env   = ElabEnv.openBasis (elabEnv, bas)
              val cdecs = if not (!primAdded) then 
                            let val _ = primAdded := true;
                            in [(cdecs, false)] end
                          else
                            []  
          in (emptyBinds, basCache, env, cdecs) end
          
      | Basdec.Prog (_, prog)  => 
          let 
            val prog = Promise.force prog
            (*
            val _ = Control.message (Control.Detail, fn () =>
                      Layout.seq [Layout.str "PROG", Ast.Program.layout prog]) 
            *)
            val (cdecs, env) = ElaborateProgram.elaborateProgram (prog,elabEnv)
            val cdecs = List.map (Decs.toList cdecs, Core.Dec.toCoreML)
            val deadCodeElim = ElabControl.current ElabControl.deadCode
          in
           (emptyBinds, basCache, env, [(cdecs, deadCodeElim)])
          end
          
      | Basdec.MLB  ({fileAbs, ...}, mlb)   =>
          (case BasCache.peek (basCache, fileAbs) of
             SOME bas => let val env = ElabEnv.openBasis (elabEnv, bas)
                         in (emptyBinds, basCache, env, [])
                         end
           | None     => let val dec = Promise.force mlb
                             (*
                             val _ = Control.message (Control.Detail, fn () =>
                                       Layout.seq [Layout.str "MLB", Basdec.layout dec])
                             *)
                             val ((_, cache, _, cdecs), bas) = 
                                   ElabEnv.makeBasis (ElabEnv.empty, 
                                     fn env => elabBasDec (basBinds, basCache, env, dec),
                                     fn x   => #3 x)
                             val env = ElabEnv.openBasis (elabEnv, bas)
                         in
                          (emptyBinds, BasCache.extend(cache, fileAbs, bas), env, cdecs)
                         end)
          
      | Basdec.Ann  (ann, region, dec) => 
          case ElabControl.parseIdAndArgs ann of
            ElabControl.Bad               => 
              let val _ = if !Control.warnAnn then 
                            let open Layout
                            in 
                              Control.warning (region, seq [str "unrecognized annotation: ", str ann], empty)
                            end
                          else ()
              in
                elabBasDec (basBinds, basCache, elabEnv, dec)
              end
          | ElabControl.Deprecated alts   => 
              let open ElabControl
                  val (id, args) = List.unzip alts
                  val _ = if !Control.warnDeprecated then
                            let open Layout
                            in
                              Control.warning (region, seq [], empty)
                            end
                          else ()
                  val restore = List.map (args, Args.processAnn)
                  val result  = elabBasDec (basBinds, basCache, elabEnv, dec)
                  val _       = List.foreach (List.rev restore, fn r => r ())
              in
                result
              end 
          | ElabControl.Good (id, arg)   =>
              let open ElabControl 
                  val restore = Args.processAnn arg
                  val result  = 
                    if equalsId (forceUsed, id) andalso enabled forceUsed then
                      let
                        val _ = Control.warning (region,
                                  Layout.str "force used annoation not supported",
                                  Layout.empty)
                      in
                        elabBasDec (basBinds, basCache, elabEnv, dec)
                      end
                    else if equalsId (ffiStr, id) then
                      let 
                        val _ = Control.warning (region,
                                  Layout.str "ffi used annoation not supported",
                                  Layout.empty)
                      in
                        elabBasDec (basBinds, basCache, elabEnv, dec)
                      end
                    else 
                      elabBasDec (basBinds, basCache, elabEnv, dec)
                  val _       = restore ()
              in 
                result
              end
          | Other                         => 
              elabBasDec (basBinds, basCache, elabEnv, dec)

    and elabBasExp (basBinds: BasBindsT, basCache: BasCacheT, elabEnv: ElabEnv.t, exp: Basexp.t) :
                    BasBindsT * BasCacheT * ElabEnv.t * cdecs =
      case Basexp.node exp of
        Basexp.Bas dec        => 
          elabBasDec (basBinds, basCache, elabEnv, dec)
      | Basexp.Let (dec, exp) => 
          let val (badd1, cache1, env1, cdec1) = elabBasDec (basBinds, basCache, elabEnv, dec)
              val (badd2, cache2, env2, cdec2) = elabBasExp (basBinds <+> badd1, cache1, env1, exp)
          in (badd2, cache2, env2, cdec1 @ cdec2) end
      | Basexp.Var var        => 
          let 
            val env = Option.fold (BasBinds.peek (basBinds, var), elabEnv,
                        fn (bas, env) => ElabEnv.openBasis (env, bas))
          in
           (emptyBinds, basCache, env, []) 
          end

    val (_, _,env,decs) = elabBasDec (emptyBinds, emptyCache, emptyEnv, mlb)
    val _               = Control.message (Control.Detail, fn () =>
                            List.layout CoreML.Dec.layout (List.concatMap (decs, #1)))
  in 
    (env, Vector.fromList decs)
  end
end

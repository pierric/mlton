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
structure ElaboratePrograms = ElaboratePrograms (structure Ast    = Ast
                                                 structure CoreML = CoreML
                                                 structure Decs   = Decs
                                                 structure Env    = Env)
structure ElabControl = Control.Elaborate
structure ElabEnv = S.Env

type CoreMLDecs = (S.CoreML.Dec.t list * bool) list

 
fun elaborateMLB (mlb : Basdec.t, {addPrim}) =
  let val primAdded = ref false
      val prim      = Promise.lazy (fn () => 
                        let val emptyElabEnv = ElabEnv.empty ()
                        in ElabEnv.makeBasis (emptyElabEnv, fn () => addPrim emptyElabEnv)
                        end)
      val emptyBinds= BasBinds.empty ()
      val emptyCache= BasCache.empty ()
      infix 3 <+>
      fun a <+> b = BasBinds.+ (a,b)
      
      fun elabBasDec (basBinds: BasBindsT, basCache: BasCacheT, elabEnv: ElabEnv.t, dec: Basdec.t) =
        case Basdec.node dec of 
          Basdec.Basis binds  => 
            Vector.fold (binds, (emptyBinds, basCache, []), 
              fn (x,(badd, cache1, cdec)) =>  
                let val ((_, cache2, d1), bas) = 
                        ElabEnv.makeBasis (elabEnv, fn () => 
                            elabBasExp (basBinds <+> badd, cache1, elabEnv, #def x))
                in 
                  (BasBinds.extend(badd, #name x, bas), cache2, cdec @ d1)
                end)
        | Basdec.Defs  def       => 
            let fun bindIt (lookup, extend, {lhs=name, rhs=target}) = 
                    Option.app (lookup (elabEnv, target), 
                      fn rhs => extend (elabEnv, name, rhs))
                val _ = case ModIdBind.node def of 
                          ModIdBind.Fct bnds => 
                            Vector.map (bnds, fn bnd => bindIt(ElabEnv.lookupFctid, ElabEnv.extendFctid, bnd))
                        | ModIdBind.Sig bnds => 
                            Vector.map (bnds, fn bnd => bindIt(ElabEnv.lookupSigid, ElabEnv.extendSigid, bnd))
                        | ModIdBind.Str bnds => 
                            Vector.map (bnds, fn bnd => bindIt(ElabEnv.lookupStrid, ElabEnv.extendStrid, bnd))
            in (emptyBinds, basCache, [])
            end
        
        | Basdec.Local (dec1,dec2)  =>
            ElabEnv.localAll (elabEnv, 
              fn ()                      => elabBasDec (basBinds, basCache, elabEnv, dec1),
              fn (badd1, cache1, cdecs1)  => 
                 let val (badd2, cache2, cdecs2) = 
                         elabBasDec (basBinds <+> badd1, cache1, elabEnv, dec2)
                 in (badd2, cache2, cdecs1 @ cdecs2) end)
        
        | Basdec.Open  basvars      => 
            let val _ = Vector.map (basvars, fn x => 
                          Option.app(BasBinds.peek (basBinds, x), fn bas =>
                            ElabEnv.openBasis (elabEnv, bas)))
            in (emptyBinds, basCache, []) end

        | Basdec.Seq   decs          =>
            List.fold (decs, (emptyBinds, basCache, []),
              fn (decl, (badd, cache, cdec)) =>
                let val (badd1, cache1, cdec1) = 
                        elabBasDec (basBinds <+> badd, cache, elabEnv, decl)
                in (badd <+> badd1, cache1, cdec @ cdec1)
                end) 

        | Basdec.Prim            => 
            let val (cdecs, bas) = prim ()
                val _     = ElabEnv.openBasis (elabEnv, bas)
                val cdecs = if not (!primAdded) then 
                              let val _ = primAdded := true;
                              in [(cdecs, false)] end
                            else
                              []  
            in (emptyBinds, basCache, cdecs) end
            
        | Basdec.Prog (_, prog)  => 
            let val prog = Promise.force prog
                (*
                val _ = Control.message (Control.Detail, fn () =>
                          Layout.seq [Layout.str "PROG", Ast.Program.layout prog]) 
                *)
                val elabProg  = ElaboratePrograms.elaborateProgram o (fn prog => 
                                  (prog,{env=elabEnv}))
                val cdecs = (Decs.toList o elabProg) prog
                val deadCodeElim = ElabControl.current ElabControl.deadCode
            in (emptyBinds, basCache, [(cdecs, deadCodeElim)]) 
            end
            
        | Basdec.MLB  ({fileAbs, ...}, mlb)   =>
            (case BasCache.peek (basCache, fileAbs) of
               SOME bas => let val _ = ElabEnv.openBasis (elabEnv, bas)
                           in (emptyBinds, basCache, [])
                           end
             | None     => let val dec = Promise.force mlb
                               (*
                               val _ = Control.message (Control.Detail, fn () =>
                                         Layout.seq [Layout.str "MLB", Basdec.layout dec])
                               *)
                               val ((_, cache, cdecs), bas) = 
                                   let
                                     val emptyElabEnv = ElabEnv.empty ()
                                   in 
                                     ElabEnv.makeBasis (emptyElabEnv, fn () => 
                                         elabBasDec (basBinds, basCache, emptyElabEnv, dec))
                                   end
                               val _ = ElabEnv.openBasis (elabEnv, bas)
                           in (emptyBinds, BasCache.extend(cache, fileAbs, bas), cdecs)
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
                        ElabEnv.forceUsedLocal (elabEnv, fn () => 
                            elabBasDec (basBinds, basCache, elabEnv, dec))
                      else if equalsId (ffiStr, id) then
                        let val ffi = valOf (current ffiStr)
                            val ffi = Ast.Longstrid.fromSymbols 
                                        (List.map (String.split (ffi, #"."),
                                         Ast.Longstrid.Symbol.fromString),
                                         region)
                            val result = elabBasDec (basBinds, basCache, elabEnv, dec)
                            val ()     = Option.app (ElabEnv.lookupLongstrid (elabEnv, ffi), 
                                             fn S => (ElabEnv.Structure.ffi := SOME S;
                                                      ElabEnv.Structure.forceUsed S))
                        in result end
                      else 
                        elabBasDec (basBinds, basCache, elabEnv, dec)
                    val _       = restore ()
                in result end
                
            | Other                         => 
                elabBasDec (basBinds, basCache, elabEnv, dec)

      and elabBasExp (basBinds: BasBindsT, basCache: BasCacheT, elabEnv: ElabEnv.t, exp: Basexp.t) =
        case Basexp.node exp of
          Basexp.Bas dec        => 
            elabBasDec (basBinds, basCache, elabEnv, dec)
        | Basexp.Let (dec, exp) => 
            let val (badd1, cache1, cdec1) = ElabEnv.scopeAll(elabEnv,
                                                fn () => elabBasDec (basBinds, basCache, elabEnv, dec))
                val (badd2, cache2, cdec2) = elabBasExp (basBinds <+> badd1, cache1, elabEnv, exp)
            in (badd2, cache2, cdec1 @ cdec2) end
        | Basexp.Var var        => 
            let val _ = Option.app(BasBinds.peek (basBinds, var),
                            fn bas => ElabEnv.openBasis (elabEnv, bas))
            in (emptyBinds, basCache, []) 
            end

      val elabEnv      = ElabEnv.empty ()
      val (_, _, decs) = elabBasDec (emptyBinds, emptyCache, elabEnv, mlb)
      val _            = Control.message (Control.Detail, fn () =>
                             List.layout CoreML.Dec.layout (List.concatMap (decs, #1)))
  in 
    (elabEnv, Vector.fromList decs)
  end
end

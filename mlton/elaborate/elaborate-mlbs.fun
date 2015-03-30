functor ElaborateMLBs (S: ELABORATE_MLBS_STRUCTS): ELABORATE_MLBS = 
struct

open S
type CoreMLDecs = (CoreML.Dec.t list * bool) list
structure ElabEnv = Env
local
   open Ast
in
   structure BasExp     = Basexp
   structure BasDec     = Basdec
   structure ModIdBind  = ModIdBind
   structure AstProgram = Ast.Program
   structure BasEnv : sig 
     type t
     val null : t
     val singleton : Basid.t * ElabEnv.Basis.t -> t
     val concat : t * t -> t
     val lookup : t * Basid.t -> ElabEnv.Basis.t option
   end = 
   struct
     type t = (Basid.t * ElabEnv.Basis.t) list
     val null = []
     fun singleton x  = [x]
     fun concat (x,y) = x @ y 
     fun lookup (x:t,n:Basid.t) = 
       let val res = List.peek (x, fn (m,_) => Basid.equals (n,m))
       in Option.map (res, fn (_,bas) => bas) end
   end
end
structure ElaboratePrograms = ElaboratePrograms (structure Ast = Ast
                                                 structure CoreML = CoreML
                                                 structure Decs = Decs
                                                 structure Env = Env)
structure ElabControl = Control.Elaborate


 
fun elaborateMLB (mlb : BasDec.t, {addPrim}) =
  let val primAdded = ref false
      val prim      = Promise.lazy (fn () => 
                        let val emptyElabEnv = ElabEnv.empty ()
                        in ElabEnv.makeBasis (emptyElabEnv, fn () => addPrim emptyElabEnv)
                        end)
      
      fun elabBasDec (basEnv: BasEnv.t, elabEnv: ElabEnv.t, mlb: BasDec.t) =
        case BasDec.node mlb of 
          BasDec.Basis basbinds  => 
            Vector.fold (basbinds, (BasEnv.null, []), 
              fn (x,(envadd, cdec)) =>  
                let val ((_, d1), bas) = ElabEnv.makeBasis (elabEnv, fn () =>
                                           elabBasExp (BasEnv.concat (basEnv, envadd), elabEnv, #def x))
                    val envadd1        = BasEnv.singleton (#name x, bas)
                in 
                  (BasEnv.concat (envadd, envadd1), cdec @ d1)
                end)
        | BasDec.Defs  def       => 
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
            in (BasEnv.null, [])
            end
        
        | BasDec.Local (dec1,dec2)  =>
            ElabEnv.localAll (elabEnv, 
              fn ()                 => elabBasDec (basEnv, elabEnv, dec1),
              fn (envadd1, cdecs1)  => 
                let val (envadd2, cdecs2) = elabBasDec (BasEnv.concat(basEnv, envadd1), elabEnv, dec2)
                in (envadd2, cdecs1 @ cdecs2) end)
        
        | BasDec.Open  basvars      => 
            let val _ = Vector.map (basvars, fn x => 
                          Option.app(BasEnv.lookup (basEnv, x), fn bas =>
                            ElabEnv.openBasis (elabEnv, bas)))
            in (BasEnv.null, []) end

        | BasDec.Seq   decs          =>
            List.fold (decs, (BasEnv.null, []),
              fn (decl, (envadd, cdec)) =>
                let val (envadd1, cdec1) = elabBasDec (BasEnv.concat(basEnv,envadd), elabEnv, decl)
                in (BasEnv.concat(envadd, envadd1), cdec @ cdec1)
                end) 
        | BasDec.Prim            => 
            let val (cdecs, bas) = prim ()
                val _     = ElabEnv.openBasis (elabEnv, bas)
                val cdecs = if not (!primAdded) then 
                              let val _ = primAdded := true;
                              in [(cdecs, false)] end
                            else
                              []  
            in (BasEnv.null, cdecs) end
            
        | BasDec.Prog (_, prog)  => 
            let val prog = Promise.force prog
                val _ = Control.message (Control.Detail, fn () =>
                          Layout.seq [Layout.str "PROG", AstProgram.layout prog]) 
                val elabProg  = ElaboratePrograms.elaborateProgram o (fn prog => 
                                  (prog,{env=elabEnv}))
                val cdecs = (Decs.toList o elabProg) prog
                val deadCodeElim = ElabControl.current ElabControl.deadCode
            in (BasEnv.null, [(cdecs, deadCodeElim)]) end
            
        | BasDec.MLB  (_, mlb)   =>
            let val mlb = Promise.force mlb
                val _ = Control.message (Control.Detail, fn () =>
                          Layout.seq [Layout.str "MLB", BasDec.layout mlb])
                val ((_, cdecs), bas) = 
                    let
                      val emptyElabEnv = ElabEnv.empty ()
                    in 
                      ElabEnv.makeBasis (emptyElabEnv, fn () => elabBasDec (basEnv, emptyElabEnv, mlb))
                    end
                val _ = ElabEnv.openBasis (elabEnv, bas)
            in (BasEnv.null, cdecs) end
            
        | BasDec.Ann  (ann, region, mlb) => 
            case ElabControl.parseIdAndArgs ann of
              ElabControl.Bad               => 
                let val _ = if !Control.warnAnn then 
                              let open Layout
                              in 
                                Control.warning (region, seq [str "unrecognized annotation: ", str ann], empty)
                              end
                            else ()
                in elabBasDec (basEnv, elabEnv, mlb) end
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
                    val result  = elabBasDec (basEnv, elabEnv, mlb)
                    val _       = List.foreach (List.rev restore, fn r => r ())
                in result end 
            | ElabControl.Good (id, arg)   =>
                let open ElabControl 
                    val restore = Args.processAnn arg
                    val result  = if equalsId (forceUsed, id) andalso enabled forceUsed then
                                    ElabEnv.forceUsedLocal (elabEnv, fn () => elabBasDec (basEnv, elabEnv, mlb))
                                  else if equalsId (ffiStr, id) then
                                    let val ffi = valOf (current ffiStr)
                                        val ffi = Ast.Longstrid.fromSymbols (List.map (String.split (ffi, #"."), Ast.Longstrid.Symbol.fromString), region)
                                        val result = elabBasDec (basEnv, elabEnv, mlb)
                                        val ()     = Option.app (ElabEnv.lookupLongstrid (elabEnv, ffi), fn S => 
                                                       (ElabEnv.Structure.ffi := SOME S;
                                                        ElabEnv.Structure.forceUsed S))
                                    in result end
                                  else 
                                    elabBasDec (basEnv, elabEnv, mlb)
                    val _       = restore ()
                in result end
                
            | Other                         => 
                elabBasDec (basEnv, elabEnv, mlb)
        
            

      and elabBasExp (basEnv: BasEnv.t, elabEnv: ElabEnv.t, exp: BasExp.t) =
        case BasExp.node exp of
          BasExp.Bas dec        => 
            elabBasDec (basEnv, elabEnv, dec)
        | BasExp.Let (dec, exp) => 
            let val (envadd1, cdec1) = ElabEnv.scopeAll(elabEnv, fn () => elabBasDec (basEnv, elabEnv, dec))
                val (envadd2, cdec2) = elabBasExp (BasEnv.concat(basEnv, envadd1), elabEnv, exp)
            in (envadd2, cdec1 @ cdec2) end
        | BasExp.Var var        => 
            let val _ = Option.app(BasEnv.lookup (basEnv, var), fn bas => 
                          ElabEnv.openBasis (elabEnv, bas))
            in (BasEnv.null, []) end

      val elabEnv   = ElabEnv.empty ()
      val (_, decs) = elabBasDec (BasEnv.null, elabEnv, mlb)
      val _ = Control.message (Control.Detail, fn () =>
                List.layout CoreML.Dec.layout (List.concatMap (decs, #1)))
  in 
    (elabEnv, Vector.fromList decs)
  end
end

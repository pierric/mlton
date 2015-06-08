(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ELABORATE_MLBS_STRUCTS = 
   sig
      structure Ast : AST
      structure Core: CORE_LANG
      structure Decs: DECS
      structure Env : ELABORATE_ENV
      structure CoreML : CORE_ML
      sharing Ast = Env.Ast
      sharing Core = Decs.Core
      sharing CoreML = Core.CoreML
      sharing Env.Atoms  = CoreML.Atoms 
      sharing Env.TyAtom = Core.TyAtom
      sharing Ast.Tyvar = Env.Tyvar
   end

signature ELABORATE_MLBS = 
   sig
      include ELABORATE_MLBS_STRUCTS

      val elaborateMLB:
         Ast.Basdec.t * {addPrim: Env.t -> CoreML.Dec.t list * Env.t}
         -> Env.t * (CoreML.Dec.t list * bool) vector
    end

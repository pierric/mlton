(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ELABORATE_STRUCTS = 
   sig
      structure Ast    : AST
      structure TyAtom : TYPE_ATOM
      structure CoreML : CORE_ML
      sharing Ast.Tyvar    = CoreML.Tyvar
      sharing CoreML.Atoms = TyAtom.Atoms
      sharing CoreML.Type  = TyAtom.Type
   end

signature ELABORATE = 
   sig
      include ELABORATE_STRUCTS

      structure Env : ELABORATE_ENV

      val elaborateMLB:
         Ast.Basdec.t * {addPrim: Env.t -> CoreML.Dec.t list * Env.t}
         -> Env.t * (CoreML.Dec.t list * bool) vector
  end

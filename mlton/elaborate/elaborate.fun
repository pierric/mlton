(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Elaborate (S: ELABORATE_STRUCTS): ELABORATE = 
struct

open S

structure Core = CoreLang     (structure TyAtom = TyAtom
							   structure CoreML = CoreML)

structure Decs = Decs         (structure TyAtom = TyAtom
							   structure Core   = Core)

structure Env  = ElaborateEnv (structure Atoms  = CoreML.Atoms
							   open Atoms
							   structure Ast    = Ast
							   structure TyAtom = TyAtom)

structure ElaborateMLBs = ElaborateMLBs (structure Ast = Ast
							             structure Core = Core
                                         structure Decs = Decs
                                         structure Env = Env
                                         structure CoreML = CoreML)

open ElaborateMLBs
end

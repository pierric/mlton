signature DECS_STRUCTS =
   sig
      structure Core   : CORE_LANG
      structure TyAtom : TYPE_ATOM
      sharing TyAtom = Core.TyAtom
   end

signature DECS =
   sig
      include DECS_STRUCTS

      type dec = Core.Dec.t

      type t

      val empty: t
      val single: dec -> t
      val cons: dec * t -> t
      val add: t * dec -> t
      val append: t * t -> t
      val fromList: dec list -> t
      val layout: t -> Layout.t
      val map: t * (dec -> dec) -> t
      val toList: t -> dec list
      val toVector : t -> dec vector

      val subst : TyAtom.Subst.t * t -> t
   end
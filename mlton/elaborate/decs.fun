functor Decs (S: DECS_STRUCTS) = 
struct
  open S
  
  type dec = Core.Dec.t
  type t   = dec list

  val empty      = []
  fun single (d) = [d]
  val append 	 = List.append
  fun cons (d,t) = d::t
  fun add  (t,d) = append(t, single (d))
  fun fromList t = t
  val layout     = List.layout (Core.Dec.layout)
  val map        = List.map
  fun toList t   = t
  val toVector   = Vector.fromList

  fun subst (rho, t) = map (t, fn cdec => Core.Dec.subst (rho, cdec))
end
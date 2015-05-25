signature CORE_LANG_STRUCTS = sig
  include ATOMS
  structure TyAtom : TYPE_ATOM
end

signature CORE_LANG = sig

  include CORE_LANG_STRUCTS

  structure Pat: sig
    datatype t =
       Var      of Var.t
     | Const    of Const.t
     | Con      of Con.t * t option
     | Layered  of Var.t * t
     | Wild

    val layout: t -> Layout.t

    val isWild: t -> bool
    val truee: t
    val falsee: t
  end

end

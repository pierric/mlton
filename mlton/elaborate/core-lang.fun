functor CoreLang (S: CORE_LANG_STRUCTS) = 
struct
  open S

  structure Pat = 
  struct
    datatype t = Var      of Var.t
               | Const    of Const.t
               | Con      of Con.t * t option
               | Layered  of Var.t * t
               | Wild

    fun layout pat = Layout.empty

    fun isWild pat = 
      case pat of
        Wild => true
      | _    => false

    fun truee  pat = 
      Con (Con.truee,  NONE)
    fun falsee pat = 
      Con (Con.falsee, NONE)
  end
end

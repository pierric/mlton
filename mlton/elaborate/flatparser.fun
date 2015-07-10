functor FlatParser(S:FLAT_PARSER_STRUCTS) : FLAT_PARSER =
struct
  open S

  structure Apat = Ast.Pat
  structure Aexp = Ast.Exp

  datatype 'a tree   = E of 'a list
                     | B of { x: 'a, lft: 'a tree, rit: 'a tree }
  datatype 'a node   = T of 'a tree
                     | O of 'a
  type     'a stack  = 'a node list
  datatype 'a result = Done of 'a tree
                     | Err  of Layout.t

  fun fromTree (tr: 'a tree, app: 'a list -> 'a) : 'a =
    case tr of
      E xs => app xs
    | B {x, lft, rit} => app [ x
                             , fromTree (lft, app)
                             , fromTree (rit, app) ]

  fun pushS (a, s) = a :: s
  fun popS  (a::s) = (a,s)
    | popS  _      = Error.bug "pop an empty stack in flatparser"
  fun topS  (a::s) = a
    | topS  _      = Error.bug "retrieve top of an empty stack in flatparser"
  val emptyS       = []
  fun isEmptyS []  = true
    | isEmptyS _   = false

  fun 'a parse { xs    : 'a list
               , env   : Env.t
               , lay   : 'a -> Layout.t
               , deVid : 'a -> Ast.Longvid.t option } : 'a result =
    Exn.withEscape (fn done =>
      let
        open Layout
        fun layT (E xs) = List.layout lay xs
          | layT (B {x, lft, rit}) = seq [ str "(", layT lft, str ") "
                                         , lay x
                                         , str " (", layT rit, str ")"]
        fun misplaced_op e =
          Err (seq [str "mispalced infix operator: ", lay e])
        fun expect_op    e =
          Err (seq [str "expecting an operator, but met a expression: ", layT e])
        fun insufficient_args (p, e) =
          Err (seq [str "insufficient arguments: ", lay p, str " ", layT e, str "?"])
        fun ambiguous (op1,op2) =
          Err (seq [ str "Operators of same precedence with mixed associativity: "
                   , lay op1
                   , str " "
                   , lay op2])

        datatype z = datatype Env.Fixity.t
        fun peek (xs : 'a list) : 'a list * 'a list =
          List.splitPrefix (xs, fn x =>
            case deVid x of
              SOME vid => (case Env.lookupFixity (env, vid) of
                             NONE        => true
                           | SOME Nonfix => true
                           | _           => false)
            | NONE     => true)

        datatype OpOrder = LT | GT
        fun compareOp op1 op2 =
          let
            val fixity1 = Env.lookupFixity (env, Option.valOf (deVid op1))
            val fixity2 = Env.lookupFixity (env, Option.valOf (deVid op2))
            fun comparePrec (p1, p2, default) =
              let
                val p1 = case p1 of NONE => 0 | SOME p => p
                val p2 = case p2 of NONE => 0 | SOME p => p
              in
                if p1 < p2      then SOME LT
                else if p1 > p2 then SOME GT
                else default
              end
            val order   = case (Option.valOf fixity1, Option.valOf fixity2) of
                            (Infix  p1, Infix  p2) => comparePrec (p1, p2, SOME GT)
                          | (Infixr p1, Infixr p2) => comparePrec (p1, p2, SOME LT)
                          | (Infix  p1, Infixr p2) => comparePrec (p1, p2, NONE)
                          | (Infixr p1, Infix  p2) => comparePrec (p1, p2, NONE)
                          | _                      => Error.bug "non-operator in compareOp"
            val _ = if Option.isNone order then done (ambiguous (op1, op2)) else ()
          in
            Option.valOf order
          end

        fun reduceToS (to: 'a option, stack: 'a stack) : 'a stack =
          let
            (* `done` must give up control, but `done` is not polymorphic. *)
            fun err msg = (done msg; Error.bug "Unhandled error in flatparser")
            val _ = if isEmptyS stack then
                      Error.bug "No element to deal with in flatparser"
                    else
                      ()
            val (e1, stack') = popS stack
            val e1 = case e1 of O x => err (misplaced_op x) | T x => x
          in
            if isEmptyS stack' then
              stack (* only one sitting on stack, return the original stack *)
            else
              let
                val (operator, stack') = popS stack'
                val operator = case operator of O x => x | T x => err (expect_op x)
                val _  = if isEmptyS stack' then
                           err (insufficient_args (operator, e1))
                         else
                           ()
              in
                if Option.map (to, compareOp operator) = SOME LT then
                  stack (* reach the destination, return the original stack *)
                else
                  let
                    val (e2, stack') = popS stack'
                    val e2 = case e2 of O x => err (misplaced_op x) | T x => x
                    val stack' = pushS (T (B {x = operator, lft = e2, rit = e1}), stack')
                  in
                    reduceToS (to, stack')
                  end
              end
          end

        fun go (xs : 'a list, stack : 'a stack) : 'a tree =
          case xs of
            (* Reduce the whole stack to a single element.
               At this point of time, the operators on stack
               are in strictly descending order.
             *)
            [] => (case topS (reduceToS (NONE, stack)) of
                     O _ => Error.bug "An operator left on stack after reduce."
                   | T x => x)
            (* Go on to process the left elements *)
          | _  => let
                    val _ = Control.message (Control.Detail, fn () =>
                              Layout.seq [Layout.str "Going to peek from: ", List.layout lay xs])
                    val (e, xs) = peek xs
                    val _ = Control.message (Control.Detail, fn () =>
                              Layout.seq [Layout.str "peek done: ",
                                          List.layout lay e,
                                          str ",",
                                          List.layout lay xs])
                    val stackN  = pushS (T (E e), stack)
                  in
                    case xs of
                      (* if nothing is left to process, it's time to reduce all *)
                      []       => go ([], stackN)
                      (* otherwise, obtain one operator *)
                    | x :: xs' => go (xs', pushS (O x, reduceToS (SOME x, stackN)))
                  end
      in
        Done (go (xs, emptyS))
      end)

  structure Aexp = Ast.Exp
  fun parseExps (env, exps) =
    let
      val _ = Control.message (Control.Detail, fn () =>
                Layout.seq [Layout.str "parseExps: ", Vector.layout Aexp.layout exps])
      val result = parse { xs    = Vector.toList exps
                         , env   = env
                         , deVid = fn e => case Aexp.node e of
                                     Aexp.Var {name,...} => SOME name
                                   | _ => NONE
                         , lay   = Aexp.layout}
      val e0     = Vector.sub (exps, 0)
      val region = Vector.foldFrom (exps, 1, Aexp.region e0 ,
                     fn (e,r) => Region.append (r, Aexp.region e))
      fun flip (a,b) = (b,a)
    in
      case result of
        Err msg => (Control.error (region, msg, Layout.empty); e0)
      | Done tr => let
                     val exp = fromTree (tr, fn xs =>
                                 case xs of
                                   []    => Error.bug "empty expression in flatparser"
                                 | [x]   => x
                                 | x::xs => List.fold (xs, x, Aexp.app o flip))
                     val _   = Control.message (Control.Detail, fn () =>
                                 Layout.seq [Layout.str "result exp: ", Aexp.layout exp])
                   in
                     exp
                   end
    end

  fun parsePats (env, pats) =
    let
      val _ = Control.message (Control.Detail, fn () =>
                Layout.seq [Layout.str "parsePats: ", Vector.layout Apat.layout pats])
      val result = parse { xs    = Vector.toList pats
                         , env   = env
                         , deVid = fn e => case Apat.node e of
                                     Apat.Var {name,...} => SOME name
                                   | _ => NONE
                         , lay   = Apat.layout}
      val e0     = Vector.sub (pats, 0)
      val region = Vector.foldFrom (pats, 1, Apat.region e0 ,
                     fn (e,r) => Region.append (r, Apat.region e))
      fun apat_flip_app (a, b) =
        let
          val pat = case Apat.node b of
                      Apat.Var {name = vid, ...} =>
                        Apat.App (Ast.Longvid.toLongcon vid, a)
                    | _ =>
                      (Control.error (Apat.region b,
                         Layout.str "misplaced pattern as constructor",
                         Layout.empty);
                       Apat.Wild)
          val reg = Region.append (Apat.region b, Apat.region a)
        in
          Apat.makeRegion (pat, reg)
        end
    in
      case result of
        Err msg => (Control.error (region, msg, Layout.empty); e0)
      | Done tr => fromTree (tr, fn xs =>
                     case xs of
                       []    => Error.bug "empty patterns in flatparser - parsePats"
                     | [x]   => x
                     | x::xs => List.fold (xs, x, apat_flip_app))
    end

  fun parsePatsFunSig (env, pats) =
    let
      val _ = Control.message (Control.Detail, fn () =>
                Layout.seq [Layout.str "parsePatsFunSig: ", Vector.layout Apat.layout pats])
      val result = parse { xs    = Vector.toList pats
                         , env   = env
                         , deVid = fn e => case Apat.node e of
                                     Apat.Var {name,...} => SOME name
                                   | _ => NONE
                         , lay   = Apat.layout}
      val e0     = Vector.sub (pats, 0)
      val region = Vector.foldFrom (pats, 1, Apat.region e0 ,
                     fn (e,r) => Region.append (r, Apat.region e))
    in
      case result of
        Err msg => (Control.error (region, msg, Layout.empty);
                    (e0, Vector.dropPrefix (pats, 1)))
      | Done tr =>
         (case tr of
            E [] => Error.bug "empty patterns in flatparser - parsePatsFunSig"
          | E (x::args) =>
              let
                val _ = Control.message (Control.Detail, fn () =>
                          Layout.seq [ Layout.str "func name: ", Apat.layout x
                                     , Layout.str ", args: ", List.layout Apat.layout args])
              in
                (x, Vector.fromList args)
              end
          | B {x,lft,rit} =>
              let
                val e  = fn ret =>
                           (Control.error (Apat.region x,
                              Layout.str "misplaced operator name.",
                              Layout.empty);
                            ret)
                val a1 = case lft of
                           E []  => Error.bug "operator function has not left hand side operand"
                         | E [x] => x
                         | B _   => e (Apat.wild)
                val a2 = case rit of E xs  => xs | B _ => e ([Apat.wild])
                val _ = Control.message (Control.Detail, fn () =>
                          Layout.seq [ Layout.str "func name: ", Apat.layout x
                                     , Layout.str ", args: ", List.layout Apat.layout (a1::a2)])
              in
                (x, Vector.fromList (a1::a2))
              end)
    end

end

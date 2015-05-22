signature ELABORATE_PROGRAMS_STRUCTS = sig
    structure Ast: AST
    structure CoreML: CORE_ML
    structure Decs: DECS
    structure Env: ELABORATE_ENV
    sharing Ast = Env.Ast
    sharing Ast.Tyvar = CoreML.Tyvar
    sharing CoreML = Decs.CoreML = Env.CoreML
    sharing Decs = Env.Decs
end

signature ELABORATE_PROGRAM = sig
    include ELABORATE_PROGRAMS_STRUCTS
    val elaborateProgram: Ast.Program.t * {env: Env.t} -> Decs.t
end

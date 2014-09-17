namespace FSharp.Monad

type F2 = F2

type F2<'T, 'U, 'V> = {
  Apply : 'T -> 'U -> 'V
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module F2 =

  let ofFunc f = { Apply = f }

  let toFunc f = f.Apply

[<AutoOpen>]
module F2DefaultOps =

  let (|F2|) f = F2.ofFunc f
  let (|StdF2|) f = F2.toFunc f

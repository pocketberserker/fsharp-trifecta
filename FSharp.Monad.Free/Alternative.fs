namespace FSharp.Monad

type Alternative<'F> =
  inherit Applicative<'F>
  inherit PlusEmpty<'F>

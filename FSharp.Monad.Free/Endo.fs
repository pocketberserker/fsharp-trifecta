namespace FSharp.Monad

type Endo<'T> = Endo of ('T -> 'T)

module Endo =

  let apply a (Endo run) = run a

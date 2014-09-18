namespace FSharp.Monad

[<AutoOpen>]
module Prelude =

  let flip f a b = f b a

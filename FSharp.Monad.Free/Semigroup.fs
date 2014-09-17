namespace FSharp.Monad

type Semigroup<'F> =
  abstract member Append: 'F * 'F -> 'F

module Semigroup =

  let append (s: #Semigroup<_>) a b = s.Append(a, b)

  let instance f = { new Semigroup<_> with
    member this.Append(f1, f2) = f f1 f2 }

namespace FSharp.Monad

type Traverse<'F> =
  inherit Foldable<'F>
  inherit Functor<'F>
  abstract member Traverse: _1<'F, 'A> * F1<'A, _1<'G, 'B>> * #Applicative<'G> -> _1<'G, _1<'F, 'B>>

module Traverse =

  let traverse (T: Traverse<_>) (G: #Applicative<_>) (F1 f) fa =
    T.Traverse(fa, f, G)

  let sequence (T: Traverse<_>) (G: #Applicative<_>) fga =
    T.Traverse(fga, F1.ofFunc id, G)

namespace FSharp.Monad

type Foldable<'F> =
  abstract member FoldMap: F1<'A, 'B> * _1<'F, 'A> * #Monoid<'B> -> 'B
  abstract member Fold: _1<'F, 'A> * 'B * F2<'B, 'A, 'B> -> 'B

module Foldable =

  let foldMap (f: #Foldable<_>) M (F1 g) fa = f.FoldMap(g, fa, M)
  let fold (f: #Foldable<_>) (F2 g) b fa = f.Fold(fa, b, g)

  let foldl (f: #Foldable<_>) g z fa =
    fa
    |> foldMap f Endo.monoid (fun a -> Endo(flip g a))
    |> Endo.apply z

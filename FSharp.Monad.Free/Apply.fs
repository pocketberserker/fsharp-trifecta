namespace FSharp.Monad

type Apply<'F> =
  inherit Functor<'F>
  abstract member Ap :F0<_1<'F, F1<'A, 'B>>> * F0<_1<'F, 'A>> -> _1<'F, 'B>

module Apply =

  let inline ap (fa: #Apply<_>) (F0 f) (F0 g) = fa.Ap(f, g)

  let apply2 (a: #Apply<_>) fa1 fa2 f =
    ap a (fun () -> Functor.map a (fun a1 -> F1.ofFunc <| fun a2 -> f a1 a2) (fa1 ())) fa2

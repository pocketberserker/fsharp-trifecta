namespace FSharp.Monad

type Plus<'F> =
  abstract member Plus: #_1<'F, 'A> * #_1<'F, 'A> -> #_1<'F, 'A>

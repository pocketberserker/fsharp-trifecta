namespace FSharp.Monad

type List = List

type List<'A> = {
  Value: 'A list
}
  with
    interface _1<List, 'A>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module List =

  let empty<'T> = { Value = [] } :> _1<List, 'T>

  let inline cons hd (tl: _1<List, _>) = { Value = hd :: (tl :?> List<_>).Value } :> _1<List, _>

  let toStdList xs = xs.Value

  let ofStdList xs = { Value = xs } :> _1<List, _>

  let private traverse' (F: #Applicative<_>) f (xs: 'T list) =
    List.foldBack (fun a (fbs: _1<_, _1<List, _>>) -> Apply.apply2 F (fun () -> f a) (fun () -> fbs) cons) xs (F.Point(F0.ofFunc <| fun () -> empty))

  let foldMap (m: #Monoid<_>) f xs =
    let rec inner acc = function
      | [] -> acc
      | x::xs -> inner (Monoid.append m acc (f x)) xs
    inner (Monoid.zero m) xs

  let traverse = { new Traverse<List> with
    member x.Fold(fa, z, StdF2 f) =
      fa :?> List<_> |> toStdList |> List.fold f z
    member x.FoldMap(StdF1 f, fa, m) =
      foldMap m f (fa :?> List<_> |> toStdList)
    member x.Map(f, fa) =
      fa :?> List<_> |> toStdList |> List.map f |> ofStdList
    member x.Traverse(fa: _1<_, _>, StdF1 f, G: #Applicative<_>) =
      fa :?> List<_> |> toStdList |> traverse' G f
  }

  let monad = { new Monad<List>() with
    member this.Map(f, fa) = fa :?> List<_> |> toStdList |> List.map f |> ofStdList
    member this.Point(a) = { Value = [a.Apply()] } :> _1<List, _>
    member this.Bind(StdF1 f, fa) =
      fa :?> List<_> |> toStdList |> List.collect (fun a -> f a :?> List<_> |> toStdList) |> ofStdList }

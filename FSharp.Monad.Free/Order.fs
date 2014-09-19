namespace FSharp.Monad

// TODO: implement Equal type class
[<AbstractClass>]
type Order<'T>() =
  abstract member Order: 'T * 'T -> Ordering
  abstract member Eq: 'T * 'T -> bool
  default this.Eq(x, y) = this.Order(x, y) = EQ
  member this.LessThan(x, y) = this.Order(x, y) = LT
  member this.LessThanOrEqual(x, y) = this.Order(x, y) <> GT
  member this.GreaterThan(x, y) = this.Order(x, y) = GT
  member this.GreaterThanOrEqual(x, y) = this.Order(x, y) <> LT
  member this.Max(x, y) = if this.GreaterThanOrEqual(x, y) then x else y
  member this.Min(x, y) = if this.LessThan(x, y) then x else y

module Order =

  let inline order (o: #Order<_>) x y = o.Order(x, y)
  let inline eq (o: #Order<_>) x y = o.Eq(x, y)
  let inline lessThan (o: #Order<_>) x y = o.LessThan(x, y)
  let inline lessThanOrEqual (o: #Order<_>) x y = o.LessThanOrEqual(x, y)
  let inline greaterThan (o: #Order<_>) x y = o.GreaterThan(x, y)
  let inline greaterThanOrEqual (o: #Order<_>) x y = o.GreaterThanOrEqual(x, y)
  let inline max (o: #Order<_>) x y = o.Max(x, y)
  let inline min (o: #Order<_>) x y = o.Min(x, y)

  let reverse (o: #Order<_>) = { new Order<_>() with
    override this.Order(x, y) = o.Order(y, x)
    override this.Eq(x, y) = o.Eq(x, y) }

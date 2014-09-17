namespace Trifecta

module private SupplyM =

  [<Literal>]
  let minSplitSupplySize = 32

  [<Literal>]
  let blockSize = 1024

  let mutable block = 0

  let lockObject = System.Object()

  let getBlock () =
    lock lockObject (fun () ->
      let result = block
      block <- result + blockSize
      result
    )

type Supply private (lo: int, hi: int) =
  let mutable lo = lo
  let mutable hi = hi
  member this.Fresh() =
    if lo <> hi then
       let result = lo
       lo <- result + 1
       result
    else
      let result = SupplyM.getBlock ()
      hi <- result + SupplyM.blockSize - 1
      lo <- result + 1
      result
  member this.Split() =
    if hi - lo >= SupplyM.minSplitSupplySize then
      let mid = lo + (hi - lo) / 2
      let result = Supply(lo, mid)
      lo <- mid + 1
      result
    else
      let newLo = SupplyM.getBlock ()
      lo <- newLo + SupplyM.blockSize / 2
      hi <- newLo + SupplyM.blockSize - 1
      Supply(newLo, lo - 1)
  static member Create() =
    let b = SupplyM.getBlock ()
    Supply(b, b + SupplyM.blockSize - 1)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Supply =

  let inline split (s: Supply) = s.Split()
  let inline fresh (s: Supply) = s.Fresh()
  let inline create () = Supply.Create()

  let minSplitSupplySize = SupplyM.minSplitSupplySize
  let blockSize = SupplyM.blockSize
  let getBlock () = SupplyM.getBlock()

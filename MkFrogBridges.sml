functor MkFrogBridges(structure Seq : SEQUENCE) : FROGBRIDGES =
struct
  structure Seq = Seq
  open Seq

  structure  Memoizer = MkMemoizer(
    structure Argument = MkPairElt(
      structure EltA = IntElt
      structure EltB = IntElt
    )
  )

  type color = int
  type village = int * color

  fun maxBridges (L, R) =
    let
      fun srt ((x1, _), (x2, _)) = Int.compare (x1, x2)
      val L1 = Seq.sort srt L
      val R2 = Seq. sort srt R

      fun isBridge (i, j) = 
        let
           val (lx, lc) = Seq.nth L1 i 
           val (rx, rc) = Seq.nth R2 j
         in
           lc = rc
         end 

      val build = Memoizer.memoize(
          fn build =>
            fn (0, 0) => if isBridge (0, 0) then 1 else 0
             | (0, r) => if isBridge(0, r) then 1 else build(0, r-1)
             | (l, 0) => if isBridge(l, 0) then 1 else build(l-1, 0)
             | (l, r) => if isBridge(l, r) 
                         then 1 + build(l-1, r-1) 
                         else Int.max (build(l, r-1), build(l-1, r))
      )
    in
      build(Seq.length L - 1, Seq.length R - 1)
    end
end

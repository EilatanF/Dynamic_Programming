functor MkSeamFind(structure Seq : SEQUENCE) : SEAMFIND =
struct
  structure Seq = Seq
  open Seq

  type pixel = { r : real, g : real, b : real }
  type image = { width : int, height : int, data : pixel seq seq }
  type gradient = real

  fun findSeam G =
    let
      val idxG = Seq.map Seq.enum G

      val w = Seq.length (Seq.nth G 0)
      val h = Seq.length G

      val base' = Seq.nth idxG 0
      val base = Seq.map (fn (i, g) => ([i], g)) base'
      val nextG = Seq.subseq idxG (1, h-1)

      fun findMin ((i1, r1), (i2, r2)) =
        case (Real.compare (r1, r2)) of
          LESS => (i1, r1)
          | _ => (i2, r2)

      fun update (topLine, botLine) = 
        let
          fun find num = 
            Seq.nth topLine num

          fun findTop num = 
            case (num, w-1-num) of
              (0, _) => findMin(find 0, find 1)
            | (_, 0) => findMin(find (w-2), find (w-1))
            | _ =>  findMin(find (num-1), 
                   findMin(find num, find (num+1)))

          fun mapFun (i, grad) = 
            let
              val (is, grads) = findTop i

            in
              (i :: is, grads + grad)
            end
        in
          Seq.map mapFun botLine
        end

      val M = Seq.iterate update base nextG

      val (seam, seamVal) = Seq.reduce findMin (Seq.nth M 0) M

    in
      Seq.rev (Seq.fromList seam)
    end
end
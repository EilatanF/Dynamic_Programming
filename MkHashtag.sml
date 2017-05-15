functor MkHashtag(structure Seq : SEQUENCE
                  structure Dict : DICTIONARY) : HASHTAG =
struct
  structure Seq = Seq
  open Seq
  structure Dict = Dict
  open Dict

  fun isValidHashtag S =
    let
      val length = String.size S
      fun hash (0, n) = isWord(String.substring (S, 0, n+1))
        | hash (m, n) = (isWord(String.substring(S, m, n-m+1)) andalso 
                        hash(m-1, m-1))
                        orelse hash(m-1, n)
    in
      hash(length-1, length-1)
    end
end

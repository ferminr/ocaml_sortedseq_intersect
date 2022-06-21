(* 
  Signature for a container type with functions get and length, and an comparsion function for elements of the container
*)
module type S = sig
  type elt
  type t

  val cmp: elt -> elt -> int
  val get: t -> int -> elt
  val length: t -> int
end


(* Intersection by linear search on sorted sequences. Restricted to ranges [lo..hi] and [b_lo..b_hi] *)

let intersect_merge_range (type e) (type t) ?(acc=[]) (module M : S with type t = t and type elt = e) a lo hi b b_lo b_hi =
  let ( .%{} ) = M.get in
  let rec imerge acc i j = 
    if i>hi || j>b_hi then 
      acc
    else
      match (M.cmp a.%{i} b.%{j}) with
      | -1 -> imerge acc (i+1) j
      |  0 -> imerge (a.%{i}::acc) (i+1) (j+1)
      |  _ -> imerge acc i (j+1)
  in
  imerge acc lo b_lo

(* Intersection by linear search on sorted sequences *)
  
let intersect_merge (type e) (type t) (module M : S with type t = t and type elt = e) a b = intersect_merge_range (module M) a 0 (M.length a - 1) b 0 (M.length b - 1)


  
(* Lookup e in arr by binary search, returning the first index where we'd insert e to preserve the sorted property.
   Returns i = min value in [lo..hi] such that arr[i] >= e 
   If e>arr[i] for every i in [lo..hi], then return hi+1
   Precondition: arr sorted. 
  *)

let lookup (type e) (type t) (module M : S with type t = t and type elt = e) e arr lo hi =
  let rec look lo hi =
    if lo > hi then
      lo
    else 
      let mid = lo + (hi-lo) / 2 in
      match M.cmp e (M.get arr mid) with
      |  0 -> mid
      | -1 -> look lo (mid-1)
      |  _ -> look (mid+1) hi
  in
    look lo hi
  

(* TODO: better name than swap *)
let[@inline] swap f a lo hi b b_lo b_hi =
  if hi-lo <= b_hi-b_lo then
    f a lo hi b b_lo b_hi
  else     
    f b b_lo b_hi a lo hi


(* Intersection using the Baeza-Yates-Salinger algorithm on sorted sequences
   Complexity is O(m log(n/m)), where m=|a|, n=|b|
   If m is o(n/log n), this algorithm is better than linear merging, which is O(m+n)
   If m << n, it's better to do m binary searches in b

   When we call this function (including in the recursive calls), the first sequence sould be the one of smaller size
*)
let rec intersect_bisect_range (type e) (type t) a lo hi b b_lo b_hi acc (module M : S with type t = t and type elt = e) =
  let ( .%{} ) = M.get in
  let rec isect a lo hi b b_lo b_hi acc = 
    (* the intersection is empty if either array range is empty, or if the values in the arrays don't overlap *)
    if lo > hi || b_lo > b_hi || a.%{lo} > b.%{b_hi} || b.%{b_lo} > a.%{hi} then
        acc
    else
      (* Divide a into two sections, left and right of the median. The two sections are the same size, +/- 1 elem.
        Divide b into two sections, left and right of the position of a's median in b. B's sections need not be of the same size. *)
      let mid = lo + (hi-lo) / 2 in
      let median = a.%{mid} in
      (* possible optimisation: obtain a range in b smaller than lob..hib by bracketing with a.(lo) and a.(hi) in b. But searching has a cost, so we're not guaranteed to reduce the total runtime *)
      let b_mid = lookup (module M) median b b_lo b_hi in
      (* recurse on the left sections of a and b *)
      let acc = swap isect a lo (mid-1) b b_lo (b_mid-1) acc in
      (* recurse on the right sections of a and b *)
      if M.cmp median b.%{b_mid} = 0 then
        swap isect a (mid+1) hi b (b_mid+1) b_hi (median::acc)
      else
        swap isect a (mid+1) hi b b_mid b_hi acc
  in
    isect a lo hi b b_lo b_hi acc


let intersect_bisect (type e) (type t) (module M : S with type t = t and type elt = e) a b = swap intersect_bisect_range a 0 (M.length a - 1) b 0 (M.length b - 1) [] (module M)


(* Adaptive algorithm: switch to linear merge when m grows wrt to n. The paper suggests once m = 0.031n + 20.696 *)
let rec intersect_adaptive_range (type e) (type t) a lo hi b b_lo b_hi acc (module M : S with type t = t and type elt = e) =
  let ( .%{} ) = M.get in
  let rec isect a lo hi b b_lo b_hi acc =
  (* the intersection is empty if either array range is empty, or if the values in the arrays don't overlap *)
    if lo > hi || b_lo > b_hi || a.%{lo} > b.%{b_hi} || b.%{b_lo} > a.%{hi} then
      acc
  (* switch to linear merge when m grows wrt n *)
    else if (hi-lo) * 32 > (b_hi-b_lo) then
      intersect_merge_range  ~acc:acc (module M) a lo hi b b_lo b_hi 
    else 
      (* Divide a into two sections, left and right of the median. The two sections are the same size, +/- 1 elem.
        Divide b into two sections, left and right of the position of a's median in b. B's sections need not be of the same size. *)
      let mid = lo + (hi-lo) / 2 in
      let median = a.%{mid} in
      (* possible optimisation: obtain a range in b smaller than lob..hib by bracketing with a.(lo) and a.(hi) in b. But searching has a cost, so we're not guaranteed to reduce the total runtime *)
      let b_mid = lookup (module M) median b b_lo b_hi in
      (* recurse on the left sections of a and b *)
      let acc = swap isect a lo (mid-1) b b_lo (b_mid-1) acc in
      (* recurse on the right sections of a and b *)
      if M.cmp median b.%{b_mid} = 0 then
        swap isect a (mid+1) hi b (b_mid+1) b_hi (median::acc) 
      else
        swap isect a (mid+1) hi b b_mid b_hi acc
  in
    isect a lo hi b b_lo b_hi acc


let intersect_bisect_adaptive (type e) (type t) (module M : S with type t = t and type elt = e) a b = swap intersect_adaptive_range a 0 (M.length a - 1) b 0 (M.length b - 1) [] (module M) 


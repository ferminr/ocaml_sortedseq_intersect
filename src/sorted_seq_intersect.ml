
(* Intersection by linear search on sorted sequences. Restricted to ranges [lo..hi] and [b_lo..b_hi] *)
let intersect_merge_range ?(acc=[]) a lo hi b b_lo b_hi = 
  let rec imerge acc i j = 
    if i>hi || j>b_hi then 
      acc
    else
      match (compare a.(i) b.(j)) with
      | -1 -> imerge acc (i+1) j
      |  0 -> imerge (a.(i)::acc) (i+1) (j+1)
      |  _ -> imerge acc i (j+1)
  in
  imerge acc lo b_lo

(* Intersection by linear search on sorted sequences *)
let intersect_merge a b = intersect_merge_range a 0 (Array.length a - 1)  b 0 (Array.length b - 1)
  
  
  
(* Lookup e in arr by binary search, returning the first index where we'd insert e to preserve the sorted property.
   Returns i = min value in [lo..hi] such that arr[i] >= e 
   If e>arr[i] for every i in [lo..hi], then return hi+1
   Precondition: arr sorted. 
  *)
let rec lookup e arr lo hi =
  if lo > hi then
    lo
  else 
    let mid = lo + (hi-lo) / 2 in
    match compare e arr.(mid) with
    |  0 -> mid
    | -1 -> lookup e arr lo (mid-1)
    |  _ -> lookup e arr (mid+1) hi



(* TODO: better name than swap *)
let[@inline] swap f acc a lo hi b b_lo b_hi =
  if hi-lo <= b_hi-b_lo then
    f acc a lo hi b b_lo b_hi
  else     
    f acc b b_lo b_hi a lo hi


(* Intersection using the Baeza-Yates-Salinger algorithm on sorted sequences
   Complexity is O(m log(n/m)), where m=|a|, n=|b|
   If m is o(n/log n), this algorithm is better than linear merging, which is O(m+n)
   If m << n, it's better to do m binary searches in b

   When we call this function (including in the recursive calls), the first sequence sould be the one of smaller size
*)
let rec intersect_bisect_range acc a lo hi b b_lo b_hi =
  (* the intersection is empty if either array range is empty, or if the values in the arrays don't overlap *)
  if lo > hi || b_lo > b_hi || a.(lo) > b.(b_hi) || b.(b_lo) > a.(hi) then
      acc
  else
    (* Divide a into two sections, left and right of the median. The two sections are the same size, +/- 1 elem.
       Divide b into two sections, left and right of the position of a's median in b. B's sections need not be of the same size. *)
    let mid = lo + (hi-lo) / 2 in
    let median = a.(mid) in
    (* possible optimisation: obtain a range in b smaller than lob..hib by bracketing with a.(lo) and a.(hi) in b. But searching has a cost, so we're not guaranteed to reduce the total runtime *)
    let b_mid = lookup median b b_lo b_hi in
    (* recurse on the left sections of a and b *)
    let acc = swap intersect_bisect_range acc a lo (mid-1) b b_lo (b_mid-1) in
    if b_mid > b_hi then
      acc
    else  
      (* recurse on the right sections of a and b *)
      if median = b.(b_mid) then
        swap intersect_bisect_range (median::acc) a (mid+1) hi b (b_mid+1) b_hi
      else
        swap intersect_bisect_range acc a (mid+1) hi b b_mid b_hi


let intersect_bisect a b = swap intersect_bisect_range [] a 0 (Array.length a - 1) b 0 (Array.length b - 1)

(* Adaptive algorithm: switch to linear merge when m grows wrt to n. The paper suggests once m = 0.031n + 20.696 *)
let rec intersect_adaptive_range acc a lo hi b b_lo b_hi =
  (* the intersection is empty if either array range is empty, or if the values in the arrays don't overlap *)
  if lo > hi || b_lo > b_hi || a.(lo) > b.(b_hi) || b.(b_lo) > a.(hi) then
      acc
  (* switch to linear merge when m grows wrt n *)
  else if (hi-lo) * 32 > (b_hi-b_lo) then
    intersect_merge_range ~acc:acc a lo hi b b_lo b_hi
  else 
    (* Divide a into two sections, left and right of the median. The two sections are the same size, +/- 1 elem.
       Divide b into two sections, left and right of the position of a's median in b. B's sections need not be of the same size. *)
    let mid = lo + (hi-lo) / 2 in
    let median = a.(mid) in
    (* possible optimisation: obtain a range in b smaller than lob..hib by bracketing with a.(lo) and a.(hi) in b. But searching has a cost, so we're not guaranteed to reduce the total runtime *)
    let b_mid = lookup median b b_lo b_hi in
    (* recurse on the left sections of a and b *)
    let acc = swap intersect_adaptive_range acc a lo (mid-1) b b_lo (b_mid-1) in
    if b_mid > b_hi then
      acc
    else  
      (* recurse on the right sections of a and b *)
      if median = b.(b_mid) then
        swap intersect_adaptive_range (median::acc) a (mid+1) hi b (b_mid+1) b_hi
      else
        swap intersect_adaptive_range acc a (mid+1) hi b b_mid b_hi


let intersect_bisect_adaptive a b = swap intersect_adaptive_range [] a 0 (Array.length a - 1) b 0 (Array.length b - 1)


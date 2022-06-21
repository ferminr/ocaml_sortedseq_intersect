(** 
  Signature for a container type with functions get and length, and an comparsion function for elements in the container
*)
module type S = sig
  type elt
  type t

  val cmp: elt -> elt -> int
  val get: t -> int -> elt
  val length: t -> int
end


(** 
  Intersection of sorted sequences, using a linear merge algorithm, with complexity O(m+n).
*)
val intersect_merge: (module S with type elt = 'a and type t = 't) -> 't -> 't -> 'a list

(**
  Like [intersect_merge], but restricted to ranges a[lo..hi], b[b_lo..b_hi]
*)
val intersect_merge_range: ?acc:'a list -> (module S with type elt = 'a and type t = 't) ->
  't -> int -> int -> 't -> int -> int -> 'a list

(** 
  Intersection of sorted sequences, using the Baeza-Yates-Salinger algorithm, with complexity O(m log(n/m)).
  If m is o(n/log n), this algorithm is better than linear merging, which is O(m+n)
  If m << n, it's better to do m binary searches in b
*)
val intersect_bisect: (module S with type elt = 'a and type t = 't) -> 't -> 't -> 'a list

(**
  Like [intersect_bisect_merge], but restricted to ranges a[lo..hi], b[b_lo..b_hi]
*)
val intersect_bisect_range: 't -> int -> int -> 't -> int -> int -> 'a list -> (module S with type elt = 'a and type t = 't) -> 'a list

(** 
  Intersection of sorted sequences, using the adaptive Baeza-Yates-Salinger algorithm, which switches to a merge algorithm when n/m < 32.
  If m is o(n/log n), this algorithm is better than linear merging, which is O(m+n)
  If m << n, it's better to do m binary searches in b
*)
val intersect_bisect_adaptive: (module S with type elt = 'a and type t = 't) -> 't -> 't -> 'a list

(**
  Like [intersect_bisect_adaptive], but restricted to ranges a[lo..hi], b[b_lo..b_hi]
*)
val intersect_adaptive_range: 't -> int -> int -> 't -> int -> int -> 'a list -> (module S with type elt = 'a and type t = 't) -> 'a list

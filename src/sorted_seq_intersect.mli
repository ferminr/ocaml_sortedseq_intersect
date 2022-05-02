(** 
  Intersection of sorted sequences, using a linear merge algorithm, with complexity O(m+n).
*)
val intersect_merge: 'a array -> 'a array -> 'a list

(**
  Like [intersect_merge], but restricted to ranges a[lo..hi], b[b_lo..b_hi]
*)
val intersect_merge_range: ?acc:'a list -> 'a array -> int -> int -> 'a array -> int -> int -> 'a list

(** 
  Intersection of sorted sequences, using the Baeza-Yates-Salinger algorithm, with complexity O(m log(n/m)).
  If m is o(n/log n), this algorithm is better than linear merging, which is O(m+n)
  If m << n, it's better to do m binary searches in b
*)
val intersect_bisect: 'a array -> 'a array -> 'a list

(**
  Like [intersect_bisect_merge], but restricted to ranges a[lo..hi], b[b_lo..b_hi]
*)
val intersect_bisect_range: 'a list -> 'a array -> int -> int -> 'a array -> int -> int -> 'a list

(** 
  Intersection of sorted sequences, using the adaptive Baeza-Yates-Salinger algorithm, which switches to a merge algorithm when n/m < 32.
  If m is o(n/log n), this algorithm is better than linear merging, which is O(m+n)
  If m << n, it's better to do m binary searches in b
*)
val intersect_bisect_adaptive: 'a array -> 'a array -> 'a list

(**
  Like [intersect_bisect_adaptive], but restricted to ranges a[lo..hi], b[b_lo..b_hi]
*)
val intersect_adaptive_range: 'a list -> 'a array -> int -> int -> 'a array -> int -> int -> 'a list

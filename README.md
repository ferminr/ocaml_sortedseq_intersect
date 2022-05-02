# sortedseq_intersect

A divide-and-conquer algorithm in OCaml for the intersection of sorted sequences. The algorithm is described in this paper:

> Fast Intersection Algorithms for Sorted Sequences
>
> Ricardo Baeza-Yates and Alejandro Salinger
>
> Algorithms and Applications 2010 (LNCS 6060, pp. 45–61)

Complexity is O(m log(n/m)), where m,n are the sizes of the sequences. If m is o(n/log n), this algorithm is better than linear merging, which is O(m+n). On the other hand, if m << n, it's better to do m binary searches. An adaptive version of the algorithm further improves the performance on average by switching from divide-and-conquer to linear merge based on the sizes of the sequences.

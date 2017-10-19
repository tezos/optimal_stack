(** An imperative priority queue *)

type ('k,'v) t
val one:      'k -> 'v -> ('k,'v) t
val size:     ('k,'v) t -> int
val pop:      ('k,'v) t -> 'k * 'v
val mem:      ('k,'v) t -> 'k -> bool
val insert:   ('k,'v) t -> 'k -> 'v -> unit
val decrease: ('k,'v) t -> 'k -> 'v -> unit

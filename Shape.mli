(* Shape module interface *)
(* LAP (AMD 2020) *)

(*
01234567890123456789012345678901234567890123456789012345678901234567890123456789
   80 columns
*)

type point = float*float

type shape = Rect of point*point
           | Circle of point*float
           | Union of shape*shape
           | Intersection of shape*shape
           | Subtraction of shape*shape

val hasRect : shape -> bool
val countBasic : shape -> int
val belongs : point -> shape -> bool
val density : point -> shape -> int
val which : point -> shape -> shape list
val minBound : shape -> shape
val grid : int -> int -> float -> float -> shape
val countBasicRepetitions: shape -> int
val svg : shape -> string
val partition : shape -> shape list

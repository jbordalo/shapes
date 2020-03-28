(* Shape module body *)

(* 
Aluno 1: Jacinta Sousa, 55075
Aluno 2: Joao Bordalo, 55697

Comment:

?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????

*)

(*
01234567890123456789012345678901234567890123456789012345678901234567890123456789
   80 columns
*)


(* COMPILATION - How to build this module
         ocamlc -c Shape.mli Shape.ml
*)



(* TYPES *)

type point = float*float;;

type shape = Rect of point*point
           | Circle of point*float
           | Union of shape*shape
           | Intersection of shape*shape
           | Subtraction of shape*shape
;;


(* EXAMPLES *)

let rect1 = Rect ((0.0, 0.0), (5.0, 2.0));;
let rect2 = Rect ((2.0, 2.0), (7.0, 7.0));;
let shape1 = Union (rect1, rect2);;

(* Module List for reference *)
(* http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html *)

(* FUNCTION hasRect *)

let rec hasRect s =
    match s with
          Rect (p,q) -> true
        | Circle (p,f) -> false
        | Union (l,r) -> hasRect l || hasRect r
        | Intersection (l,r) -> hasRect l || hasRect r
        | Subtraction (l,r) -> hasRect l || hasRect r
;;


(* FUNCTION countBasic *)

let rec countBasic s =
    match s with
          Rect (p,q) -> 1
        | Circle (p,f) -> 1
        | Union (l,r) -> countBasic l + countBasic r
        | Intersection (l,r) -> countBasic l + countBasic r
        | Subtraction (l,r) -> countBasic l + countBasic r
;;

(* countBasic shape1 *)

(* FUNCTION belongs *)

(* Distance between two points *)
let dist p1 p2 =
		sqrt ( (fst p1 -. fst p2)**2. +. (snd p1 -. snd p2)**2. )	

let belongs p s =
    match s with
					(* Inclusive inequality due to border belonging to closed shape *)
      		Rect (tl, br) -> fst p >= fst tl && fst p <= fst br && snd p >= snd tl && snd p <= snd br
				| Circle (center, radius) -> dist p center < radius
        | Union (l,r) -> belongs p l || belongs p r
        | Intersection (l,r) -> belongs p l && belongs p r
        | Subtraction (l,r) -> belongs p l && not (belongs p r)
;;

(* val shape1 : shape =
  Union (Rect ((0., 0.), (5., 2.)), Rect ((2., 2.), (7., 7.)))
*)

(* rect1 *)
(* belongs (3., 1.) shape1 *)
(* rect2 *)
(* belongs (3., 5.) shape1 *)
(* none *)
(* belongs (1., 5.) shape1 *)
(* let circle = Circle ((0., 0.),(5.));; *)
(* belongs (0., 4.) circle *)
(* belongs (0., 9.) circle *)



(* FUNCTION density *)

(*
In a basic shape, the density is one if p belongs to the shape; zero outside
In a union, the two partial densities are added up if p is in the intersection zone; outside that common area the density remains.
In an intersection, the two partial densities are added up if p is in the intersection zone; but the density is zero if p is outside the intersection zone.
In a difference, the density remains if p is in the part of the shape that remains; the density is zero if p is in the part that represents erasure.
*)
let density p s =
    0
;;


(* FUNCTION which *)

let which p s =
    [s]
;;


(* FUNCTION minBound *)

let minBound s =
    rect1
;;


(* FUNCTION grid *)

let grid m n a b =
    shape1
;;


(* FUNCTION countBasicRepetitions *)

(* Para testar repeti��es, use a igualdade "=". Por exemplo, se houver dois c�rculos iguais (com o mesmo centro e raio) e as restantes forma b�sicas forem �nicas, ent�o o resultado ser� 2. *)

let countBasicRepetitions s =
    0
;;


(* FUNCTION svg *)

(* https://www.w3schools.com/html/html5_svg.asp *)

let svg s =
    ""
;;


(* FUNCTION partition *)

let partition s =
    [s]
;;


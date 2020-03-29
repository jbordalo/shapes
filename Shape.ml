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

(* MORE EXAMPLES *)

let rectOne = Rect ((0.0, 0.0), (5.0, 4.0));;
let rectTwo = Rect ((2.0, 2.0), (7.0, 7.0));;
let shapeOverlapUnion = Union (rectOne, rectTwo);;
let shapeIntersection = Intersection (rectOne, rectTwo);;

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

(* TODO Is it including all borders? *)
let rec belongs p s =
    match s with
		(* Inclusive inequality due to border belonging to closed shape *)
      	Rect (tl, br) -> fst p >= fst tl && fst p <= fst br && snd p >= snd tl && snd p <= snd br
		| Circle (center, radius) -> dist p center <= radius
        | Union (l,r) -> belongs p l || belongs p r
        | Intersection (l,r) -> belongs p l && belongs p r
        | Subtraction (l,r) -> belongs p l && not (belongs p r)
;;

(* val shapeOverlapUnion : shape =
  Union (Rect ((0., 0.), (5., 4.)), Rect ((2., 2.), (7., 7.)))
*)

(* rectOne = true *)
(* belongs (3., 1.) shapeOverlapUnion *)
(* rectTwo = true *)
(* belongs (6., 3.) shapeOverlapUnion *)
(* none = false *)
(* belongs (1., 15.) shapeOverlapUnion *)
(* let circle = Circle ((0., 0.),(5.));; *)
(* belongs (0., 4.) circle *)
(* belongs (0., 9.) circle *)

(* val shapeIntersection : shape =
  Intersection (Rect ((0., 0.), (5., 4.)), Rect ((2., 2.), (7., 7.))) *)

(* rectOne = false *)
(* belongs (3., 1.) shapeIntersection *)
(* rectTwo = false *)
(* belongs (6., 3.) shapeIntersection *)
(* none = false *)
(* belongs (1., 15.) shapeIntersection *)
(* both = true *)
(* belongs (3., 3.) shapeIntersection *)

(* FUNCTION density *)

(*
In a basic shape, the density is one if p belongs to the shape; zero outside
In a union, the two partial densities are added up if p is in the intersection zone; outside that common area the density remains.
In an intersection, the two partial densities are added up if p is in the intersection zone; but the density is zero if p is outside the intersection zone.
In a difference, the density remains if p is in the part of the shape that remains; the density is zero if p is in the part that represents erasure.
*)
(* TODO Subtraction off the top of my head, may be wrong *)
let rec density p s =
    match s with
      	Rect (_, _) -> if belongs p s then 1 else 0
		| Circle (_, _) -> if belongs p s then 1 else 0
        | Union (l,r) -> density p l + density p r
        | Intersection (l,r) -> if belongs p s then density p l + density p r else 0
        | Subtraction (l,r) -> if belongs p s then density p l else 0
;;

(* Inside rectOne but not rectTwo = 1 *)
(* density (2., 1.) shapeOverlapUnion *)
(* Inside rectTwo but not rectOne = 1 *)
(* density (3., 5.) shapeOverlapUnion *)
(* Inside both = 2 *)
(* density (3., 3.) shapeOverlapUnion *)
(* Outside both = 0 *)
(* density (0., 15.) shapeOverlapUnion *)

(* Inside rectOne but not rectTwo = 0 *)
(* density (2., 1.) shapeIntersection *)
(* Inside rectTwo but not rectOne = 0 *)
(* density (3., 5.) shapeIntersection *)
(* Inside both = 2 *)
(* density (3., 3.) shapeIntersection *)
(* Outside both = 0 *)
(* density (0., 15.) shapeIntersection *)

(* FUNCTION which *)

(* Not sure about shapes of shapes of shapes of..*)
let rec which p s =
    match s with
      	Rect (_, _) -> if belongs p s then [s] else []
		| Circle (_, _) -> if belongs p s then [s] else []
        | Union (l,r) -> which p l @ which p r
        | Intersection (l,r) -> []
        | Subtraction (l,r) -> []
;;

(* Inside rectOne but not rectTwo = [rectOne] *)
(* which (2., 1.) shapeOverlapUnion *)
(* Inside rectTwo but not rectOne = [rectTwo] *)
(* which (3., 5.) shapeOverlapUnion *)
(* Inside both = [rectOne, rectTwo] *)
(* which (3., 3.) shapeOverlapUnion *)
(* Outside both = [] *)
(* which (0., 15.) shapeOverlapUnion *)

(* Inside rectOne but not rectTwo = 0 *)
(* which (2., 1.) shapeIntersection *)
(* Inside rectTwo but not rectOne = 0 *)
(* which (3., 5.) shapeIntersection *)
(* Inside both = 2 *)
(* which (3., 3.) shapeIntersection *)
(* Outside both = 0 *)
(* which (0., 15.) shapeIntersection *)

(* FUNCTION minBound *)

let rectSum r1 r2 =
	match r1, r2 with
		Rect(fr1, sr1), Rect(fr2, sr2) ->
		Rect (
        	( min (fst fr1) (fst fr2 ) , min (snd fr1 ) (snd fr2 ) ),
        	( max (fst sr1 ) (fst sr2 ) , max (snd sr1 ) (snd sr2 ) )
        	 )		
;;

rectSum (Rect ((0.,0.),(3.,3.))) (Rect((3.,2.),(6.,5.)));;

let rec minBound s =
	match s with 
		Rect (_, _) -> s
		| Circle (c, r) -> Rect ((fst c-.r, fst c-.r), (snd c+.r,snd c+.r))
        | Union (l,r) -> rectSum (minBound l) (minBound r)
        | Intersection (l,r) -> rectSum (minBound l) (minBound r)
        | Subtraction (l,r) -> minBound l
;;

minBound (Circle((2.,2.), 2.));;
minBound (Union(Rect ((0.,0.),(3.,3.)), Rect((3.,2.),(6.,5.))));;
(* Not tested for intersection or subtraction *)


(* FUNCTION grid *)

let grid m n a b =
    shape1
;;


(* FUNCTION countBasicRepetitions *)

(* Para testar repetições, use a igualdade "=". Por exemplo, se houver dois círculos iguais (com o mesmo centro e raio) e as restantes forma básicas forem únicas, então o resultado será 2. *)

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


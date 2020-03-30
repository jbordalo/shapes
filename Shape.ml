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
let c1 = Circle((2.0,2.0), 2.0);;
let shape1 = Union (rect1, rect2);;
let shapeSubtract = Subtraction(rect2, c1);;

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
      	Rect (tl, br) -> fst p >= fst tl && fst p <= fst br 
				&& snd p >= snd tl && snd p <= snd br
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
        | Intersection (l,r) -> if belongs p s 
				then density p l + density p r else 0
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

(* Not sure about a lot of shit*)
let rec which p s =
  	if belongs p s then
			match s with
    		Rect(_,_) -> [s]
				| Circle(_,_) -> [s]
				| Union(l,r) -> which p l @ which p r
				| Intersection(l,r) -> which p l @ which p r
				| Subtraction (l,r)-> which p l @ which p r
	else []
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

(* On the subtracted area = 0*)
(* which (3.0,3.0) shapeSubtract *)
(* On the limit of the subtracted area = 0*)
(* which (4.0,2.0) shapeSubtract *)
(* Inside the shape = [rect2] *)
(* which (4.0, 3.0) shapeSubtract *)

(* FUNCTION minBound *)

let rectSum r1 r2 =
	match r1, r2 with
		Rect(fr1, sr1), Rect(fr2, sr2) ->
		Rect (
        	( min (fst fr1) (fst fr2 ) , min (snd fr1 ) (snd fr2 ) ),
        	( max (fst sr1 ) (fst sr2 ) , max (snd sr1 ) (snd sr2 ) )
        	 )
;;

let rectAnd r1 r2 =
	match r1, r2 with
		Rect(fr1, sr1), Rect(fr2, sr2) ->
		Rect (
        	( max (fst fr1) (fst fr2 ) , max (snd fr1 ) (snd fr2 ) ),
        	( min (fst sr1 ) (fst sr2 ) , min (snd sr1 ) (snd sr2 ) )
        	 )
;;

(* rectSum (Rect ((0.,0.),(3.,3.))) (Rect((3.,2.),(6.,5.)));; *)
(* rectAnd (Rect ((0.,0.),(2.,2.))) (Rect((1.,1.),(4.,4.)));; *)

(* A n B = fuck all *)
let rec minBound s =
	match s with
		Rect (_, _) -> s
		| Circle (c, r) -> Rect ((fst c-.r, fst c-.r), (snd c+.r,snd c+.r))
        | Union (l,r) -> rectSum (minBound l) (minBound r)
        | Intersection (l,r) -> rectAnd (minBound l) (minBound r)
        | Subtraction (l,r) -> minBound l
;;

(* minBound (Circle((2.,2.), 2.));; *)
(* minBound (Union(Rect ((0.,0.),(3.,3.)), Rect((3.,2.),(6.,5.))));; *)
(* minBound (Union(Rect ((0.,1.),(2.,3.)), Rect((1.,0.),(3.,4.))));; *)
(* minBound (Union(Rect ((1.,0.),(3.,4.)), Rect((0.,1.),(2.,3.))));; *)
(* minBound (Intersection(Rect ((0.,1.),(2.,3.)), Rect((1.,0.),(3.,4.))));; *)
(* minBound (Intersection(Rect ((1.,0.),(3.,4.)), Rect((0.,1.),(2.,3.))));; *)
(* minBound shapeSubtract *)


(* FUNCTION grid *)
let rec row m n a b =
	let mx = float_of_int m in
		let ny = float_of_int n in
			let both_odd = n mod 2 = 1 && m mod 2 = 1 in
				let both_even = n mod 2 = 0 && m mod 2 = 0 in
		if m = 1 && n mod 2 = 1 
				then Rect((0.0, (ny-.1.0)*.b),(a,ny*.b)) 
		else if m = 2 && n mod 2 = 0
			then Rect(((mx-.1.0)*.a, (ny-.1.0)*.b),(mx*.a, ny*.b))
		else if both_odd || both_even
			then Union (
					Rect(((mx-.1.0)*.a, (ny-.1.0)*.b),(mx*.a, ny*.b)), row (m-1) n a b
				)
		else row (m-1) n a b
;;
(* Both col and row even numbers : Union (Rect ((3., 1.), (4., 2.)), Rect ((1., 1.), (2., 2.)))*)
(* row 4 2 1.0 1.0 *)
(* Both col and row odd numbers : Union (Rect ((6., 4.), (7., 5.)), Union (Rect ((4., 4.), (5., 5.)), Union (Rect ((2., 4.), (3., 5.)), Rect ((0., 4.), (1., 5.))))) *)
(* row 7 5 1.0 1.0 *)
(* Col even and row odd: Union (Rect ((3., 5.), (4., 6.)), Rect ((1., 5.), (2., 6.))) *)
(* row 5 6 1.0 1.0 *)


(* Probably unecessary but wtv*)
let rec col m n a b = 
	let mx = float_of_int m in
		let ny = float_of_int n in
			let both_odd = n mod 2 = 1 && m mod 2 = 1 in
				let both_even = n mod 2 = 0 && m mod 2 = 0 in
		if n = 1 && m mod 2 = 1
			then Rect(((mx-.1.0)*.a, 0.0),(mx*.a, b))
		else if n = 2 && m mod 2 = 0
			then Rect(((mx-.1.0)*.a, (ny-.1.0)*.b),(mx*.a, ny*.b))
		else if both_odd || both_even
			then Union (
					Rect(((mx-.1.0)*.a, (ny-.1.0)*.b),(mx*.a, ny*.b)), col m (n-1) a b
				)
		else col m (n-1) a b
;;
(* Both col and row even numbers : Rect ((3., 1.), (4., 2.) *)
(* col 4 2 1.0 1.0 *)
(* Both col and row odd numbers : Union (Rect ((6., 4.), (7., 5.)), Union (Rect ((6., 2.), (7., 3.)), Rect ((6., 0.), (7., 1.)))) *)
(* col 7 5 1.0 1.0 *)
(* Col even and row odd: Union (Rect ((4., 4.), (5., 5.)), Union (Rect ((4., 2.), (5., 3.)), Rect ((4., 0.), (5., 1.))))*)
(* col 5 6 1.0 1.0 *)

let rec sub m n a b =
	if n = 1 && m = 1 
		then Rect((0.0,0.0),(a,b))
	else if n = 1 
		then row m n a b
	else if m = 1 
		then col m n a b
	else Union(row m n a b, sub m (n-1) a b)
;;

let grid m n a b =  (* @pre: n,m > 0 *)
    Subtraction( Rect((0.0,0.0),((float_of_int m)*.a,(float_of_int n)*.b)), 
		sub m n a b)
;;

(* FUNCTION countBasicRepetitions *)
(* TODO - if there are 4 shapes equal in pairs what number should this function return?*)

(* Para testar repeticoes, use a igualdade "=". Por exemplo, se houver dois circulos iguais (com o mesmo centro e raio) e as restantes forma basicas forem unicas, entao o resultado sera 2. *)

let rec countBasicRepetitions s =
	match s with
	Rect(_,_) -> 0 
	| Circle(_,_) -> 0
	| Union (s1, s2) -> count s1 s2
	| Intersection (s1, s2)-> count s1 s2
	| Subtraction (s1, s2) -> count s1 s2
and count s1 s2 =
	if s1 = s2 
	then 2 + countBasicRepetitions s1 + countBasicRepetitions s2 
	else countBasicRepetitions s1 + countBasicRepetitions s2
;;

(* Two repeated in union = 2*)
(* countBasicRepetitions (Union(Circle((0.0,0.0), 1.0),Circle((0.0,0.0), 1.0))) *)
(* Two repeated and more shapes in s= 2*)
(* countBasicRepetitions (Union(Intersection(Circle((0.0,0.0), 1.0),Circle((0.0,0.0), 1.0)), shape1)) *)
(* More than two repeated = 4*)
(* countBasicRepetitions (Union(Intersection(Circle((0.0,0.0), 1.0),Circle((0.0,0.0), 1.0)), Union(Circle((0.0,0.0), 1.0),Circle((0.0,0.0), 1.0))))*)
(* More than two repeated but paired 2 to 2 = ??? result ??? 4 or 2 ???*)
(* countBasicRepetitions (Union(Intersection(Circle((0.0,0.0), 1.0),Circle((0.0,0.0), 1.0)), Union(Circle((1.0,2.0), 3.0),Circle((1.0,2.0), 3.0))))*)


(* FUNCTION svg *)

let width r = (* @pre: r is Rect(_,_) *)
	match r with
		Rect(l, r) -> fst r -. fst  l
;;

let height r = (* @pre: r is Rect(_,_) *)
	match r with
		Rect(l, r) -> snd r -. snd l
;;

let rec auxSvg s color =
	match s with
		Rect (lt, rb) -> "<rect x=\"" ^ string_of_float (fst lt) ^ "\" y=\"" ^ string_of_float (snd lt) ^ "\" width=\"" ^ string_of_float (width s) ^ "\" height=\"" ^ string_of_float (height s) ^ "\" fill=\"" ^ color ^ "\"/>"
		| Circle (c, r) -> "<circle cx=\"" ^ string_of_float ( fst c ) ^ "\" cy=\"" ^ string_of_float ( snd c ) ^ "\" r=\"" ^ string_of_float r ^ "\" fill=\""^ color ^"\" />"
        | Union (l,r) -> auxSvg l color ^ auxSvg r color
        | Intersection (l,r) -> auxSvg (Subtraction(l, Subtraction(l, r))) color
        | Subtraction (l,r) -> auxSvg l "black" ^ auxSvg r "white"
;;

let svg s =
	let minimum = minBound s in
    	"<html><body><svg width=\"" ^ string_of_float (width minimum) ^ "\" height=\"" ^ string_of_float (height minimum) ^ "\">"^ auxSvg s "black" ^"</svg></body></html>"
;;

output_string stdout (svg (Rect((100.,100.),(300.,300.))));;
output_string stdout (svg (Circle((100.,100.),300.)));;
output_string stdout (svg (Union(Rect((100.,100.),(300.,300.)),Circle((50.,50.),150.))));;
output_string stdout (svg (Subtraction(Rect((100.,90.),(300.,520.)),Circle((50.,50.),150.))));;
output_string stdout (svg (Subtraction(Rect((100.,90.),(300.,520.)), Union(Rect((50., 60.),(36.,40.)) ,Rect((50.,50.),(150., 150.))))));;
output_string stdout (svg (grid 8 8 100. 100.));;
output_string stdout (svg (Union((grid 8 8 100. 100.), Subtraction(Circle((400.,400.), 200.), Rect((300.,300.),(500.,500.))))));;
output_string stdout (svg (Intersection(Rect((40.,40.),(500.,500.)), Circle((50.,50.), 500.))));;

(* FUNCTION partition *)

let emptyIntersection s1 s2 = 
	match (minBound (Intersection(s1, s2))) with
		Rect( tl, br) ->
				let p = ( min (fst tl)(fst br) +. (abs_float ((fst tl)-.(fst br))), min (snd tl)(snd br) +. abs_float ((snd tl)-.(snd br))) in
				not (belongs p s1 && belongs p s2)
;;

(* Test empty intersection: (Very Basic) *)
(* True *)
(* emptyIntersection (Rect((1.0, 0.0 ), (3.0, 2.0))) (Circle ((6.0, 6.0), 1.0))*)
(* False *)
(* emptyIntersection (Rect((1.0, 0.0 ), (3.0, 2.0))) (Rect((2.0, 0.0 ), (6.0, 2.0)))*)

(* TODO: Intersection/Subtraction with Unions *)
let partition s =
	match s with 
	Rect(_,_) -> [s]
	|Circle (_,_) -> [s]
	| Union(s1,s2) -> if (emptyIntersection s1 s2) then partition s1 @ partition s2 else [s]
	| Intersection(s1,s2) -> if (emptyIntersection s1 s2) then [] else [s]
	| Subtraction(s1,s2) -> if (emptyIntersection s1 s2) then partition s1 else [s]
;;

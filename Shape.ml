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

(* TODO Not sure about a lot of shit*)
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

(* TODO CHANGE TO ENUNCIADO'S REQUIREMENTS *)
let rec minBound s =
	match s with
		Rect (_, _) -> s
		| Circle (c, r) -> Rect ((fst c-.r, fst c-.r), (snd c+.r,snd c+.r))
        | Union (l,r) -> rectSum (minBound l) (minBound r)
        | Intersection (l,r) -> rectSum (minBound l) (minBound r)
        | Subtraction (l,r) -> rectSum (minBound l) (minBound r)
;;

let rec minBoundSvg s =
	match s with
		Rect (_, _) -> s
		| Circle (c, r) -> Rect ((fst c-.r, fst c-.r), (snd c+.r,snd c+.r))
        | Union (l,r) -> rectSum (minBoundSvg l) (minBoundSvg r)
        | Intersection (l,r) -> rectAnd (minBoundSvg l) (minBoundSvg r)
        | Subtraction (l,r) -> minBoundSvg l
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
		if m = 1 && n mod 2 = 0
				then Rect((0.0, (ny-.1.0)*.b),(a,ny*.b)) 
		else if m = 2 && n mod 2 = 1
			then Rect(((mx-.1.0)*.a, (ny-.1.0)*.b),(mx*.a, ny*.b))
		else if both_odd || both_even
			then row (m-1) n a b
		else Union (
						Rect(((mx-.1.0)*.a, (ny-.1.0)*.b),(mx*.a, ny*.b)), row (m-1) n a b
			   )
;;
(* Both col and row even numbers : Union (Rect ((2., 1.), (3., 2.)), Rect ((0., 1.), (1., 2.)))*)
(* row 4 2 1.0 1.0 *)
(* Both col and row odd numbers : Union (Rect ((5., 4.), (6., 5.)), Union (Rect ((3., 4.), (4., 5.)), Rect ((1., 4.), (2., 5.)))) *)
(* row 7 5 1.0 1.0 *)
(* Col even and row odd: Union (Rect ((4., 5.), (5., 6.)), Union (Rect ((2., 5.), (3., 6.)), Rect ((0., 5.), (1., 6.)))) *)
(* row 5 6 1.0 1.0 *)


(* Probably unecessary but wtv*)
let rec col m n a b = 
	let mx = float_of_int m in
		let ny = float_of_int n in
			let both_odd = n mod 2 = 1 && m mod 2 = 1 in
				let both_even = n mod 2 = 0 && m mod 2 = 0 in
		if n = 1 && m mod 2 = 0
			then Rect(((mx-.1.0)*.a, 0.0),(mx*.a, b))
		else if n = 2 && m mod 2 = 1
			then Rect(((mx-.1.0)*.a, (ny-.1.0)*.b),(mx*.a, ny*.b))
		else if both_odd || both_even
			then col m (n-1) a b
		else Union (
						Rect(((mx-.1.0)*.a, (ny-.1.0)*.b),(mx*.a, ny*.b)), col m (n-1) a b
				)
;;
(* Both col and row even numbers : Rect ((3., 0.), (4., 1.)) *)
(* col 4 2 1.0 1.0 *)
(* Both col and row odd numbers : Union (Rect ((6., 3.), (7., 4.)), Rect ((6., 1.), (7., 2.))) *)
(* col 7 5 1.0 1.0 *)
(* Col even and row odd: Union (Rect ((4., 5.), (5., 6.)), Union (Rect ((4., 3.), (5., 4.)), Rect ((4., 1.), (5., 2.))))*)
(* col 5 6 1.0 1.0 *)

let rec grid m n a b =
	let empty = Rect ((1.0, 1.0), (0.0, 0.0)) in 
		if n <= 1 && m <= 1 
			then empty
		else if n = 1 
			then row m n a b
		else if m = 1 
			then col m n a b
		else Union(row m n a b, grid m (n-1) a b)
;;
(* TEST : grid 6 6 1.0 1.0;; *)
(* Union
 (Union (Rect ((4., 5.), (5., 6.)),
   Union (Rect ((2., 5.), (3., 6.)), Rect ((0., 5.), (1., 6.)))),
 Union
  (Union (Rect ((5., 4.), (6., 5.)),
    Union (Rect ((3., 4.), (4., 5.)), Rect ((1., 4.), (2., 5.)))),
  Union
   (Union (Rect ((4., 3.), (5., 4.)),
     Union (Rect ((2., 3.), (3., 4.)), Rect ((0., 3.), (1., 4.)))),
   Union
    (Union (Rect ((5., 2.), (6., 3.)),
      Union (Rect ((3., 2.), (4., 3.)), Rect ((1., 2.), (2., 3.)))),
    Union
     (Union (Rect ((4., 1.), (5., 2.)),
       Union (Rect ((2., 1.), (3., 2.)), Rect ((0., 1.), (1., 2.)))),
     Union (Rect ((5., 0.), (6., 1.)),
      Union (Rect ((3., 0.), (4., 1.)), Rect ((1., 0.), (2., 1.)))))))))*)

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

let genID = 
    let idBase = ref 0 in
        fun () ->
            idBase := !idBase + 1;
            "id" ^ (Printf.sprintf "%04d" !idBase)
;;

let mask s id = 
    "\n<mask id=\""^id^"\">\n
    "^s^"
  \n</mask>\n"
;;

let rec maskAux s id = 
	match s with
	Rect (lt, rb) -> "<rect x=\"" ^ string_of_float (fst lt) ^ "\" y=\"" ^ string_of_float (snd lt) ^ "\" width=\"" ^ string_of_float (width s) ^ "\" height=\"" ^ string_of_float (height s) ^ "\" mask=\"url(#" ^ id ^ ")\"/>\n"
		| Circle (c, r) -> "<circle cx=\"" ^ string_of_float ( fst c ) ^ "\" cy=\"" ^ string_of_float ( snd c ) ^ "\" r=\"" ^ string_of_float r ^ "\" mask=\"url(#"^ id ^")\" />\n"
        | Union (l,r) -> maskAux l id ^ maskAux r id
        | Intersection (l,r) -> failwith "inter inside sub"
			(*maskAux (Subtraction(l, Subtraction(l, r))) string_of_int(Random.int 5000)*)
			
        | Subtraction (l,r) -> failwith "sub inside sub"
			(* let id = string_of_int(Random.int 5000) in
			maskAux l ^ maskAux l id ^ mask ((auxSvg l "white" ) ^ (auxSvg r "black")) id *)

let rec auxSvg s color =
	match s with
		Rect (lt, rb) -> "<rect x=\"" ^ string_of_float (fst lt) ^ "\" y=\"" ^ string_of_float (snd lt) ^ "\" width=\"" ^ string_of_float (width s) ^ "\" height=\"" ^ string_of_float (height s) ^ "\" fill=\"" ^ color ^ "\"/>\n"
		| Circle (c, r) -> "<circle cx=\"" ^ string_of_float ( fst c ) ^ "\" cy=\"" ^ string_of_float ( snd c ) ^ "\" r=\"" ^ string_of_float r ^ "\" fill=\""^ color ^"\" />\n"
        | Union (l,r) -> auxSvg l color ^ auxSvg r color
        | Intersection (l,r) ->
			let id = genID() in
				let ss = Subtraction(l, Subtraction(l,r)) in
			auxSvg (ss) color
		(* | Subtraction (l, Subtraction(ll, rr)) ->
			let id = genID() in
			maskAux l id ^ mask ((auxSvg ll "white" ) ^ (auxSvg rr "black")) id ^ *)
        | Subtraction (l,r) ->
			let id = genID() in
			maskAux l id ^ mask ((auxSvg l "white" ) ^ (auxSvg r "black")) id
;;

let svg s =
	let minimum = minBoundSvg s in
    	"<html>\n<body>\n<svg width=\"" ^ string_of_float (width minimum) ^ "\" height=\"" ^ string_of_float (height minimum) ^ "\">\n"^ auxSvg s "black" ^"</svg>\n</body>\n</html>"
;;

output_string stdout (svg (Rect((100.,100.),(300.,300.))));;
output_string stdout (svg (Circle((100.,100.),300.)));;
output_string stdout (svg (Union(Rect((100.,100.),(300.,300.)),Circle((50.,50.),150.))));;
output_string stdout (svg (Subtraction(Rect((100.,90.),(300.,520.)),Circle((50.,50.),150.))));;
output_string stdout (svg (Subtraction(Rect((100.,90.),(300.,520.)), Union(Rect((50., 60.),(36.,40.)) ,Rect((50.,50.),(150., 150.))))));;
output_string stdout (svg (grid 8 8 100. 100.));;
output_string stdout (svg (Union((grid 8 8 100. 100.), Subtraction(Circle((400.,400.), 200.), Rect((300.,300.),(500.,500.))))));;
output_string stdout (svg (Intersection(Rect((40.,40.),(500.,500.)), Circle((50.,50.), 500.))));;
output_string stdout (svg (Intersection(Circle((50.,50.), 500.), Rect((40.,40.),(500.,500.)))));;
output_string stdout (svg (Subtraction(Rect((100.,90.),(300.,520.)),Circle((50.,50.),150.))));;
output_string stdout (svg (Subtraction(Circle((50.,50.),150.), Rect((100.,90.),(300.,520.)))));;
output_string stdout (svg (Subtraction(Circle((50.,50.),50.), Subtraction(Circle((40.,40.),40.),Rect((70.,10.),(90.,30.))))));;
output_string stdout (svg (Subtraction(Subtraction(Circle((40.,40.),40.),Rect((70.,10.),(90.,30.))), Circle((50.,50.), 50.))));;

(* FUNCTION partition *)

let emptyIntersection s1 s2 = 
	match (minBoundSvg (Intersection(s1, s2))) with
		Rect( tl, br) ->
			let x = (fst tl +. fst br)/. 2.0 in
				let y =  (snd tl +. snd br)/. 2.0 in
					let p = (x, y) in
						not (belongs p s1 && belongs p s2)
;;

(* Test empty intersection: (Very Basic) *)
(* True *)
(* emptyIntersection (Rect((1.0, 0.0 ), (3.0, 2.0))) (Circle ((6.0, 6.0), 1.0))*)
(* False *)
(* emptyIntersection (Rect((1.0, 0.0 ), (3.0, 2.0))) (Rect((2.0, 0.0 ), (6.0, 2.0)))*)
(* False *)
(* emptyIntersection (Circle ((2.0,2.0), 1.0)) (Circle ((4.0, 2.0), 1.0))*)
(* False *)
(* emptyIntersection (Rect((1.0, 0.0 ), (2.0, 3.0))) (Rect((2.0, 0.0 ), (3.0, 3.0)))*)

(*
let rec ISUnion s = (*Pre: s is either a Intersection or a Subtraction *)
	match s with 
	Intersection (s1,s2)-> 
	| Subtraction (s1, s2 ->  
;;
*)
let aux s2 s3 = 
	 if (emptyIntersection s3 s2)
				 then [s3] else partition (Subtraction (s3, s2)) 
				;;

(* TODO: Intersection/Subtraction with Unions *)
let rec partition s =
	match s with 
	Rect(_,_) -> [s]
	|Circle (_,_) -> [s]
	| Union(s1,s2) -> if (emptyIntersection s1 s2) then partition s1 @ partition s2 else [s]
	| Intersection(s1,s2) -> if (emptyIntersection s1 s2) then [] else [s]
	| Subtraction(s1,s2) -> if (emptyIntersection s1 s2) 
			then partition s1 
		else match s1 with Union (s3,s4) -> if (emptyIntersection s3 s4) 
			then (aux s2 s3) @ (aux s2 s4)
			else [s]
		|_-> [s]
;;

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
let rect1 = Rect ((0.0, 0.0), (0.1, 0.1)) ;;
let rect2 = Rect ((2.0, 2.0), (7.0, 7.0)) ;;
let circle = Circle ((1.0, 1.0), 0.1) ;;
let shape1 = Union (rect1, rect2) ;;
let shape2 = Union (shape1, shape1) ;;
let shape3 = Union (circle, shape2) ;;

(* MORE EXAMPLES *)

let rectOne = Rect ((0.0, 0.0), (5.0, 4.0));;
let rectTwo = Rect ((2.0, 2.0), (7.0, 7.0));;
let shapeOverlapUnion = Union (rectOne, rectTwo);;
let shapeIntersection = Intersection (rectOne, rectTwo);;

(* FUNCTION hasRect *)

let rec hasRect s =
    match s with
		Rect (_,_) -> true
        | Circle (_,_) -> false
        | Union (l,r)
        | Intersection (l,r)
        | Subtraction (l,r) -> hasRect l || hasRect r
;;


(* FUNCTION countBasic *)

let rec countBasic s =
    match s with
		Rect (_,_)
        | Circle (_,_) -> 1
        | Union (l,r)
        | Intersection (l,r)
        | Subtraction (l,r) -> countBasic l + countBasic r
;;

(* countBasic shape1 *)
(* countBasic  (Intersection(Rect((5., -110.0),(15., 110.0)),
Union(Subtraction(Circle((10.0, 10.0), 100.0), Circle((10.0, 10.0), 90.0)),
Union(Subtraction(Circle((10.0, 10.0), 80.0), Circle((10.0, 10.0), 70.0)),
Union(Subtraction(Circle((10.0, 10.0), 60.0), Circle((10.0, 10.0), 60.0)),
Union(Subtraction(Circle((10.0, 10.0), 40.0), Circle((10.0, 10.0), 30.0)),
Subtraction(Circle((10.0, 10.0), 20.0), Circle((10.0, 10.0), 10.0))))))));; *)

(* FUNCTION belongs *)

(* Distance between two points *)
let dist p1 p2 =
	sqrt ( (fst p1 -. fst p2)**2. +. (snd p1 -. snd p2)**2. )

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
(* belongs (0.0, 0.0) shape3;; *)
(* belongs (10.0, 10.0) shape3;; *)

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

(* TODO Subtraction off the top of my head, may be wrong *)
let rec density p s =
    if belongs p s then
	match s with
      	Rect (_, _)
		| Circle (_, _) -> 1
        | Union (l,r)
        | Intersection (l,r) -> density p l + density p r
        | Subtraction (l,r) -> density p l
	else 0
;;

(* let s = Subtraction(Intersection(Circle((40.,40.),60.), Rect((20.,20.),(80.,90.))), Circle((50.,40.),10.));; *)

(* output_string stdout (svg s) *)

(* density (51.,41.) s = 0 *)
(* density (39.,39.) s = 2 *)

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

(* density (20.,20.) (Subtraction(Circle((20.,20.), 20.), Circle((20.,20.), 10.))) *)
(* density (30.,30.) (Subtraction(Circle((20.,20.), 20.), Circle((20.,20.), 10.))) *)
(* density (500.,500.) (Subtraction(Circle((20.,20.), 20.), Circle((20.,20.), 10.))) *)

(* FUNCTION which *)

let rec which p s =
  	if belongs p s then
			match s with
    			Rect(_,_)
				| Circle(_,_) -> [s]
				| Union(l,r)
				| Intersection(l,r)
				| Subtraction (l,r)-> which p l @ which p r
	else []
;;

(* Testing for union *)
(* On first rectangle *)
(* which (1.,1.) shapeOverlapUnion;; *)
(* On second rectangle *)
(* which (6.,4.) shapeOverlapUnion;; *)
(* On both rectangles (intersection) *)
(* which (4., 3.) shapeOverlapUnion;; *)
(* On none *)
(* which (6.,1.) shapeOverlapUnion;; *)

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

(* On the subtracted area = 0 *)
(* which (3.0,3.0) shapeSubtract *)
(* On the limit of the subtracted area = 0 *)
(* which (4.0,2.0) shapeSubtract *)
(* Inside the shape = [rect2] *)
(* which (4.0, 3.0) shapeSubtract *)

(* FUNCTION minBound *)

(* Gives a rectangle that includes both rectangles it gets *)
let rectSum r1 r2 = (* pre: r1 and r2 are both Rect *)
	match r1, r2 with
		Rect(fr1, sr1), Rect(fr2, sr2) ->
		Rect (
        	(min (fst fr1) (fst fr2) , min (snd fr1) (snd fr2)),
        	(max (fst sr1) (fst sr2) , max (snd sr1) (snd sr2))
        	 )
		| _ -> failwith "r1 and r2 not Rects"
;;

(* Gives a rectangle that bounds the intersection of the two rectangles it gets *)
let rectAnd r1 r2 = (* pre: r1 and r2 are both Rect *)
	match r1, r2 with
		Rect(fr1, sr1), Rect(fr2, sr2) ->
		Rect (
        	(max (fst fr1) (fst fr2) , max (snd fr1) (snd fr2)),
        	(min (fst sr1) (fst sr2) , min (snd sr1) (snd sr2))
        	 )
		| _ -> failwith "r1 and r2 not Rects"
;;

(* rectSum (Rect ((0.,0.),(3.,3.))) (Rect((3.,2.),(6.,5.)));; *)
(* rectAnd (Rect ((0.,0.),(2.,2.))) (Rect((1.,1.),(4.,4.)));; *)

(* Minimum bounding box for the subshapes of the shape be them white or black *)
let rec minBound s =
	match s with
		Rect (_, _) -> s
		| Circle (c, r) -> Rect ((fst c-.r, snd c-.r), (fst c+.r,snd c+.r))
        | Union (l,r)
        | Intersection (l,r)
        | Subtraction (l,r) -> rectSum (minBound l) (minBound r)
;;

(* Tighter minBound that binds the black part, used for svg *)
let rec minBoundSvg s =
	match s with
		Rect (_, _) -> s
		| Circle (c, r) -> Rect ((fst c-.r, snd c-.r), (fst c+.r,snd c+.r))
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
(* Col even and row odd: Union (Rect ((4., 5.), (5., 6.)), Union (
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
Rect ((4., 3.), (5., 4.)), Rect ((4., 1.), (5., 2.))))*)
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


(* Makes a list with all the basic shapes out of a shape *)
let rec createListFromShape s =
	match s with
	Rect(_,_) 
	| Circle(_,_) -> [s]
	| Union (s1, s2)
	| Intersection (s1, s2)
	| Subtraction (s1, s2) -> createListFromShape s1 @ createListFromShape s2
;;

(* Counts how many times the element s appears on the list l *)
let rec countAppearances s l =
	match l with
        [] -> 0
    	| x::xs -> if s = x 
						then 1 + countAppearances s xs
					else countAppearances s xs
;;

(* Counts each element's appearance and filters it out *)
let rec countAppearancesFilter l = 
	match l with
	[] -> 0
	| x::xs ->
		let a = countAppearances x l in
			if a = 1 
				then countAppearancesFilter (List.filter (fun i -> i <> x) xs)
			else
				a + countAppearancesFilter (List.filter (fun i -> i <> x) xs)
;;

(* aux [1;1;2;3;3;3;3];; *)
(* aux [1;2;3];; *)
(* aux [1;1;2;3];; *)
(* aux [1;1;2;2;3];; *)

let countBasicRepetitions s =
	match s with
	Rect(_,_)
	| Circle(_,_) -> 0
	| Union (s1, s2)
	| Intersection (s1, s2)
	| Subtraction (s1, s2) -> 
		if s1 = s2 then 2
		else
			countAppearancesFilter (createListFromShape s)
;;


(* countBasicRepetitions shape3;; *)

(* Two repeated in union = 2 *)
(* countBasicRepetitions (Union(Circle((0.0,0.0), 1.0),Circle((0.0,0.0), 1.0))) *)
(* Two repeated and more shapes in s = 2 *)
(* countBasicRepetitions (Union(Intersection(Circle((0.0,0.0), 1.0),Circle((0.0,0.0), 1.0)), rect1)) *)
(* More than two repeated = 4 *)
(* countBasicRepetitions (Union(Intersection(Circle((0.0,0.0), 1.0),Circle((0.0,0.0), 1.0)), Union(Circle((0.0,0.0), 1.0),Circle((0.0,0.0), 1.0)))) *)
(* More than two repeated but paired 2 to 2 =  4 *)
(* countBasicRepetitions (Union(Intersection(Circle((0.0,0.0), 1.0),Circle((0.0,0.0), 1.0)), Union(Circle((1.0,2.0), 3.0),Circle((1.0,2.0), 3.0)))) *)


(* FUNCTION svg *)

(* Absolute value for floats *)
let absFloat x = 
	if x < 0. then (~-. x) else x
;;

(* Gives the width of a rectangle *)
let width r = (* @pre: r is a Rect *)
	match r with
		Rect(l, r) -> absFloat (fst r -. fst l)
		| _ -> failwith "r not Rect"
;;

(* Gives the height of a rectangle *)
let height r = (* @pre: r is a Rect *)
	match r with
		Rect(l, r) -> absFloat (snd r -. snd l)
		| _ -> failwith "r not Rect"
;;

let genID = 
    let idBase = ref 0 in
        fun () ->
            idBase := !idBase + 1;
            "id" ^ (Printf.sprintf "%04d" !idBase)
;;

(* Surrounds the string s with a mask element with the given id *)
let mask s id = 
    "\n\t<mask id=\""^id^"\">\n"^s^"\t</mask>\n"
;;

(* Auxiliar mask function for creating elements with a mask id *)
let rec maskAux s id = 
	match s with
    	Rect (lt, rb) -> 
    		"\t<rect x=\"" ^ string_of_float (fst lt) 
    		^ "\" y=\"" ^ string_of_float (snd lt) ^
    		 "\" width=\"" ^ string_of_float (width s) ^
    		 "\" height=\"" ^ string_of_float (height s) ^
    		 "\" mask=\"url(#" ^ id ^ ")\"/>\n"
		| Circle (c, r) -> 
			"\t<circle cx=\"" ^ string_of_float ( fst c ) ^
			 "\" cy=\"" ^ string_of_float ( snd c ) ^
			 "\" r=\"" ^ string_of_float r ^
			 "\" mask=\"url(#"^ id ^")\" />\n"
        | Union (l,r) -> maskAux l id ^ maskAux r id
        | Intersection (l,r)
        | Subtraction (l,r) -> failwith "Handled outside this function"
;;

(* Svg auxiliary function to create the elements *)
let rec auxSvg s color =
	match s with
		Rect (lt, rb) -> "\t<rect x=\"" ^ string_of_float (fst lt) ^
		 "\" y=\"" ^ string_of_float (snd lt) ^
		 "\" width=\"" ^ string_of_float (width s) ^
		 "\" height=\"" ^ string_of_float (height s) ^
		 "\" fill=\"" ^ color ^ "\"/>\n"
		| Circle (c, r) -> "\t<circle cx=\"" ^ string_of_float (fst c) ^
		 "\" cy=\"" ^ string_of_float ( snd c ) ^
		 "\" r=\"" ^ string_of_float r ^
		 "\" fill=\""^ color ^"\" />\n"
        | Union (l,r) -> auxSvg l color ^ auxSvg r color
        | Intersection (l,r) ->
			let ss = Subtraction(l, Subtraction(l,r)) in
			auxSvg ss color
		| Subtraction (Subtraction(a, b), c) ->
			let id = genID() in
			maskAux a id ^ mask ((auxSvg a "white" ) ^
			 (auxSvg (Union(b, c)) "black")) id
		| Subtraction (Intersection(li, ri),r) ->
			(* These expressions are mathematically equivalent *)
			(* through simple rules of set theory *)
			let ss = Subtraction(li, Union(Subtraction(li,ri), r)) in
			auxSvg ss color
		| Subtraction (l,r) ->
			let id = genID() in
			maskAux l id ^ mask ((auxSvg l "white" ) ^ (auxSvg r "black")) id
;;

(* Gives the topleftmost x coordinate of the rectangle *)
let topleftmost r = (* r is a Rect *)
	match r with
	Rect(x,y) -> fst x
	| _ -> failwith "r is not a Rect"
;;

(* Gives the bottomrightmost y coordinate of the rectangle *)
let bottomrightmost r = (* r is a Rect *)	
	match r with
	Rect(x,y) -> snd y
	| _ -> failwith "r is not a Rect"
;;

(* Didn't remove the x. from output since HTML accepts it *)
let svg s =
	let minimum = minBoundSvg s in
    		"<html>\n<body>\n\t<svg width=\"" ^ 
				string_of_float (topleftmost minimum +. width minimum) ^
    		 "\" height=\"" ^
				string_of_float (bottomrightmost minimum +. height minimum) ^
			 "\">\n" 
    		^ auxSvg s "black" ^ "\t</svg>\n</body>\n</html>"
;;

(* output_string stdout (svg (Rect((100.,100.),(300.,300.))));; *)
(* output_string stdout (svg (Circle((100.,100.),300.)));; *)
(* output_string stdout (svg (Union(Rect((100.,100.),(300.,300.)),Circle((50.,50.),150.))));; *)
(* output_string stdout (svg (Subtraction(Rect((100.,90.),(300.,520.)),Circle((50.,50.),150.))));; *)
(* output_string stdout (svg (Subtraction(Rect((100.,90.),(300.,520.)), Union(Rect((50., 60.),(36.,40.)) ,Rect((50.,50.),(150., 150.))))));; *)
(* output_string stdout (svg (grid 8 8 100. 100.));; *)
(* output_string stdout (svg (Union((grid 8 8 100. 100.), Subtraction(Circle((400.,400.), 200.), Rect((290.,290.),(510.,510.))))));; *)
(* Border problem between here *)
(* output_string stdout (svg (Intersection(Rect((40.,40.),(500.,500.)), Circle((50.,50.), 500.))));; *)
(* output_string stdout (svg (Intersection(Circle((50.,50.), 500.), Rect((40.,40.),(500.,500.)))));; *)
(* Border problem between here *)
(* output_string stdout (svg (Subtraction(Rect((100.,90.),(300.,520.)),Circle((50.,50.),150.))));; *)
(* output_string stdout (svg (Subtraction(Circle((50.,50.),150.), Rect((100.,90.),(300.,520.)))));; *)

(* Dreamworks kid *)
(* output_string stdout (svg (Subtraction(Circle((50.,50.),50.), Subtraction(Circle((40.,40.),40.),Rect((70.,10.),(90.,30.))))));; *)

(* Moon *)
(* output_string stdout (svg (Subtraction(Subtraction(Circle((40.,40.),40.),Rect((70.,10.),(90.,30.))), Circle((50.,50.), 50.))));; *)

(* Donut *)
(* output_string stdout (svg (Subtraction(Circle((80.,80.), 60.), Circle((80.,80.), 20.))));; *)
(* output_string stdout (svg (Subtraction(Circle((80.,80.), 60.), Rect((115.,20.), (160.,80.)))));; *)
(* output_string stdout (svg (Subtraction(Subtraction(Circle((80.,80.), 60.),Circle((80.,80.),20.)), Rect((115.,20.), (160., 80.)))));; *)

(* output_string stdout (svg (Intersection(Rect((100.,200.),(600.,600.)), Rect((400.,100.),(900.,400.)))));; *)
(* output_string stdout 
(svg (Subtraction(Intersection(Rect((100.,200.),(600.,600.)), Rect((400.,100.),(900.,400.))), Circle((600.,300.), 100.))));; *)

(* Two lobed afro *)
(* output_string stdout (svg (Subtraction(Union(Circle((200.,200.),100.),Circle((300.,200.),100.)),Rect((200.,200.),(300.,400.)))));; *)

(* A-B such that AnB is empty *)
(* output_string stdout (svg (Subtraction(Rect((10.,10.),(40.,40.)), Rect((50.,10.),(70.,30.)))));; *)
(* output_string stdout (svg (Subtraction(Rect((10.,10.),(40.,40.)), Circle((60.,50.),10.))));; *)
(* output_string stdout (svg (Subtraction(Rect((10.,10.),(40.,40.)), Union(Rect((50.,10.),(70.,30.)), Circle((60.,50.),10.)))));; *)

(* let intr = Intersection(Circle((300.,200.), 100.), Circle((400.,200.),100.));; *)
(* Has a border *)
(* output_string stdout(svg intr);; *)
(* output_string stdout(svg (Intersection(Circle((400.,200.), 100.), Circle((300.,200.),100.))));; *)

(* Works but solving border issues *)
(* output_string stdout (svg (Subtraction(Rect((200.,100.),(500.,300.)), intr)));; *)

(* *)
(* output_string stdout (svg (Union(Circle((500.,500.), 100.), (Subtraction(Circle((500.,500.), 50.),Circle((500.,500.), 100.))))));; *)

(* output_string stdout (svg (Intersection(Rect((5., -110.0),(15., 110.0)),
Union(Subtraction(Circle((10.0, 10.0), 100.0), Circle((10.0, 10.0), 90.0)),
Union(Subtraction(Circle((10.0, 10.0), 80.0), Circle((10.0, 10.0), 70.0)),
Union(Subtraction(Circle((10.0, 10.0), 60.0), Circle((10.0, 10.0), 60.0)),
Union(Subtraction(Circle((10.0, 10.0), 40.0), Circle((10.0, 10.0), 30.0)),
Subtraction(Circle((10.0, 10.0), 20.0), Circle((10.0, 10.0), 10.0)))))))));; *)

(* density (220.,220.) (Subtraction(Union(Circle((200.,200.),100.),Circle((300.,200.),100.)),Rect((200.,200.),(300.,400.))));; *)
(* which (220.,220.) (Subtraction(Union(Circle((200.,200.),100.),Circle((300.,200.),100.)),Rect((200.,200.),(300.,400.))));; *)

(* density (190.,190.) (Subtraction(Union(Circle((200.,200.),100.),Circle((300.,200.),100.)),Rect((200.,200.),(300.,400.))));; *)
(* which (190.,190.) (Subtraction(Union(Circle((200.,200.),100.),Circle((300.,200.),100.)),Rect((200.,200.),(300.,400.))));; *)

(* density (250.,190.) (Subtraction(Union(Circle((200.,200.),100.),Circle((300.,200.),100.)),Rect((200.,200.),(300.,400.))));; *)
(* which (250.,190.) (Subtraction(Union(Circle((200.,200.),100.),Circle((300.,200.),100.)),Rect((200.,200.),(300.,400.))));; *)

(* FUNCTION partition *)

let boundaries r1 r2 = (* pre: r1 and r2 are rects *)
	match r1, r2 with
		Rect((x, y), (f,g)), Rect((z, w), (l,p)) ->
			let a = z>x in (*x1r2 > x1r1*)
				 let b = l<f in (*x2r2 < x2r1*)
				let c =  w>y in (*y1r2 > y1r1*)
					let d = p<g in (*y2r2 < y2r1*)
					if a then ( 
						if not (b) then (*x1r2 > x1r1  &  x2r2 > =x2r1 *) 
							(if not (c || d) then Rect((x,y),(z,g)) (* r'1 *)
							else r1)
						else (* A e B *) (*x1r2 > x1r1  &  x2r2 < x2r1 *) 
							if c && d then  
								r1
							else
								(* union *)
								Union(Rect((x,y),(z,g)),Rect((l, y),(f,g)))
						)
					else ( 
						if b then (*x1r2 <= x1r1 && x2r2 < x2r1*)
							if (c && d) then r1
							else Rect((l, y),(f,g)) (* r'2 *)
						else (* nA e nB *) (*x1r2 <= x1r1 && x2r2 >= x2r1*)
							if (c && d) then  
								Union(Rect((x,y),(f, w)), Rect((x, p),(f,g)))
								else if not d then Rect((x, y),(f,w)) 
									else if not c then Rect((x, p),(f,g)) 
									else r1
							)
		| _ -> failwith "r1 and r2 not Rect"
;;


let rec boundInt r1 r2 = (* pre: r1 and r2 are both rects *)
	match r1, r2 with
		Rect(fr1, sr1), Rect(fr2, sr2) ->
		Rect (
        	( max (fst fr1) (fst fr2 ) , max (snd fr1 ) (snd fr2 ) ),
        	( min (fst sr1 ) (fst sr2 ) , min (snd sr1 ) (snd sr2 ) )
        	 )
		| Union(r3,r4), Rect(fr1, sr1) -> 
								Union ( boundInt r3 r2, boundInt r4 r2)
		| _ -> failwith "r1 and r2 not Rect"
;;


let rec boundP s =
	match s with
		Rect (_, _) -> s
		| Circle (c, r) -> Rect ((fst c-.r, snd c-.r), (fst c+.r,snd c+.r))
    | Union (l,r) -> rectSum (boundP l) (boundP r)
    | Intersection (l,r) -> boundInt (boundP l) (boundP r)
    | Subtraction (l,r) -> boundaries (boundP l) (boundP r)
;;

(* Tests : *)
(* 1. Expected : Union(Rect ((2., 2.), (4., 4.)), Rect ((5., 2.), (7., 4.)))*)
(* boundaries (Rect((2.,2.),(7.,4.))) (Rect((4.,1.), (5.,5.)) );; *)
(* 2. Expected : Union (Rect ((2., 1.), (4., 2.)), Rect ((2., 4.), (4., 5.)))*)
(* boundaries (  Rect((2.,1.),(4.,5.)) ) ( Rect((1.,2.),(5.,4.)) );; *)
(* 3. Expected : Rect ((3., 4.), (6., 6.)) *)
(* boundaries (Rect((3.,3.), (6.,6.)) ) (Rect((2.,2.),(7.,4.)));; *)
(* 4. Expected : Rect ((2., 6.), (4., 8.)) *)
(* boundaries (  Rect((2.,6.),(4.,9.)) ) ( Rect((1.,8.),(5.,10.)) );; *)
(* 5. Expected : Rect ((3., 2.), (7., 4.)) *)
(* boundaries (Rect((2.,2.),(7.,4.))) (Rect((1.,1.), (3.,5.)) );; *)
(* 6. Expected : Rect ((1., 12.), (3., 14.)) *)
(* boundaries (  Rect((1.,12.),(4.,14.)) ) ( Rect((3.,11.),(5.,15.)) );; *)

let rec emptyIntersection s1 s2 = 
	match (boundP (Intersection(s1, s2))) with
		Rect(tl, br) ->
			let x = (fst tl +. fst br)/. 2.0 in
				let y =  (snd tl +. snd br)/. 2.0 in
					let p = (x, y) in
						(auxl p s1 s2) && (auxl p s2 s1)
		| _ -> failwith "Bounds have to be a rectangle"
and auxl p s s0 =
	match s with 
	Union (l, r) -> if emptyIntersection l r then 
		(emptyIntersection l s0 && emptyIntersection r s0) 
		else (not (belongs p s))
	| _ -> (not (belongs p s))
;;

(* emptyIntersection (Union(Circle((2.,2.), 1.),Circle((5.,2.), 1.))) (Union( Circle((2.,4.), 2.),Circle((5.,4.),2.)));; *)
(* boundP (Intersection(Union(Circle((2.,2.), 1.),Circle((5.,2.), 1.)), Union( Circle((2.,4.), 2.),Circle((5.,4.),2.))));; *)

(* emptyIntersection (Circle((2.,2.), 1.)) (Union( Circle((2.,4.), 2.),Circle((5.,4.),2.)));; *) 
(* emptyIntersection (Circle((5.,2.), 1.)) (Union( Circle((2.,4.), 2.),Circle((5.,4.),2.)));; *)

(* boundP ( Intersection(Union(Circle((2.,3.), 1.),Circle((6.,3.), 1.)), Union(Circle((4.,4.), 2.), Rect((2.,5.),(6.,6.)))));; *)
(* emptyIntersection (Union(Circle((2.,3.), 1.),Circle((6.,3.), 1.))) (Union(Circle((4.,4.), 2.), Rect((2.,5.),(6.,6.))));; *)

(* emptyIntersection(Circle((2.,3.), 1.)) (Circle((4.,4.), 2.));; *)
(* emptyIntersection(Circle((6.,3.), 1.)) (Circle((4.,4.), 2.));; *)
(* emptyIntersection (Rect((1.0, 0.0 ), (3.0, 2.0))) (Circle ((6.0, 6.0), 1.0));; *)
(* emptyIntersection (Rect((1.0, 0.0 ), (3.0, 2.0))) (Rect((2.0, 0.0 ), (6.0, 2.0)));; *)
(* emptyIntersection (Circle ((2.0,2.0), 1.0)) (Circle ((4.0, 2.0), 1.0));; *)
(* emptyIntersection (Rect((1.0, 0.0 ), (2.0, 3.0))) (Rect((2.0, 0.0 ), (3.0, 3.0)));; *)
(* emptyIntersection (Union(Circle((2.,3.), 1.),Circle((6.,3.), 1.))) (Circle((4.,4.), 2.));; *)

(* Test empty intersection: (Very Basic) *)
(* True *)
(* emptyIntersection (Rect((1.0, 0.0 ), (3.0, 2.0))) (Circle ((6.0, 6.0), 1.0))*)
(* False *)
(* emptyIntersection (Rect((1.0, 0.0 ), (3.0, 2.0))) (Rect((2.0, 0.0 ), (6.0, 2.0)))*)
(* False *)
(* emptyIntersection (Circle ((2.0,2.0), 1.0)) (Circle ((4.0, 2.0), 1.0))*)
(* False *)
(* emptyIntersection (Rect((1.0, 0.0 ), (2.0, 3.0))) (Rect((2.0, 0.0 ), (3.0, 3.0))) *)
(* TODO: Intersection/Subtraction with Unions *)
let rec partition s =
	match s with 
	Rect(_,_)
	|Circle (_,_) -> [s]
	| Union(s1,s2) -> if (emptyIntersection s1 s2) then 
							partition s1 @ partition s2 else [s]
	| Intersection(s1,s2) -> 
			if (emptyIntersection s1 s2) 
				then [] 
			else (
					match s1, s2 with 
						Union (l,r),_ -> inter l r s2 s
						| _, Union (l,r) -> inter l r s1 s
						| _,_ ->[s] 
			)
	| Subtraction(s1,s2) -> if (emptyIntersection s1 s2) 
			then partition s1 
		else (	
			let a = boundP s in 
				match s1,s2, a with 
					Union (l,r),_ ,_ -> subtr l r s2 s
					| _, Union (l,r),_ -> subtr l r s1 s
					| _,_ ,Union(l, r) -> 
									[Subtraction(l,s2); Subtraction(r,s2)]
					| _,_,_ -> [s]
		)		
and subaux s2 s3 = 
	 if (emptyIntersection s3 s2)
			then [s3] else partition (Subtraction (s3, s2)) 
and intaux s2 s3 = 
	if (emptyIntersection s3 s2)
			then [s3] else partition (Intersection (s3, s2)) 
and inter l r s1 s =
	if (emptyIntersection l r) 
			then (intaux s1 l) @ (intaux s1 r)
	else [s]
and subtr l r s1 s = 
	if (emptyIntersection l r) 
			then (subaux s1 l) @ (subaux s1 r)
	else [s]
	;;

(* Tests: *)
(* list = [Circle ((4., 4.), 2.)] *)
(* partition (Circle((4.,4.), 2.));; *)
(* list = [Rect ((3.3, 3.), (6., 5.))] *)
(* partition (Rect ((3.3,3.),(6.,5.)));; *)
(* list = [Union (Rect ((0., 0.), (5., 2.)), Rect ((2., 2.), (7., 7.)))] *)
(* partition (Union(rect1, rect2));; *)
(* list =[Subtraction (Circle ((2., 2.), 1.)  Union (Circle ((2., 4.), 2.), Circle ((5., 4.), 2.)));
 Subtraction (Circle ((5., 2.), 1.),  Union (Circle ((2., 4.), 2.), Circle ((5., 4.), 2.)))] *)
(* partition (Subtraction(Union(Circle((2.,2.), 1.),Circle((5.,2.), 1.)), Union( Circle((2.,4.), 2.),Circle((5.,4.),2.))));; *)
(* list = [Subtraction (Rect ((2., 2.), (4., 4.)), Rect ((4., 1.), (5., 5.)));
 Subtraction (Rect ((5., 2.), (7., 4.)), Rect ((4., 1.), (5., 5.)))]*)
(* partition (Subtraction( Rect((2.,2.),(7.,4.)) ,Rect((4.,1.), (5.,5.))));; *)
(* list = [Subtraction (Rect ((2., 1.), (4., 2.)), Rect ((1., 2.), (5., 4.)));
 Subtraction (Rect ((2., 4.), (4., 5.)), Rect ((1., 2.), (5., 4.)))]*)
(* partition (Subtraction(  Rect((2.,1.),(4.,5.)) , Rect((1.,2.),(5.,4.)) ));; *)
(* list = [Subtraction (Rect ((3., 4.), (6., 6.)), Rect ((2., 2.), (7., 4.)))] *)
(* partition (Subtraction(Rect((3.,3.), (6.,6.)) , Rect((2.,2.),(7.,4.))));; *)
(* list = [Subtraction (Rect ((2., 6.), (4., 8.)), Rect ((1., 8.), (5., 10.)))] *)
(* partition (Subtraction(  Rect((2.,6.),(4.,9.)) , Rect((1.,8.),(5.,10.)) ));; *)
(* list = [Subtraction (Rect ((1., 12.), (3., 14.)), Rect ((3., 11.), (5., 15.)))] *)
(* partition (Subtraction(   Rect((1.,12.),(4.,14.)) ,  Rect((3.,11.),(5.,15.)) ));; *)


(* list = [Intersection (Circle ((2., 3.), 1.), Circle ((4., 4.), 2.));
 Intersection (Circle ((6., 3.), 1.), Circle ((4., 4.), 2.))]  for both vv*)
(* partition(Intersection((Circle((4.,4.), 2.)),Union(Circle((2.,3.), 1.),Circle((6.,3.), 1.))));; *)
(* partition(Intersection(Union(Circle((2.,3.), 1.),Circle((6.,3.), 1.)) ,(Circle((4.,4.), 2.))));; *)

(* list =[Intersection (Circle ((2., 3.), 1.),  Union (Circle ((4., 4.), 2.), Rect ((2., 5.), (6., 6.))));
Intersection (Circle ((6., 3.), 1.), Union (Circle ((4., 4.), 2.), Rect ((2., 5.), (6., 6.))))]*)
(* partition(Intersection(Union(Circle((2.,3.), 1.),Circle((6.,3.), 1.)), Union(Circle((4.,4.), 2.), Rect((2.,5.),(6.,6.)))));; *)

(* list = [Subtraction (Circle ((2., 3.), 1.), Circle ((4., 4.), 2.));
 Subtraction (Circle ((6., 3.), 1.), Circle ((4., 4.), 2.))]*)
(* partition (Subtraction(Union(Circle((2.,3.), 1.),Circle((6.,3.), 1.)), (Circle((4.,4.), 2.))));; *)

(* partition (Subtraction(Union(Circle((200.,200.),100.),Circle((300.,200.),100.)),Rect((200.,200.),(300.,400.))));; *)

(* list =[Subtraction (Subtraction (Circle ((80., 80.), 60.), Circle ((80., 80.), 20.)),
 Rect ((115., 20.), (160., 80.)))] *)
(* partition (Subtraction(Subtraction(Circle((80.,80.), 60.),Circle((80.,80.),20.)), Rect((115.,20.), (160., 80.))));; *)
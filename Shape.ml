(* Shape module body *)

(*
Aluno 1: Jacinta Sousa, 55075
Aluno 2: Joao Bordalo, 55697

Comment:

In the function emptyIntersections we are aware that our function boundP 
returns both Rectangles and Unions of Rectangles but only account for the 
Rectangles therefore any more intricate shapes may not be well represented in
the output of the partitions function (that might as well not cover all the 
cases possible for the description of said function)

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


(* FUNCTION density *)

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

(* Gives a rectangle that bounds the intersection of the rectangles it gets *)
let rectAnd r1 r2 = (* pre: r1 and r2 are both Rect *)
	match r1, r2 with
		Rect(fr1, sr1), Rect(fr2, sr2) ->
		Rect (
        	(max (fst fr1) (fst fr2) , max (snd fr1) (snd fr2)),
        	(min (fst sr1) (fst sr2) , min (snd sr1) (snd sr2))
        	 )
		| _ -> failwith "r1 and r2 not Rects"
;;


(* Minimum bounding box for the subshapes of the shape (white or black) *)
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

(* FUNCTION countBasicRepetitions *)

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


(* FUNCTION svg *)

(* Absolute value function for floats *)
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

(* Auxiliary function - deals with the Rects and Unions*)
let rec boundAux s1 s2 f = 
	match s1,s2 with 
		Rect(fr1, sr1), Rect(fr2, sr2) -> 	(f s1 s2)
		| Union(r3,r4), Rect(fr1, sr1) -> 
								Union ( boundAux r3 s2 f, boundAux r4 s2 f)
		| Rect(fr1, sr1),  Union(r3,r4)-> 
			Union ( boundAux r3 s1 f, boundAux r4 s1 f)
		| _ ,_-> failwith "s1 or s2 not Rect"
;;

(*boundP is a function that either returns a Rect *)
(* or a Union of Rects (when the shape has two separate shapes) *)
let rec boundP s =
	match s with
		Rect (_, _) -> s
		| Circle (c, r) -> Rect ((fst c-.r, snd c-.r), (fst c+.r,snd c+.r))
    | Union (l,r) -> boundAux (boundP l) (boundP r) (rectSum)
    | Intersection (l,r) -> boundAux(boundP l) (boundP r) (rectAnd)
    | Subtraction (l,r) -> boundaries (boundP l) (boundP r)
;;


(* emptyIntersection - checks if the intersection of two shapes is empty *)
(* Returns true if so, false if not*)
let rec emptyIntersection s1 s2 = 
	match (boundP (Intersection(s1, s2))) with
		Rect(tl, br) ->
			let x = (fst tl +. fst br)/. 2.0 in
				let y =  (snd tl +. snd br)/. 2.0 in
					let p = (x, y) in
						(auxl p s1 s2) && (auxl p s2 s1)
		|_  -> failwith "Bounds has to be a rectangle"
and auxl p s s0 =
	match s with 
	Union (l, r) -> if emptyIntersection l r then 
		(emptyIntersection l s0 && emptyIntersection r s0) 
		else (not (belongs p s))
	| _ -> (not (belongs p s))
;;


let rec partition s =
	match s with 
	Rect(_,_)
	|Circle (_,_) -> [s]
	| Union(s1,s2) -> if (emptyIntersection s1 s2) then 
							partition s1 @ partition s2 else [s]
			(* If there is a union between two shapes that don't intersect *)
			(* those two shapes are islands, so we call partition again*)
			(* if not the union is one single island *)
	| Intersection(s1,s2) -> 
			if (emptyIntersection s1 s2) 
				then [] 
			else (
					match s1, s2 with 
						Union (l,r),_ -> inter l r s2 s
						| _, Union (l,r) -> inter l r s1 s
						| _,_ ->[s] 
			)
			(* If an intersection is empty *)
			(* it means there is no black shape displayed, *)
			(* If not, both sides of the intersection *)
			(* are compared to a union,*)
			(* a similar process to Union is initiated if so. *)
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
			(* If a subtraction is empty it means*)
			(* the first shape is displayed fully, *)
			(* If not, both sides of the intersection*)
			(*  are compared to a union,*)
			(* a similar process to Union is initiated if so. *)
			(* If not it is checked whether *)
			(* the subtraction creates 2 shapes *)
			(* (Returned by a union on boundP) or one. *) 
			
			(* Intersection auxiliary functions *)
and intaux s2 s3 = 
	if (emptyIntersection s3 s2)
			then [] else partition (Intersection (s3, s2)) 
			(* intaux - Checks if the intersection is empty, *)
			(* between one of the sides of the Union *)
			(* (that is one of the sides of the Intersection) *)
			(* and the other side of the Intersection *)
			(* is empty, if it is there is no black shape*)
			(* else call partition on the Intersection of the arguments *)
and inter l r s1 s =
	if (emptyIntersection l r) 
			then (intaux s1 l) @ (intaux s1 r)
	else [s] 
			(* inter - Checks if the two sides of a union *)
			(* (Union that is one of the sides (or both) *)
			(* of the Intersection) are islands*)
			(* calling intaux if so, *)
			(* or 'inserting' the whole Intersection in the list*)
			
			(* Subtraction auxiliary functions *)
and subaux s2 s3 = 
	 if (emptyIntersection s3 s2)
			then [s3] else partition (Subtraction (s3, s2)) 
			(* subaux - Checks if the intersection between *)
			(* one of the sides of the Union *)
			(* (that is one of the sides of the Subtraction) *)
			(* and the other side of the Subtraction*)
			(* is empty, if it is then the black shape *)
			(* is the side of the Union*)
			(* else call partition on the Subtraction of the arguments *)
and subtr l r s1 s = 
	if (emptyIntersection l r) 
			then (subaux s1 l) @ (subaux s1 r)
	else [s]
			(* subtr - Checks if the two sides of a union *)
			(* (Union that is one of the sides(or both)*)
			(*  of the Subtraction) are islands*)
			(* calling subaux if so, *)
			(* or 'inserting' the whole Subtracion in the list*)	
;;

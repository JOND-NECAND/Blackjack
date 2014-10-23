(*----------------Définition des types--------------------*)
(*type nom = string  ;;*)
(*type argent = Entier of int ;;*)
type couleur = Pique | Coeur | Carreau | Trefle;;
type valeur = Roi | Dame | Valet | As | Nombre of int;;
type carte = {vale : valeur ; coul : couleur};;
type liste = Nil | String of int * liste;;
type main = carte list;;
type joueur = { n : string ; m : main ; ar : int ; num : int};;
type croupier = { nc : string ; mc : main };;
type table = { jl: joueur list ; cr: croupier };;
(*---------------------------------------*)

Random.self_init ();;            
let creerCarte v c = {vale= v ; coul=c};;
let creerJoueur nj mj arj numj = {n=nj ; m=mj ; ar=arj ; num=numj};;

let rec creerValnb n c = 
	if (n=0) then []
	else if (n=1) then (creerCarte As c)::(creerValnb (n-1) c)
		else if (n=11) then (creerCarte Valet c)::(creerValnb (n-1) c)			
			else if (n=12) then (creerCarte Dame c)::(creerValnb (n-1) c)
				else if (n=13) then (creerCarte Roi c)::(creerValnb (n-1) c)
					else (creerCarte (Nombre n) c)::(creerValnb (n-1) c);;


let jeudecartes n =  (creerValnb n Trefle)@(creerValnb n Coeur)@(creerValnb n Pique)@(creerValnb n Carreau);;


(*----------------------------------------------------------------------------*)
(*----------------Fonction pour supprimer un ième éleément--------------------*)
(*'a' est la liste, res est le i-ème élément, 'r' est le reste de la liste, 't' est la teteliste *)
let rec supprime a res = match a with
	[]-> []
	| t::r -> if res = 0 then
			 r
		else 
			t::(supprime r (res-1));;





(*------------------Mélanger des éléments au hasard--------------------------*)
(*'x' est la liste d'éléments et 'l' est le nb de fois qu'on mélange *)
let rec melanger x l = 
	if l=0 then x else
	let res  = Random.int (List.length x) in
	
    	melanger ((List.nth x res)::(supprime x res)) (l-1);;

let x = jeudecartes 13;;
melanger x 1000;;
let x = melanger (jeudecartes 13) 1000;;


(*--------------------------Choix de Distribuer-----------------------------*)
(* list.tl = tout ce qui nest pas la tete / list.hd = tete de liste*)
(*'x'=Liste / 'l'=Liste d'arrivée(joueur) / 'n' = nb de distributions *)


let rec distribuer x l n =
	if n = 0 then l
	else distribuer (List.tl x) ((List.hd x)::l) (n-1);;
		
		
let l1 = [];;
let l2 = [];;
let a = distribuer x l1 2;;
(* on enlève les deux carte du paquet init*)
let x = List.tl (List.tl x);;
let l = List.length x;;
let b = distribuer x l2 2;;
(* on enlève les deux carte du paquet init*)
let x = List.tl (List.tl x);;
let l = List.length x;;
(*----------------------------------------------------------------------------*)
(*--------------------------Choi du As---------------------------------------*)
let rec choixas x = match x with
	As ->	print_string( "Le As prend quelle valeur? 1 ou 11?");
		let j = read_int () in	
			if (j = 11) then 11
			else if (j = 1) then 1
				else begin
					print_string "Valeur invalide";
					choixas x
				end
	| Roi| Dame| Valet -> 10
	| Nombre n -> n;;
		


(*-------------------------Comptage de Points-----------------------------*)

		
let rec comptage x = 
	match x with 
	[] -> 0
	|t::r -> choixas(t.vale) + (comptage r);;
		
let av = comptage a;;
let bv = comptage b;;

(*----------------------------------------------------------------------------*)
(*----------------------------Perdant ou poursuivre---------------------------*)
let result x c = if (comptage x > 21) then print_string "P E R D U !! Vous sortez du jeu!"						
					
		else
			if ((comptage c <= comptage x) && (comptage x <21)) then print_string( "Vous pouvez continuer!")
			else ();;

(*let resultpr x c = if (comptage x = 21) then print_string "B L A C K J A C K!!";
			else (result x c);;*)



(*----------------------------------------------------------------------------*)
(*-------------------------------Nombre de joueurs----------------------------*)
(*Nb *)

let rec nombrejoueur () = 
	print_string ("Entrez le nombre de joueurs");
	let j = read_int () in
		if ((j>1) && (j<=7)) then j
		else begin			
			print_string ("Nombre de joueurs trop grand, veuillez entrez un autre nombre");
			nombrejoueur()
		end;;
				
(*let nombrejoueur () = j ;;*)

(*----------------------------------------------------------------------------*)
(*-------------------------------Liste de joueurs-----------------------------*)
(* n = nombre de joueur *)

let rec listejoueur j n = match n with
	1 -> [creerJoueur]
(*j mj arj 1]*)
	|2|3|4|5|6|7 ->(creerJoueur(*j mj arj n*)::[(creerJoueur nj mj arj 1)]; listejoueur j (n-1)
	


 (* (j <> 1) then (creerJoueur "Michel" [] 0 j)::(listejoueur(j-1))
			else  (creerJoueur "Michel" [] 0 j)::[];;*)
	

let j = nombrejoueur();;
print_string("chier");;
let x = listejoueur j;;
(*-------------------------argent------------------------------------------*)
(* x est la mise du tour ; a est l'argent de base; mj est la main du joueur; mjc la main du croupier 
la fonction retourne le new argent de base
la fonction regroupe le gain, la perte et la mise du joueur*)
(*let rec resultat x a mj mjc = 
	if ((a.ar)<= 0) 
		then failwith "joueur éliminé" 
	else
		if ((comptage mj.m < 21)&&(comptage mj.m >= comptage mjc.mc))
			then (a.ar + x) 
		else 
			if (comptage mj.m = 21)
				then (a.ar + int_of_float(1.5*.float_of_int(x))) 
			else (a.ar - x);;*)


  

(*-------------------------------------------------------------------------*)

(*--------------------------Tours-----------------------------*)

(*------------------Demande du joueur-------------------*)

(*--------------------Croupier--------------------------*)

(*---------------------Abandon--------------------------*)

(*--------------Carte croupier & Carte joeur-------------------*)

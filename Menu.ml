type couleur = Pique | Coeur | Carreau | Trefle;;
type valeur = Roi | Dame | Valet | As | Nombre of int;;
type carte = {vale : valeur ; coul : couleur};;
type liste = Nil | String of int * liste;;
type main = carte list;;
type joueur = { n : string ; m : main ; ar : int ; num : int};;
type croupier = { nc : string ; mc : main };;
type table = { jl: joueur list ; cr: croupier };;

(*---------------- menu-------------*)
(* faire les patterns avec des read_line() et "transformer " les entiers en et autres caracteres en chaine de caractères pour traiter tous les cas de saisie*)

let rec accueil () =
	print_newline();
	print_newline(); 
	print_string("              BLACKJACK              ");
	print_newline();
	print_newline();
	print_string("1 - Parametrage");
	print_newline();
	print_string("2 - Jeu  ");
	print_newline();
	print_string("Que voulez-vous faire ?");
	print_newline();
	print_newline();

	match read_int() with
		1 ->	print_string("             Menu de parametrage              ");
			print_newline();
			print_newline();
			print_string(" 1- Nombre de joueur ? ");
			print_newline();
			print_string(" 2- Argent de départ ? ");
			print_newline();
			print_string(" 3- Nom joueur(s) ? ");
			print_newline();
			print_string(" 4 -Valeur de l'AS ? ");
			print_newline();
			print_string(" 5 - Nombre paquet de carte ? ");
			print_newline();
			print_string(" 6 - Parametres par defaut") ;
			print_newline();
			print_string(" 7 - Accueil") ;
			print_newline();
			print_string("Que voulez-vous faire ?");
			print_newline();
			print_newline();


			let rec mid () = match read_int() with

				|1 -> print_string("Combien de joueurs serez vous durant la partie? (entre 1 et 7)");
					let creerJoueur nj mj arj numj = {n=nj ; m=mj ; ar=arj ; num=numj} in
					let n = read_int() in 
					let rec listejoueur j n = 
						match n with
							1 -> let creerJoueur = {creerJoueur with num = 1 } in creerJoueur::[]
							|2|3|4|5|6|7 ->let creerJoueur = {creerJoueur with num = n } in creerJoueur::(listejoueur j (n-1))
							|_ -> print_string("Erreur valeur"); listejoueur j n  
					in listejoueur j n 



					(*let rec nombrejoueur () = 
						print_string("Entrez le nombre de joueurs");

						match read_int() with
						1|2|3|4|5|6|7 -> print_int(read_int() );
						|_ -> print_string("valeur incorrecte "); nombrejoueur () *)
						(*let j = read_int () in
							if ((j>1) && (j<=7)) then j
							else 			
								(print_string("Nombre de joueurs trop grand, veuillez entrez un autre nombre"); nombrejoueur () ) 
								   
					 in nombrejoueur () *)   

				 |2 -> print_string("Combien d'argent de départ voulez vous?"); 
						
					let creerJoueur = { creerJoueur with ar = read_int()} in
					print_string("Etes- vous sur du montant? (o/n)");
					print_newline();
					print_int(creerJoueur.ar);
						let rec choix () = match read_line() with
							o -> mid ()
							|n -> print_string("Veuillez saisir un montant"); print_newline(); j x 
							|_ -> print_string("Veuillez saisir une autre réponse "); print_newline(); choix ()  
						  
   
				|3 -> print_string("Quels sont les noms des joueurs?");

				|4 -> print_string("Quelle valeur prendre l'AS ? (1 ou 11) ");
					let rec choixas x = match x with 
						As -> print_string ("Le As prend quelle valeur? 1 ou 11?"); 
							let j = read_int () in	
								if (j = 11) then 11
								else if (j = 1) then 1
									else 
										(print_string( "Valeur invalide")
										choixas x )
									
						| Roi| Dame| Valet -> 10
						| Nombre n -> n

				|5 -> print_string("Combien de paquet de carte voulez-vous? (entre 1 et 4)");

				(*|6 -> ;*)

				|7 -> accueil ()

				|_ -> print_string("entrez une autre valeur");print_newline();print_newline(); mid () 
 
			in mid () 


		|2 -> print_string("jeu");

		|_ -> accueil () 

in accueil () ;; 




(*-----------------------------------------------------------*)
(*let rec listejoueur j n = match n with
	1 -> [creerJoueur]
(*j mj arj 1]*)
	|2|3|4|5|6|7 -> (creerJoueur)::[]; listejoueur j (n-1)
	|_ -> print_string("Erreur valeur"); listejoueur j n 	*)	
			
	


(*		

(*-------------------------Créer nom ---------------------------------------*)
let creerNom x  = { n = x };;


(*-------------------------Créer nom Croupier -----------------------------*)
let creerNomCr n = { nc = n };;


(*-------------------------Argent -----------------------------------------*)
(* a est la somme de base du joueur *)

let creerArgent a = { ar = a } ;;

(*----------------------Table---------------------------------------------*)
let creerTable jol cro = { jl = jol ; cr = cro };;


(*----------------------Liste de joueur(s)-------------------------------*)
(* l une liste vide ; j un joueur *)

let rec listejoueurs l j  =
	 match l with
	[] -> [j.joueur];
	|joueur::[r] -> if  (List.lenght > 7) then 
			(print_string("Vous avez entrez trop de joueurs veuillez recommencer"); print_newline() ; print_newline(); listejoueurs l joueur ;)
			else (listejoueurs::[j.joueur]);; 


				

*)




		

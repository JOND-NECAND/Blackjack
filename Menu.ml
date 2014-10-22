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
			print_string(" 4 - Nombre paquet de carte ? ");
			print_newline();
			print_string(" 5 - Parametres par defaut") ;
			print_newline();
			print_string(" 6 - Accueil") ;
			print_newline();
			print_string("Que voulez-vous faire ?");
			print_newline();
			print_newline();


			let rec mid () = match read_int() with
				1 -> print_string("Combien de joueurs serez vous durant la partie? (entre 1 et 7)"); 
				|2 -> print_string("Combien d'argent de départ voulez vous?");
					let j = {j with ar = read_int()};  
					print_string("Etes- vous sur du montant? (o/n)");
					print_newline();
					print_int(creerArgent ())
						let rec choix () = match read_line with
							o -> mid ();
							|n -> print_string("Veuillez saisir un montant"); print_newline(); creerArgent ();
							|_ -> print_string("Veuillez saisir une autre réponse "); print_newline(); choix ();   
				|3 -> print_string("Quels sont les noms des joueurs?");
				|4 -> print_string("Combien de paquet de carte voulez-vous? (entre 1 et 4)");
				(*|5 -> ;*)
				|6 -> accueil ()
				|_ -> print_string("entrez une autre valeur");print_newline();print_newline(); mid ()  
			in mid()


		|2 -> print_string("jeu")
		|_ -> accueil ();;
		
			
	


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
			print_string("Vous avez entrez trop de joueurs veuillez recommencer"); print_newline() ; print_newline(); listejoueurs l joueur ;
			else listejoueurs::[j.joueur];; 


				

*)




		

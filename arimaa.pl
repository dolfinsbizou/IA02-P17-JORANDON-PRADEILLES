:- module(bot,
      [  get_moves/3
      ]).

% Exemple of variable
% gamestate: [side, [captured pieces] ] (e.g. [silver, [ [0,1,rabbit,silver],[0,2,horse,silver] ]) 
% board: [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]

%%% Fonctions utilitaires %%%

% concat_list(Result, List1, List2) : concatène deux listes. %
concat_list(Y, [], Y).
concat_list([X1|L], [X1|X2], Y):- concat_list(L, X2, Y).

% length_list(Result, List) : retourne la taille d'une liste. %
length_list(0, []).
length_list(N, [_|Q]):-
	length_list(M, Q),
	N is M+1.

% nth(Index, List, Elem) : retourne l'index d'un élément donné, ou l'élément donné pour un index.
nth(1, [X|_], X).
nth(N, [X|Y], E):-
	length_list(Max, [X|Y]),
	gen_numeric(N, 1, Max),
	N > 1,
	M is N-1,
	nth(M, Y, E).

% gen_numeric(Result, Begin, End) : Génère un entier entre les deux bornes. %
gen_numeric(Result, Result, _).
gen_numeric(Result, Min, Max):- Min2 is Min+1, Min2 =< Max, gen_numeric(Result, Min2, Max).

% get_opponents(Result, Board) : récupère les opposants (gold) sur le board. %
get_opponents([],[]).
get_opponents([[Y,X,N,gold]|Queue],[[Y,X,N,gold]|Remaining]):- get_opponents(Queue, Remaining).
get_opponents(Queue,[[_,_,_,silver]|Remaining]):- get_opponents(Queue, Remaining).

% get_pieces(Result, Board) : récupère ses propres pièces (silver) sur le board. %
get_pieces([],[]).
get_pieces([[Y,X,N,silver]|Queue],[[Y,X,N,silver]|Remaining]):- get_pieces(Queue, Remaining).
get_pieces(Queue,[[_,_,_,gold]|Remaining]):- get_pieces(Queue, Remaining).

% get_by_type(Result, Board, Type) : récupère les pièces d'un type (lapin, etc.) donné sur le board. %
get_by_type([],[],_).
get_by_type([[Y,X,Type,S]|Queue],[[Y,X,Type,S]|Remaining], Type):- get_by_type(Queue, Remaining, Type).
get_by_type(Queue,[[_,_,_,_]|Remaining], Type):- get_by_type(Queue, Remaining, Type).

% get_on_coord(Result, Coord, Board) : récupère une pièce sur les coordonnées données si elle existe. %
get_on_coord([Y,X,Type,Side], [X,Y], [[Y,X,Type,Side]|_]):- !.
get_on_coord(Result, Coord, [_|Remaining]):- get_on_coord(Result, Coord, Remaining).

% get_adjacent_pieces(Result, Coord, Board) : Retourne la liste des pièces adjacentes sur le board à une coordonnée donnée. get_adjacent_piece peut aussi vérifier si il existe une pièce adjacente d'une position donnée. %
get_adjacent_pieces(Result, Coord, Board):- setof(Adj, get_adjacent_piece(Adj, Coord, Board), Result).
get_adjacent_piece(Result, [X1,Y1], Board):- distance(1, [X1,Y1], [X2,Y2]), get_on_coord(Result, [X2,Y2], Board).

% distance(Distance, Point1, Point2) : Compute la distance entre deux couples de coordonnées. Fonctionne aussi en mode générateur. %
distance(D, [X1,Y1], [X2,Y2]):- gen_numeric(X2, 0, 7), gen_numeric(Y2, 0, 7), gen_numeric(X1, 0, 7), gen_numeric(Y1, 0, 7), abs(X2-X1, DX), abs(Y2-Y1, DY), D is DX+DY.

% distance_on_board(Distance, Point1, Point2, Board) : Compute la distance entre deux couples de coordonnées compte tenu du plateau. %
distance_on_board(Distance, Point1, [X,Y], Board):- distance(Distance, Point1, [X,Y]), \+ get_on_coord(_, [X,Y], Board).
% TODO pour l'instant on élimine juste les destinations correspondant à des pièces : c'est évidemment incomplet, à voir si on améliore ça pour avoir une vraie fonction digne de ce nom (éventuellement regarder le code du jeu pour voir comment eux font. Sinon quand il fera la décomposition en mouvement, en prenant en compte le plateau il risque parfois de créer des déplacements de plus de 4 mouvements. On peut alors décider de tronquer les mouvements après le 4e.)

% compare_types(Result, Type1, Type2) : Retourne 1 si le type 1 est plus fort que le type 2, -1 si le type 2 est plus fort que le type 1, 0 si ils sont égaux.
compare_types(0, Type, Type):- !.
compare_types(R, Type1, Type2):-
	nth(X1, [rabbit, cat, dog, horse, camel, elephant], Type1),
	nth(X2, [rabbit, cat, dog, horse, camel, elephant], Type2),
	(X1>X2 -> R is 1 ; R is -1), !.

% can_move(Piece, Board) : s'unifie si une pièce donnée peut bouger.
can_move([Y,X,Type,_], Board):-
	get_adjacent_pieces(Adj, [X,Y], Board),
	Init1 is 4,
	(X is 0 -> Init2 is Init1-1 ; Init2 is Init1),
	(X is 7 -> Init3 is Init2-1 ; Init3 is Init2),
	(Y is 0 -> Init4 is Init3-1 ; Init4 is Init3),
	(Y is 7 -> Init is Init4-1 ; Init is Init4),
   	can_move_internal(Init, Adj),
	\+ stuck_by_opponent(Type, Adj, 1).
can_move_internal(Val, []):- Val > 0.
can_move_internal(Val, [_|Remaining]):- Val1 is Val-1, can_move_internal(Val1, Remaining).
stuck_by_opponent(_, [], X):- X > 1, !.
stuck_by_opponent(Type, [[_,_,T,gold]|Remaining], Act):- compare_types(R, T, Type), (R is 1 -> Next is Act*2 ; Next is Act), stuck_by_opponent(Type, Remaining, Next), !.

% get_moves_for_given_piece(Result, Piece, Board) : Retourne la liste des coups possibles pour une pièce donné sur le board. %
get_moves_for_given_piece([], Piece, Board):- \+ can_move(Piece, Board).
get_moves_for_given_piece(Result, Piece, Board):- can_move(Piece, Board), setof(Move, get_move_for_given_piece(Move, Piece, Board), Result).
get_move_for_given_piece([[X1,Y1], [X2,Y2]], [Y1,X1,_,_], Board):- distance_on_board(D, [X1, Y1], [X2, Y2], Board), D < 5, D > 0.
% TODO Primordial il faut aussi faire la partie pour push

% get_possible_moves(Result, Board_curr, Board) : Retourne la liste des coups possibles pour toutes les pièces argent du tableau sous forme d'une liste de [Origine, Destination].
get_possible_moves([], [], _).
get_possible_moves(Result, [[Y,X,Type,silver]|Remaining], Board):- get_moves_for_given_piece(Result2, [Y,X,Type,silver], Board), get_possible_moves(Result1, Remaining, Board), concat_list(Result, Result2, Result1). 
get_possible_moves(Result, [[_,_,_,gold]|Remaining], Board):- get_possible_moves(Result, Remaining, Board). 

% get_dangerous_holes_by_side(Result, Side, Board) : Une fonction qui retourne les positions des trous noirs sans pièces d'un côté donné autour. La question étant de pouvoir savoir si un trou noir présente un risque pour le côté donné. %
get_dangerous_holes_by_side(Result, Side, Board):- get_dangerous_holes_by_side_internal(Result, Side, [[2,2],[5,2],[2,5],[5,5]], Board).
get_dangerous_holes_by_side_internal([], _, [], _).
get_dangerous_holes_by_side_internal(Result, Side, [[X,Y]|Remaining], Board):- 
	get_adjacent_pieces(Adj, [X,Y], Board),
	(Side = gold -> get_opponents(Adj2, Adj) ; get_pieces(Adj2, Adj)),
	length_list(Len, Adj2),
	get_dangerous_holes_by_side_internal(Result1, Side, Remaining, Board), 
	(Len = 0 -> Result = [[X,Y]|Result1] ; Result = Result1).

% get_nearest_piece_by_side(Result, Position, Side, Board) : Une fonction qui pour une position donnée retourne la pièce d'un côté donné la plus proche. TODO %

% convert_to_move(Result, Input) : Convertit un couple source/destination en une liste de mouvements unitaires à passer au moteur. %
% TODO : tronquer si on a plus de 5 mouvements, s'arrêter si on rencontre une pièce ou qu'on arrive à une pièce adjacente adverse plus puissante que nous. Pas parfait mais bon.
% TODO aussi gérer le fait de pousser une pièce

% simulate_to_board(Result, Moves, Board) : Prend une liste de mouvement unitaires et les applique au Board. %

% count_by_type_and_side(Result, Type, Side, Board) : Compte le nombre de pièces sur le plateau d'un type et côté donnés.
count_by_type_and_side(0, _, _, []).
count_by_type_and_side(N, Type, Side, [[_,_,Type,Side]|Remaining]):- count_by_type_and_side(M, Type, Side, Remaining), N is M+1.
count_by_type_and_side(N, Type, Side, [[_,_,T,S]|Remaining]):- Type \= T, Side \= S, count_by_type_and_side(N, Type, Side, Remaining).

% is_winning_state(Board) : S'unifie si un de nos lapins est sur la ligne adverse OU qu'il n'y a plus aucun lapin adverse.
is_winning_state(Board):- count_by_type_and_side(0, rabbit, gold, Board).
is_winning_state(Board):- gen_numeric(X, 0, 7), get_on_coord([7, X, rabbit, silver], [X, 7], Board).

%%Moteur de MinMax %%
%eval_function(X,Board) : Evalue la valeur d'un mouvement
eval_function(X,Board):- count_by_type_and_side(Result1,rabbit,silver,Board),
                        count_by_type_and_side(Result2,cat,silver,Board),
                        count_by_type_and_side(Result3,dog,silver,Board),
                        count_by_type_and_side(Result4,horse,silver,Board),
                        count_by_type_and_side(Result5,camel,silver,Board),
                        count_by_type_and_side(Result6,elephant,silver,Board),
                        count_by_type_and_side(Result7,rabbit,gold,Board),
                        count_by_type_and_side(Result8,cat,gold,Board),
                        count_by_type_and_side(Result9,dog,gold,Board),
                        count_by_type_and_side(Result10,horse,gold,Board),
                        count_by_type_and_side(Result11,horse,gold,Board),
                        count_by_type_and_side(Result12,elephant,gold,Board),
                        X is (Result1*100 + Result2*40 + Result3*50 + Result4*60 + Result5*70 + Result6*80) - (Result7*100 + Result8*40 + Result9*50 + Result10*60 + Result11*70 + Result12*80).
%eval_state(val,Board) : Evalue la valeur du plateau 
%eval_state(val,Board):- is_winning_state(Board) , !, val is 1000000.
%eval_state(val,Board):- val is eval_function(Board).

%minmax() : Moteur de l'algo minmax
%avant le choose besoin de definir un coup au hasard%
%minmax():-choose_move(best,get_possible_moves(Board))

%choose_move():choisis le meilleur coup possible parmis les coup proposer
choose_move(Best,[]).
%si move1 possède une meilleur valeur que best alors on remplace best par ce mouvement%
choose_move(Best,[MOVE1|Q]):-eval_state(X,board_after_move_best),eval_state(Y,board_after_move1),Y>X,!,Best is MOVE1,choose_move(Best,[Q]).
%sinon on ne fait rien%
choose_move(Best,[MOVE1|Q]):-choose_move(Best,[Q]).

%%% Moteur de jeu %%%

% get_moves(Moves, Gamestate, Board) : Le moteur de jeu en lui même. %
get_moves([[[Y,X],[Y2,X]],[[Y2,X],[Y3,X]],[[Y3,X],[Y4,X]],[[Y4,X],[Y5,X]]], Gamestate, Board):- get_pieces(Pieces, Board), get_by_type([[Y,X,_,_]|_], Pieces, rabbit), Y2 is Y+1, Y3 is Y2+1, Y4 is Y3+1, Y5 is Y4+1, get_opponents([], Board).
get_moves([[[1,0],[2,0]],[[2,0],[1,0]]], Gamestate, Board).

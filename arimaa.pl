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

% compare_pieces(Result, Piece1, Piece2) : Retourne 1 si la pièce 1 est plus forte que la pièce 2, -1 si la pièce 2 est plus forte que la pièce 1, 0 si elles sont égales.
compare_pieces(0, [_,_,Type,_], [_,_,Type,_]):- !.
compare_pieces(R, [_,_,Type1,_], [_,_,Type2,_]):-
	nth(X1, [rabbit, cat, dog, horse, camel, elephant], Type1),
	nth(X2, [rabbit, cat, dog, horse, camel, elephant], Type2),
	(X1>X2 -> R is 1 ; R is -1), !.

% get_moves_for_given_piece(Result, Piece, Board) : Retourne la liste des destinations (ou liste de mouvements ? TODO selon distance_on_board) possibles pour une pièce donné sur le board. %
%get_possible_moves([], _, []).
get_moves_for_given_piece(Result, Piece, Board):- setof([X,Y], get_move_for_given_piece([X,Y], Piece, Board), Result).
get_move_for_given_piece([X2,Y2], [Y1,X1,_,_], Board):- distance_on_board(D, [X1, Y1], [X2, Y2], Board), D < 5, D > 0.
% TODO il faut interdire aussi les mouvements si une pièce adverse plus forte est en contact avec nous pendant le parcours : je pense qu'on va devoir décomposer le mouvement avant.

% get_dangerous_holes_by_side(Result, Type, Board) : Une fonction qui retourne les positions des trous noirs sans pièces d'un type donné autour. La question étant de pouvoir savoir si un trou noir présente un risque pour le type donné. TODO %

% get_nearest_piece(Result, Position, Board) : Une fonction qui pour une position donnée retourne la pièce la plus proche. TODO %

% convert_to_move(Result, Input) : Convertit un couple source/destination en une liste de mouvements unitaires à passer au moteur. %

% count_by_type_and_side(Result, Type, Side, Board) : Compte le nombre de pièces sur le plateau d'un type et côté donnés.
count_by_type_and_side(0, _, _, []).
count_by_type_and_side(N, Type, Side, [[_,_,Type,Side]|Remaining]):- count_by_type_and_side(M, Type, Side, Remaining), N is M+1, !.
count_by_type_and_side(N, Type, Side, [[_,_,_,_]|Remaining]):- count_by_type_and_side(N, Type, Side, Remaining), !.

% is_winning_state(Board) : S'unifie si un de nos lapins est sur la ligne adverse OU qu'il n'y a plus aucun lapin adverse.

%%% Moteur de jeu %%%

% get_moves(Moves, Gamestate, Board) : Le moteur de jeu en lui même. %
get_moves([[[Y,X],[Y2,X]],[[Y2,X],[Y3,X]],[[Y3,X],[Y4,X]],[[Y4,X],[Y5,X]]], Gamestate, Board):- get_pieces(Pieces, Board), get_by_type([[Y,X,_,_]|_], Pieces, rabbit), Y2 is Y+1, Y3 is Y2+1, Y4 is Y3+1, Y5 is Y4+1, get_opponents([], Board).
get_moves([[[1,0],[2,0]],[[2,0],[1,0]]], Gamestate, Board).

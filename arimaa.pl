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
% TODO

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

% get_moves_for_given_piece(Result, Piece, Board) : Retourne la liste des destinations (ou liste de mouvements ? TODO selon distance_on_board) possibles pour une pièce donné sur le board. %
%get_possible_moves([], _, []).
get_moves_for_given_piece(Result, Piece, Board):- setof([X,Y], get_move_for_given_piece([X,Y], Piece, Board), Result).
get_move_for_given_piece([X2,Y2], [Y1,X1,_,_], Board):- distance(D, [X1, Y1], [X2, Y2]), D < 5, D > 0.
%TODO on ne doit pas utiliser distance, mais distance_on_board qui tient compte du board ; selon comment est fait ce prédicat il faudra ausi éliminer les mouvements qui font aboutir à une pièce.
%TODO il faut interdire aussi les mouvements si une pièce adverse plus forte est en contact avec nous

% get_dangerous_holes_by_side(Result, Type, Board) : Une fonction qui retourne les positions des trous noirs sans pièces d'un type donné autour. La question étant de pouvoir savoir si un trou noir présente un risque pour le type donné. TODO %

% get_nearest_piece(Result, Position, Board) : Une fonction qui pour une position donnée retourne la pièce la plus proche. TODO %

% convert_to_move(Result, Input) : Convertit un couple source/destination en une liste de mouvements unitaires à passer au moteur. %

%%% Moteur de jeu %%%

% get_moves(Moves, Gamestate, Board) : Le moteur de jeu en lui même. %
get_moves([[[Y,X],[Y2,X]],[[Y2,X],[Y3,X]],[[Y3,X],[Y4,X]],[[Y4,X],[Y5,X]]], Gamestate, Board):- get_pieces(Pieces, Board), get_by_type([[Y,X,_,_]|_], Pieces, rabbit), Y2 is Y+1, Y3 is Y2+1, Y4 is Y3+1, Y5 is Y4+1, get_opponents([], Board).
get_moves([[[1,0],[2,0]],[[2,0],[1,0]]], Gamestate, Board).

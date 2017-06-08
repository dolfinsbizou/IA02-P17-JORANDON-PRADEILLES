:- module(bot,
      [  get_moves/3
      ]).
	
% get_moves signature
% get_moves(Moves, gamestate, board).

% Exemple of variable
% gamestate: [side, [captured pieces] ] (e.g. [silver, [ [0,1,rabbit,silver],[0,2,horse,silver] ]) 
% board: [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]

% Call exemple:
% get_moves(Moves, [silver, []], [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).

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

% get_adjacent_pieces(Result, Piece, Board) : Retourne la liste des pièces adjacentes sur le board à une pièce donnée. %
get_adjacent_pieces([],_,[]).
get_adjacent_pieces(Result, Piece, [Curr|Remaining]). % TODO

% distance(Distance, Point1, Point2) : Compute la distance entre deux couples de coordonnées. Fonctionne aussi en mode générateur. %
distance(D, [X1, Y1], [X2, Y2]):- gen_numeric(X2, 0, 7), gen_numeric(Y2, 0, 7), gen_numeric(X1, 0, 7), gen_numeric(Y1, 0, 7), abs(X2-X1, DX), abs(Y2-Y1, DY), D is DX+DY.

% get_moves_for_given_piece : Retourne dans le paramètre 1 la liste des coups possibles pour une pièce du paramètre 2 sur le board dans le paramètre 3. %
%get_possible_moves([], _, []).
get_possible_moves(Result, Piece, Board):- setof([X,Y], get_possible_move([X,Y], Piece, Board), Result).
get_possible_move([X2,Y2], [X1,Y1,_,_], Board):- distance(D, [X1, Y1], [X2, Y2]), D < 5. %TODO

% get_dangerous_holes_by_side : une fonction qui retourne les positions des trous noirs sans pièces d'un type donné autour. La question étant de pouvoir savoir si un trou noir présente un risque pour le type donné. TODO %

% get_nearest_piece : une fonction qui pour une position donnée retourne la pièce la plus proche. TODO %

%%% Moteur de jeu %%%

get_moves([[[Y,X],[Y2,X]],[[Y2,X],[Y3,X]],[[Y3,X],[Y4,X]],[[Y4,X],[Y5,X]]], Gamestate, Board):- get_pieces(Pieces, Board), get_by_type([[Y,X,_,_]|_], Pieces, rabbit), Y2 is Y+1, Y3 is Y2+1, Y4 is Y3+1, Y5 is Y4+1, get_opponents([], Board).
get_moves([[[1,0],[2,0]],[[2,0],[1,0]]], Gamestate, Board).

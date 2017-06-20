:- module(bot,
      [  get_moves/3
      ]).

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

% distances(Result, List, Point) : Compute les distances d'une liste de couples de coordonnées à un point. %
distances([], [], _):- !.
distances([Res|Result], [Coord|Remaining], Point):- distance(Res, Point, Coord), distances(Result, Remaining, Point), !.

% pieces_to_coord(Result, List) : Convertit une liste de pièces en une liste de coordonnées. %
pieces_to_coord([], []):- !.
pieces_to_coord([[X,Y]|Result], [[Y,X,_,_]|Remaining]):- pieces_to_coord(Result, Remaining).

% distance_on_board(Distance, Point1, Point2, Board) : Compute la distance entre deux couples de coordonnées compte tenu du plateau. %
distance_on_board(Distance, Point1, [X,Y], Board):- distance(Distance, Point1, [X,Y]), \+ get_on_coord(_, [X,Y], Board).

% compare_types(Result, Type1, Type2) : Retourne 1 si le type 1 est plus fort que le type 2, -1 si le type 2 est plus fort que le type 1, 0 si ils sont égaux.
compare_types(0, Type, Type):- !.
compare_types(R, Type1, Type2):-
	nth(X1, [rabbit, cat, dog, horse, camel, elephant], Type1),
	nth(X2, [rabbit, cat, dog, horse, camel, elephant], Type2),
	(X1>X2 -> R is 1 ; R is -1), !.

% can_move(Piece, Board) : s'unifie si une pièce donnée peut bouger.
can_move([Y,X,Type,Side], Board):-
	get_adjacent_pieces(Adj, [X,Y], Board),
	Init1 is 4,
	(X is 0 -> Init2 is Init1-1 ; Init2 is Init1),
	(X is 7 -> Init3 is Init2-1 ; Init3 is Init2),
	(Y is 0 -> Init4 is Init3-1 ; Init4 is Init3),
	(Y is 7 -> Init is Init4-1 ; Init is Init4),
   	can_move_internal(Init, Adj),
	\+ stuck_by_opponent(Side, Type, Adj, 1).
can_move_internal(Val, []):- Val > 0.
can_move_internal(Val, [_|Remaining]):- Val1 is Val-1, can_move_internal(Val1, Remaining).
stuck_by_opponent(_, _, [], X):- X > 1, !.
stuck_by_opponent(Side, Type, [[_,_,T,S]|Remaining], Act):- Side \= S, compare_types(R, T, Type), (R is 1 -> Next is Act*2 ; Next is Act), stuck_by_opponent(Side, Type, Remaining, Next), !.
stuck_by_opponent(Side, Type, [[_,_,Type,Side]|Remaining], Act):- stuck_by_opponent(Side, Type, Remaining, Act), !.

% get_moves_for_given_piece(Result, Piece, Board) : Retourne la liste des coups possibles pour une pièce donné sur le board. %
get_moves_for_given_piece([], Piece, Board):- \+ can_move(Piece, Board).
get_moves_for_given_piece(Result, Piece, Board):- can_move(Piece, Board), setof(Move, get_move_for_given_piece(Move, Piece, Board), Result).
get_move_for_given_piece([[X1,Y1], [X2,Y2]], [Y1,X1,_,_], Board):- distance_on_board(D, [X1, Y1], [X2, Y2], Board), D < 5, D > 0.

% get_possible_moves(Result, Board_curr, Board) : Retourne la liste des coups possibles pour toutes les pièces argent du tableau sous forme d'une liste de [Origine, Destination].
get_possible_moves([], [], _).
get_possible_moves(Result, [[Y,X,Type,silver]|Remaining], Board):- get_moves_for_given_piece(Result2, [Y,X,Type,silver], Board), get_possible_moves(Result1, Remaining, Board), concat_list(Result, Result2, Result1). 
get_possible_moves(Result, [[_,_,_,gold]|Remaining], Board):- get_possible_moves(Result, Remaining, Board). 

% get_opponent_moves(Result, Board_curr, Board) : Retourne la liste des coups possibles pour toutes les pièces or du tableau sous forme d'une liste de [Origine, Destination].
get_opponent_moves([], [], _).
get_opponent_moves(Result, [[Y,X,Type,gold]|Remaining], Board):- get_moves_for_given_piece(Result2, [Y,X,Type,gold], Board), get_opponent_moves(Result1, Remaining, Board), concat_list(Result, Result2, Result1). 
get_opponent_moves(Result, [[_,_,_,silver]|Remaining], Board):- get_opponent_moves(Result, Remaining, Board). 

% get_dangerous_holes_by_side(Result, Side, Board) : Une fonction qui retourne les positions des trous noirs sans pièces d'un côté donné autour. La question étant de pouvoir savoir si un trou noir présente un risque pour le côté donné. %
get_dangerous_holes_by_side(Result, Side, Board):- get_dangerous_holes_by_side_internal(Result, Side, [[2,2],[5,2],[2,5],[5,5]], Board).
get_dangerous_holes_by_side_internal([], _, [], _):- !.
get_dangerous_holes_by_side_internal(Result, Side, [[X,Y]|Remaining], Board):- 
	(get_adjacent_pieces(Adj1, [X,Y], Board) -> Adj = Adj1 ; Adj = []),
	(Side = gold -> get_opponents(Adj2, Adj) ; get_pieces(Adj2, Adj)),
	length_list(Len, Adj2),
	get_dangerous_holes_by_side_internal(Result1, Side, Remaining, Board), 
	(Len = 0 -> Result = [[X,Y]|Result1] ; Result = Result1), !.

% get_nearest_piece_by_side(Result, Coord, Side, Board) : Une fonction qui pour une position donnée retourne la pièce d'un côté donné la plus proche. %
get_nearest_piece_by_side(Result, Coord, Side, Board):- 
	(Side = gold -> get_opponents(Pieces, Board) ; get_pieces(Pieces, Board)),
	pieces_to_coord(Coords, Pieces),
	distances(Dis, Coords, Coord),
	min_list(Dis, MinDis),
	nth(Idx, Dis, MinDis),
	nth(Idx, Pieces, Result).

% convert_to_move(Result, Input, Board) : Convertit un couple source/destination en une liste de mouvements. %
convert_to_move(Result, [[X1,Y1],[X2,Y2]], Board):- DX is X2-X1, DY is Y2-Y1, convert_to_move_internal(Result, [X1,Y1], [DX,DY], 4, 1, Board).
convert_to_move_internal([], _, [0,0], _, _, _):- !.
convert_to_move_internal([], _, _,  N, _, _):- N < 1, !.
convert_to_move_internal(Final , [X,Y], [DX,DY], N, Parity, Board):- 
	M is N-1,
	(Parity = 0 -> Parity2 is 1,
		(DY \= 0 ->
			DX2 is DX, X2 is X, (DY > 0 -> Y2 is Y+1, DY2 is DY-1 ; Y2 is Y-1, DY2 is DY+1) ;
			DY2 is DY, Y2 is Y, (DX > 0 -> X2 is X+1, DX2 is DX-1 ; X2 is X-1, DX2 is DX+1)
		) ;
		Parity2 is 0,
		(DX \= 0 ->
			DY2 is DY, Y2 is Y, (DX > 0 -> X2 is X+1, DX2 is DX-1 ; X2 is X-1, DX2 is DX+1) ;
			DX2 is DX, X2 is X, (DY > 0 -> Y2 is Y+1, DY2 is DY-1 ; Y2 is Y-1, DY2 is DY+1)
		)
	),
	Res1 = [[X,Y],[X2,Y2]],
	% S'arrêter si on rencontre une pièce, ne fonctionne pas
	%(get_on_coord(_, [X2,Y2], Board) ->
	%	Final = [Res1|[]], Continue=0 ; Continue is 0
	%),
	Continue = 1,
	(nth(_, [[2,2],[2,5],[5,2],[5,5]], [X2,Y2]), Continue=1 ->
		Final = [Res1|[]], Continue1=0 ; Continue1=1
	),
	(Continue1=1 ->
		(get_on_coord([_,_,_,gold], [X2,Y2], Board) ->
				DPX is X2-X, DPY is Y2-Y, X3 is X2+DPX, Y3 is Y2+DPY, Final = [[[X2,Y2],[X3,Y3]]] ;
			convert_to_move_internal(Result, [X2,Y2], [DX2, DY2], M, Parity2, Board), Final = [Res1|Result]
   		) ;
		true
	), !.

% update_nth(List, Idx, New, Result) : Met à jour une liste en modifiant l'élément à l'index donné. %
% via : https://stackoverflow.com/questions/8519203/prolog-replace-an-element-in-a-list-at-a-specified-index %
update_nth([_|T], 1, X, [X|T]).
update_nth([H|T], I, X, [H|R]):- I > 0, NI is I-1, update_nth(T, NI, X, R), !.
update_nth(L, _, _, L).

% simulate_to_board(Result, Moves, Board) : Prend une liste de mouvement unitaires et les applique au Board. %
simulate_to_board(Board, [], Board):- !.
simulate_to_board(Result, [[Source, [X2,Y2]]|Remaining], Board):-
	get_on_coord([Y,X,Type,Side], Source, Board),
	nth(Idx, Board, [Y,X,Type,Side]),
	update_nth(Board, Idx, [Y2,X2,Type,Side], NewBoard),
	simulate_to_board(Result, Remaining, NewBoard), !.

% count_by_type_and_side(Result, Type, Side, Board) : Compte le nombre de pièces sur le plateau d'un type et côté donnés.
count_by_type_and_side(0, _, _, []):- !.
count_by_type_and_side(N, Type, Side, [[_,_,Type,Side]|Remaining]):- count_by_type_and_side(M, Type, Side, Remaining), N is M+1, !.
count_by_type_and_side(N, Type, Side, [[_,_,_,_]|Remaining]):- count_by_type_and_side(N, Type, Side, Remaining), !.

% check_by_type_and_side(Result, Type, Side, Board) : Vérifie le nombre de pièces sur le plateau d'un type et côté donnés.
check_by_type_and_side(0, _, _, []).
check_by_type_and_side(N, Type, Side, [[_,_,Type,Side]|Remaining]):- check_by_type_and_side(M, Type, Side, Remaining), N is M+1.
check_by_type_and_side(N, Type, Side, [[_,_,T,S]|Remaining]):- Type \= T, Side \= S, check_by_type_and_side(N, Type, Side, Remaining).

% is_winning_state(Board) : S'unifie si un de nos lapins est sur la ligne adverse OU qu'il n'y a plus aucun lapin adverse. %
is_winning_state(Board):- check_by_type_and_side(0, rabbit, gold, Board).
is_winning_state(Board):- gen_numeric(X, 0, 7), get_on_coord([7, X, rabbit, silver], [X, 7], Board).

% get_nearest_pieces_by_side(Result, List, Side, Board) : Retourne pour chaque coordonée de la liste la distance de la pièce la plus proche du côté correspondant. %
get_nearest_pieces_by_side([], [], _, _):- !.
get_nearest_pieces_by_side([Dist|Result], [Coord|Remaining], Side, Board):- get_nearest_piece_by_side([Y,X,_,_], Coord, Side, Board), distance(Dist, Coord, [X,Y]), get_nearest_pieces_by_side(Result, Remaining, Side, Board), !.

% multiply_list(Result, List, Value) : Multiplie tous les éléments de la liste par une valeur. %
multiply_list([], [], _).
multiply_list([Res|Result], [Curr|Remaining], Value):- multiply_list(Result, Remaining, Value), Res is Curr * Value.

% sum_list(Result, List) : Somme tous les éléments d'une liste. %
sum_list(0, []).
sum_list(Result, [Curr|Remaining]):- sum_list(Result1, Remaining), Result is Curr + Result1.

% max_list(List, Result) : Donne le maximum d'une liste. %
% https://stackoverflow.com/questions/27455034/find-max-integer-in-a-list-in-prolog %
max_list([H|T], Y):-
	max_list(T,X),
    (H > X ->
     H = Y;
     Y = X).
max_list([X],X).

%%%Moteur de MinMax %%%

%eval_function(Result, Board, Count) : Evalue la valeur d'un mouvement. %
eval_function(X, Board, [SilverMovesCount, GoldMovesCount]):-
	count_by_type_and_side(Result1,rabbit,silver,Board),
	count_by_type_and_side(Result2,cat,silver,Board),
	count_by_type_and_side(Result3,dog,silver,Board),
	count_by_type_and_side(Result4,horse,silver,Board),
	count_by_type_and_side(Result5,camel,silver,Board),
	count_by_type_and_side(Result6,elephant,silver,Board),
	count_by_type_and_side(Result7,rabbit,gold,Board),
	count_by_type_and_side(Result8,cat,gold,Board),
	count_by_type_and_side(Result9,dog,gold,Board),
	count_by_type_and_side(Result10,horse,gold,Board),
	count_by_type_and_side(Result11,camel,gold,Board),
	count_by_type_and_side(Result12,elephant,gold,Board),
	get_dangerous_holes_by_side(DangerousSilver, silver, Board),
	get_nearest_pieces_by_side(DistancesSilver, DangerousSilver, silver, Board),
	get_dangerous_holes_by_side(DangerousGold, gold, Board),
	get_nearest_pieces_by_side(DistancesGold, DangerousGold, gold, Board),
	sum_list(DS, DistancesSilver),
	sum_list(DG, DistancesGold),
	X is ((Result1*100 + Result2*40 + Result3*50 + Result4*60 + Result5*70 + Result6*80 + SilverMovesCount + DS*50) - (Result7*100 + Result8*40 + Result9*50 + Result10*60 + Result11*70 + Result12*80 + GoldMovesCount + DG*50)).

% eval_state(Result, Board, Count) : évalue la valeur du plateau. %
eval_state(Val, Board, Count):- (is_winning_state(Board) -> Val is 1000000 ; eval_function(Val, Board, Count)).

% minmax(Result, Board) : Moteur de l'algo minmax. %
minmax(Result, Board):- get_possible_moves(Moves, Board, Board), length_list(SMC, Moves), get_opponent_moves(OMoves, Board, Board), length_list(GMC, OMoves), choose_move(Result, Moves, Board, [SMC, GMC]).

% simulate_strategies(Result, Strategies, Board, Count) : Pour chaque mouvement, simule le coup et évalue son score. %
simulate_strategies([], [], _, _).
simulate_strategies(Result, [Curr|Remaining], Board, Count):-
	convert_to_move(Move, Curr, Board),
	simulate_strategies(Result1, Remaining, Board, Count),
	(Move \= [] ->
		simulate_to_board(NewBoard, Move, Board),
		eval_state(Res, NewBoard, Count),
		Result = [Res|Result1] ;
		Result = Result1
	).

% choose_move(Result, Strategies, Board, Count) : choisit le meilleur coup possible parmi les coups proposés. %
choose_move(Result, Strategies, Board, Count):-
	simulate_strategies(Res, Strategies, Board, Count),
	max_list(Res, Max),
	nth(Idx, Res, Max),
	nth(Idx, Strategies, Result).

%%% Moteur de jeu %%%

% get_moves(Moves, Gamestate, Board) : Le moteur de jeu en lui même. %

convert_for_motor([], []).
convert_for_motor([[[Y1,X1],[Y2,X2]]|Result], [[[X1,Y1],[X2,Y2]]|Remaining]):- convert_for_motor(Result, Remaining).

get_moves(Result, _, Board):- minmax([[X1,Y1],[X2,Y2]], Board), convert_to_move(Result2, [[X1,Y1],[X2,Y2]], Board), convert_for_motor(Result1, Result2), (Result1 = [] -> Result = [[[1,0],[2,0]],[[2,0],[1,0]]] ; Result = Result1).

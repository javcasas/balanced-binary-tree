:- use_module(library(time)).

% Reified if_, or a basic implementation below
:- use_module(library(reif)).

%if_(If, Then, Else) :-
%  call(If, T),
%  (
%    T == true -> Then;
%    T == false -> Else
%  ).


% less than, or a basic implementation below
:- use_module(library(clpz)).
lt(X, Y, T) :- #<(X, Y, T).

% lt(X, Y, T) :- X < Y -> T = true; T = false.


% Tree is:
% | branch(Elem, Height, Left, Right)
% | empty

% Basic predicates
singleton(X, branch(X, 1, empty, empty)).

size(empty, 0).
size(branch(_, S, _, _), S).

% Elem X is in the specified tree
member(X, branch(Y, _, Left, Right)) :-
  var(X),
  ( X = Y
  ; member(X, Left)
  ; member(X, Right)
  ).
member(X, branch(Y, _, Left, Right)) :-
  nonvar(X),
  if_(
    lt(X, Y),
    member(X, Left),
    if_(
      lt(Y, X),
      member(X, Right),
      X = Y
    )
  ).

% Smart constructor
mkBranch(X, Left, Right, branch(X, Size, Left, Right)) :-
  size(Left, SizeL),
  size(Right, SizeR),
  Size is 1 + SizeL + SizeR.

% Add to a tree without balancing
add_unbalanced(X, empty, R) :- singleton(X, R).
add_unbalanced(X, branch(Y, H, Left, Right), Result) :-
  if_(
    lt(X, Y),
    (
      add_unbalanced(X, Left, NewLeft),
      mkBranch(Y, NewLeft, Right, Result)
    ),
    if_(
      lt(Y, X),
      (
        add_unbalanced(X, Right, NewRight),
        mkBranch(Y, Left, NewRight, Result)
      ),
      Result = branch(Y, H, Left, Right)
    )
  ).


% Construct an unbalanced tree from the list.
mkUnbalancedTree([], empty).
mkUnbalancedTree([X|Xs], R) :-
  mkUnbalancedTree(Xs, R1),
  add_unbalanced(X, R1, R).


% Balancing utilities
too_few_elements(LN, RN, Res) :- 
  R is LN + RN,
  lt(R, 2, Res).

right_too_big(RATIO, LN, RN, Res) :-
  R is RATIO * LN,
  lt(R, RN, Res).

left_too_big(RATIO, LN, RN, Res) :- right_too_big(RATIO, RN, LN, Res).

% Smart auto-balancing constructor
b_balanced(V, L, R, Y) :-
  RATIO = 2,
  size(L, LN),
  size(R, RN),
  if_(
    too_few_elements(LN, RN),
    mkBranch(V, L, R, Y),
    if_(
      right_too_big(RATIO, LN, RN),
      (
        % Right is too big
        R = branch(_, _, RL, RR),
        size(RL, RLN),
        size(RR, RRN),
        if_(
          lt(RLN, RRN),
          single_L(V, L, R, Y),
          double_L(V, L, R, Y)
        )
      ),
      if_(
        left_too_big(RATIO, LN, RN),
        (
          % Left is too big
          L = branch(_, _, LL, LR),
          size(LL, LLN),
          size(LR, LRN),
          if_(
            lt(LRN, LLN),
            single_R(V, L, R, Y),
            double_R(V, L, R, Y)
          )
        ),
        mkBranch(V, L, R, Y)
      )
    )
  ).

% Add elements to a balanced tree
add_balanced(X, empty, Y) :- singleton(X, Y).
add_balanced(X, branch(V, H, L, R), Y) :-
  if_(
    lt(X, V),
    (
      add_balanced(X, L, NL),
      b_balanced(V, NL, R, Y)
    ),
    if_(
      lt(V, X),
      (
        add_balanced(X, R, NR),
        b_balanced(V, L, NR, Y)
      ),
      Y = branch(V, H, L, R)
    )
  ).

% Make a balanced tree from the elements in the list
mkBalancedTree([], empty).
mkBalancedTree([X|Xs], R) :-
  mkBalancedTree(Xs, R1),
  add_balanced(X, R1, R).

% Testing functions.
% Make a list of all the numbers from X to Y
mklist(X, X, [X]).
mklist(X, Y, [X|L1]) :-
  Y > X,
  X1 is X + 1,
  mklist(X1, Y, L1).

% Construct a balanced tree from all the numbers from A to B
test(A, B, X) :-
  mklist(A, B, L),
  time(mkBalancedTree(L, X)).

% Rotations
single_L(A, X, branch(B, _, Y, Z), Res) :- mkBranch(A, X, Y, L), mkBranch(B, L, Z, Res).
double_L(A, X, branch(C, _, branch(B, _, Y1, Y2), Z), Res) :-
  mkBranch(C, Y2, Z, R),
  mkBranch(A, X, Y1, L),
  mkBranch(B, L, R, Res).

single_R(B, branch(A, _, X, Y), Z, Res) :-
  mkBranch(B, Y, Z, R),
  mkBranch(A, X, R, Res).
double_R(C, branch(A, _, X, branch(B, _, Y1, Y2)), Z, Res) :-
  mkBranch(C, Y2, Z, R),
  mkBranch(A, X, Y1, L),
  mkBranch(B, L, R, Res).

% Helpers for displaying trees
write_tree(empty).
write_tree(branch(E, _, L, R)) :-
  write_tree(branch(E, _, L, R), 0).
write_tree(empty, _).
write_tree(branch(E, _, L, R), Indent) :-
  write_indent(Indent),
  write(E),
  nl,
  Indent1 is Indent + 2,
  write_tree(L, Indent1),
  write_tree(R, Indent1).

write_indent(0).
write_indent(X) :- X>0, write(' '), X1 is X - 1, write_indent(X1).

% Pseudo-traversals
list_members_inorder(branch(E, _, _, _), E).
list_members_inorder(branch(_, _, L, _), Res) :- list_members_inorder(L, Res).
list_members_inorder(branch(_, _, _, R), Res) :- list_members_inorder(R, Res).

list_members_preorder(branch(_, _, L, _), Res) :- list_members_preorder(L, Res).
list_members_preorder(branch(E, _, _, _), E).
list_members_preorder(branch(_, _, _, R), Res) :- list_members_preorder(R, Res).

list_members_postorder(branch(_, _, _, R), Res) :- list_members_postorder(R, Res).
list_members_postorder(branch(E, _, _, _), E).
list_members_postorder(branch(_, _, L, _), Res) :- list_members_postorder(L, Res).

% Construct a balanced tree out of 2 balanced trees
% (not necessarily balanced to each other)
% and an item that goes in the middle of the two trees
concat3(X, empty, R, Res) :- 
  !, 
  add_balanced(X, R, Res).

concat3(X, L, empty, Res) :- 
  !, 
  add_balanced(X, L, Res).

concat3(V, L, R, Res) :-
  L = branch(V1, N1, L1, R1),
  R = branch(V2, N2, L2, R2),
  RATIO = 3,
  if_(
    right_too_big(RATIO, N1, N2),
    (
      concat3(V, L, L2, ResL),
      b_balanced(V2, ResL, R2, Res)
    ),
    if_(
      left_too_big(RATIO, N1, N2),
      (
        concat3(V, R1, R, ResR),
        b_balanced(V1, L1, ResR, Res)
      ),
      mkBranch(V, L, R, Res)
    )
  ).

% Extract a tree that contains all the elements less than the element specified
split_lt(empty, _, empty) :- !.
split_lt(branch(V, _, L, R), X, Res) :-
  if_(
    lt(X, V),
    split_lt(L, X, Res),
    if_(
      lt(V, X),
      (
        split_lt(R, X, ResR),
        concat3(V, L, ResR, Res)
      ),
      Res = L
    )
  ).

% Extract a tree that contains all the elements greater than the element specified
split_gt(empty, _, empty) :- !.
split_gt(branch(V, _, L, R), X, Res) :-
  if_(
    lt(V, X),
    split_gt(R, X, Res),
    if_(
      lt(X, V),
      (
        split_gt(L, X, ResL),
        concat3(V, ResL, R, Res)
      ),
      Res = R
    )
  ).

% union of two trees
union(empty, T2, T2) :- !.
union(T1, empty, T1) :- !.
union(Tree1, branch(A, _, L, R), Res) :-
  !,
  split_lt(Tree1, A, L1),
  split_gt(Tree1, A, R1),
  union(L1, L, ResL),
  union(R1, R, ResR),
  concat3(A, ResL, ResR, Res).

% Delete an element from a tree
delete(A, Tree, Res) :-
  split_lt(Tree, A, L),
  split_gt(Tree, A, R),
  union(L, R, Res).


% All the elements in A that are not in B
difference(A, empty, A).
difference(empty, _, empty).
difference(A, branch(X, _, L, R), Res) :-
  split_lt(A, X, AL),
  split_gt(A, X, AR),
  difference(AL, L, ResL),
  difference(AR, R, ResR),
  union(ResL, ResR, Res).

% Intersection
% A intersect B = A - (A - B)
intersection(A, B, Res) :-
  difference(A, B, AminusB),
  difference(A, AminusB, Res).

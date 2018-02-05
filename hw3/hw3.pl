/* Homework Assignment 3
   Programming Languages
   CS471, Spring 2018
   Binghamton University */

/* Instructions */

/*

This section deals with general submission instructions. Use this
source file. BEFORE MOVING ON RENAME hw3S18.asn to hw3.pl. You will be
able to code in and run the file in the prolog interpreter directly. I
recommend reading this assignment directly from the source file.

We will be using swipl for our prolog environment:
 To load/reload this file, cd to its directory and run swipl. Then, in the prompt, type [hw3].

cd PATH_TO_FILE
swipl
[hw3].

From then on you may execute queries (goals) in the prompt. As usual,
you should provide your answers in the designated spot. Once you have
added some code to the file, rerun [hw3]. in the swipl prompt to
reload.

In addition, there are unit tests for each problem. These are there to
help you better understand what the question asks for, as well as
check your code. They are included in our knowledge base as queries
and are initially commented out -- % is a Prolog line comment.

%:- member_times(4,[3,3,2,3],0). % SUCCEED
%:- member_times(4,[1,2,3],3).   % FAIL

After you have finished a problem and are ready to test, remove the
initial % for each test for the associated problem and reload the
assignment file ([hw3].). Each SUCCEED line should silently load and
succeed, and each FAIL line should throw a WARNING. If a SUCCEED line
throws a WARNING, or a FAIL line fails to, then you solution is not
correct. If you pass the tests there is a good chance that your code
is correct, but not guaranteed; the tests are meant as guided feedback
and are not a check for 100% correctness.

*/

/* Submission */

/*
For this assignment -- and the remaing Prolog assignments -- you must
submit only the source file. There is no need to tar anything as all
coding should be done directly in hw3.pl.
*/

/* Homework 3 */

/* Due: Next Tuesday, 11:59 PM */

/* Problem 1:

The following are two basic predicates for list manipulation:
my_first/2 and my_last/2. We may refer to a predicate by writings it
as name/arity; hence, my_first/2 means a predicate named my_first with
two arguments.

my_first(X,Y) succeeds if X is the first element of list Y.
my_last(X,Y) succeeds if X is the last element of list Y.
*/

my_first(X,[X|_]).

my_last(X,[X]).
my_last(X,[_|Ys]) :- my_last(X,Ys).

/*
Note my use of the _ (wildcard). It is used to say to Prolog "I don't
care, match anything" and should be used to avoid singleton warnings
(a variable that is not unified). We will discuss this during lab.

Try the following queries before moving on. Note that they are
included in the comment section for a reason: They would be
interpreted as facts in the database otherwise.

?- my_first(X,[3,a,dd]).
?- my_last(X,[3,a,dd]).
?- my_first(3,[3,a,dd]).
?- my_first(a,[3,a,dd]).
?- my_last(dd,[3,a,dd]).
?- my_last(a,[3,a,dd]).

Now, write a predicate my_member(X,Y) that succeeds if X is a member
of the list Y.

NOTE: my_first/2 and my_last/2 are provided as examples for
manipulating lists. You should not use them in definition of
my_member/2.

*/

/* Problem 1 Answer: */
my_member(X,[X|_]).
my_member(X,[_|T]) :- my_member(X,T).

/* 
http://cs.union.edu/~striegnk/learn-prolog-now/html/node34.html 
*/ 

/* Problem 1 Test: */
my_member((1,3),[(1,2),(1,3)]).     % SUCCEED
my_member(3,[3]).         % SUCCEED
my_member(4,[1,2,3]).     % FAIL

/* Problem 2:
 Write a predicate init(All, BLst) that succeeds if Blst has all the items of ALL
 except the last item.  The items in BLst are in the same order as ALL.
*/

/* Problem 2 Answer: */
init([_], []).
init([_|T1], [_|T2]) :- init(T1, T2).

/* Problem 2 Test: */
% :- init([1], []).       % SUCCEED
% :- init([1,2,3], [1,2]).% SUCCEED
% :- init([1,2], [1,2]).  % FAIL
% :- init([1,2], [2]).	% FAIL

/* Problem 3:
Write a predicate is_increasing(X) that succeeds if X is a list of decreasing numbers -- Each number is either the same or lower than the preceding number.

NOTE: You may match two elements at a time against a list: [X,Y|Xs] = List. It's preferable to do it in the rule head however...
some_rule([X,Y|Xs]) :- ...  
*/

/* Problem 3 Answer: */
is_increasing([]).
is_increasing([_]).
is_increasing([X, H|T]) :- X =< H, is_increasing([H|T]).

% http://www.dailyfreecode.com/code/prolog-predicate-ordered-list-which-3099.aspx


/* Problem 3 Test: */
%:- is_increasing([]).            % SUCCEED
%:- is_increasing([10]).          % SUCCEED
%:- is_increasing([9,10]).        % SUCCEED
%:- is_increasing([7,9,10]).      % SUCCEED
%:- is_increasing([2,7,7,9,10]).  % SUCCEED
%:- is_increasing([1,1,1,1,1]).   % SUCCEED

%:- is_increasing([10,9,7,9]).    % FAIL
%:- is_increasing([2,3,1]).       % FAIL
%:- is_increasing([3,2,1]).       % FAIL
%:- is_increasing([77,9]).        % FAIL

/* Problem 4:
Write a predicate element_at(X,Y,N) that succeeds if X is the Nth element of list Y. Y is 0-indexed.
NOTE: Don't worry about the error cases: i.e, N greater than the length of Y.  */

/* Problem 4 Answer: */
element_at(H, [H|_], 0).
element_at(X, [H|T], N) :- (N == 0, X == H) ; N1 is N-1, element_at(X,T,N1).

% https://stackoverflow.com/questions/40964412/how-to-find-nth-element-of-list-and-have-answer-be-in-r-prolog

/* Problem 4 Test: */
%:- element_at(3,[1,2,3],2).   % SUCCEED
%:- element_at(1,[1,2,3],0).   % SUCCEED
%:- element_at(1,[1,2,3],1).   % FAIL

/* Problem 5:
Write a predicate insert_at(X,Y,N,Z) that succeeds if Z is the list Y with X inserted at index N -- Insert X at index N in Y.

NOTE: Don't worry about the error cases: i.e, N greater than the length of Y.  */

/* Problem 5 Answer: */
insert_at(Elem,List,0,[Elem|List]).
insert_at(Elem,[E|List],Nth,[E|Result]) :- Nth1 is Nth-1, insert_at(Elem,List,Nth1,Result). 

/* Problem 5 Test: */
%:- insert_at(3,[1,2,3],2,[1,2,3,3]).  % SUCCEED
%:- insert_at(1,[1,2,3],0,[1,1,2,3]).  % SUCCEED
%:- insert_at(a,[1,2,3],1,[1,a,2,3]).  % SUCCEED

%:- insert_at(1,[1,2,3],0,[1,2,3]).    % FAIL

/* Problem 6:
Write a predicate zip(Xs,Ys,Zs) that succeeds if Zs is a list where each element is a tuple, (X,Y), with Xs and Ys paired together.

For example...
zip([1,2,3],[a,b,c],Zs) should give Zs = [(1,a),(2,b),(3,c)]
zip([1],[a],Zs) should give Zs = [(1,a)]

NOTE: You may assume X and Y have the same length. */

/* Problem 6 Answer: */
zip(_, [], []).
zip([], _, []).
zip([First|X], [Second|Y], [(First, Second)|Z]) :- 
  same_length([First|X], 
  [Second|Y]), zip(X, Y, Z).

/* Problem 6 Test: */
%:- zip([1,2,3],[a,b,c],[(1,a),(2,b),(3,c)]). % SUCCEED
%:- zip([],[],[]).                      % SUCCEED
%:- zip([1],[2],[(1,2)]).               % SUCCEED

%:- zip([1],[2],[(2,3)]).               % FAIL
%:- zip([1],[2,3],[(1,2)]).             % FAIL

/* Problem 7:

Write a predicate zip2(Xs,Ys,Zs) that succeeds if Zs is a list where each element is a tuple, (X,Y), with Xs and Ys paired together. However, the length of Zs will be equal to the length of Xs or Ys which ever is less.

For example...
zip2([1,2,3,4],[a,b,c],Zs) should give Zs = [(1,a),(2,b),(3,c)]
zip2([1],[a,b],Zs) should give Zs = [(1,a)] */

/* Problem 7 Answer: */
zip2([], _, []).
zip2(_, [], []).
zip2([First|X], [Second|Y], [(First,Second)|Z]) :- zip2(X, Y, Z).

/* Problem 7 Test: */
%:- zip2([1,2,3],[a,b,c],[(1,a),(2,b),(3,c)]). % SUCCEED
%:- zip2([],[a,b,c],[]).                  % SUCCEED
%:- zip2([1,3],[],[]).                    % SUCCEED
%:- zip2([1,3],[2],[(1,2)]).              % SUCCEED

%:- zip2([1],[2],[(2,3)]).                 % FAIL
%:- zip2([1],[a,b],[(1,a),(1,b)]).         % FAIL

/* Problem 8:
Write a predicate merge(A,B,M) that succeed if the list M has all the items from lists A and B in decreasing order.  Assume that A and B are sorted in decreasing order.  Items do not need to be unique.

For example:
merge([10,3,2], [11,5,2], M) should give M =[11,10,5,3,2,2].

 */

/* Problem 8 Answer: */
merge([],[],[]).
merge([],L,L).
merge(L,[],L).
merge([H1|T1], [H2|T2], [H1|R]) :- H1 >= H2, !, merge(T1, [H2|T2], R).
merge([H1|T1], [H2|T2], [H2|R]) :- merge([H1|T1], T2, R).

% https://stackoverflow.com/questions/15926034/how-to-merge-lists-in-prolog
% http://www.dailyfreecode.com/code/prolog-merge-two-ordered-list-3129.aspx

/* Problem 8 Test: */
%:- merge([10,3,2],[11,5,2],[11,10,5,3,2,2]) .       % SUCCEED
%:- merge([0],[],[0]).                               % SUCCEED
%:- merge([],[3],[3]).                               % SUCCEED

%:- merge([4,3],[3],[3]).                            % FAIL

/* Problem 9:
   See Problem 0B above for the knowledge base used for defining greater_than/2 .
   Exercise  3.4 (http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse11)
   Define a predicate greater_than/2 that takes two numerals in the notation
   that we introduced in the text (that is, 0, succ(0), succ(succ(0)), and so on)
   as arguments and decides whether the first one is greater than the second one.
*/

/* Problem 9 Answer: */
greater_than(succ(_), 0).
greater_than(succ(X), succ(Y)):- greater_than(X, Y).

/* Problem 9 Test: */
% :- greater_than(succ(succ(succ(0))),succ(0)).        % SUCCEED
% :- greater_than(succ(succ(0)),succ(succ(succ(0)))).  % FAIL




/* Problem 10:
   See Problem 0B above for the knowledge base used for defining subtract/3 .
   Define substract(Num1,Num2,Result) to succeed if Result is the result of
   Num1 - Num2.  Num1, Num2 and Result use four symbols: 0, succ , and the left and right parentheses
   to represent numbers.

   Use the add/3, from problem 0B, definition to define subtract/3.  Do not
   write a recursive definition for subtract/3.
*/


/* Problem 10 Answer: */
add(0,Y,Y).
add(succ(X),Y,succ(Z))  :- add(X,Y,Z).

subtract(0, 0, 0).
subtract(X, 0, X).
subtract(X, X, 0).
subtract(X, Y, Z) :- add(Y, Z, X).

% https://stackoverflow.com/questions/800101/prolog-sum-sub-using-successors

/* Problem 10 Test: */
% :- subtract(succ(succ(0)), succ(0), succ(0)).       % SUCCEED
% :- subtract(succ(succ(0)), 0, succ(succ(0))).       % SUCCEED
% :- subtract(succ(succ(0)), succ(succ(0)), 0).       % SUCCEED
% :- subtract(succ(succ(0)), 0, 0).	            % FAIL
% :- subtract(succ(succ(0)), succ(0), succ(succ(0))). % FAIL

/* Problem 11:
(From Learn Prolog NOW!) Binary trees are trees where all internal nodes have exactly two children. The smallest binary trees consist of only one leaf node. We will represent leaf nodes as leaf(Label). For instance, leaf(3) and leaf(7) are leaf nodes, and therefore small binary trees. Given two binary trees B1 and B2 we can combine them into one binary tree using the predicate tree: tree(B1,B2). So, from the leaves leaf(1) and leaf(2) we can build the binary tree tree(leaf(1), leaf(2)). And from the binary trees tree(leaf(1), leaf(2)) and leaf(4) we can build the binary tree tree(tree(leaf(1), leaf(2)), leaf(4)).

Now define a predicate isBinaryTree(+BT) which succeeds if BT is a binary tree. The "+" indicates that it is assumed BT is instantiate in the query.
For example:
If BT = tree( leaf(1), tree( leaf(2),leaf(4)) ), then isBinaryTree(BT) succeeds.

*/

/* Problem 11 Answer: */
leaf(_).
tree(leaf(_)).
tree(leaf(_), leaf(_)).

tree(leaf(_), tree(leaf(_), leaf(_))).
tree(tree(leaf(_), leaf(_)), leaf(_)).

isBinaryTree(nil).
isBinaryTree(tree(leaf(X), leaf(Y))) :- tree(leaf(X)), tree(leaf(Y)).
isBinaryTree(leaf(_)).

isBinaryTree(tree(tree(leaf(_), leaf(_)), leaf(_))).
isBinaryTree(tree(leaf(_), tree(leaf(_), leaf(_)))).

/* Problem 11 Test: */
%:- isBinaryTree(leaf(1)).                                           %SUCCEED
%:- isBinaryTree(tree(leaf(a),leaf(b))).                             %SUCCEED
%:- BT = tree( leaf(b), tree( leaf(x),leaf(y)) ), isBinaryTree(BT).  %SUCCEED
%:- BT = tree(tree(leaf(1), leaf(2)), tree(leaf(10), tree(leaf(4), leaf(11)))), isBinaryTree(BT).  %SUCCEED

%:- isBinaryTree( tree(leaf(1)) ).                                   % FAIL
%:- isBinaryTree( tree() ).                                          % FAIL

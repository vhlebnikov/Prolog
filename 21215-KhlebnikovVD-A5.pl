:- set_prolog_flag(double_quotes, chars).

%#################################################################
%                                                                #
% Arithmetic expression DCG (draft I made to sort out the topic) #
%                                                                #
% exp --> sum.                                                   #
% sum --> mult.                                                  #
% sum --> mult, signSum, sum.                                    #
% mult --> atom.                                                 #
% mult --> atom, signMult, mult.                                 #
% atom --> number_a.                                             #
% atom --> ['('], exp, [')'].                                    #
% number_a --> number1_a.                                        #
% number_a --> number1_a, number_a.                              #
% number1_a --> [X], {char_type(X, digit)}.                      #
%                                                                #
% signSum --> [+] | [-].                                         #
% signMult --> [*] | [/].                                        #
%                                                                #
%#################################################################

% ================================================================
% ========================= DCG Clojure ==========================
% ================================================================

% Separator

separator --> [','] | ['\s'] | ['\t'] | ['\n'].

% Special symbol

special_symbol --> [+] | [-] | [>] | [<] | [=] | [*] | ['_'].

% Number

number --> number1.
number --> number1, number.
number1 --> [X], {char_type(X, digit)}.

% String

string --> ['\"'], string1, ['\"'].
string1 --> symbol.
string1 --> string1, symbol.
symbol --> ([X], {char_type(X, alpha)}) | separator
           | special_symbol | number.

% ID

id --> id_sp | id_letter.
id_sp --> sp.
id_sp --> sp, seq.
id_letter --> letter_seq.
id_letter --> letter_seq, seq1.

sp --> special_symbol.
sp --> special_symbol, sp.
seq --> special_symbol | number.
seq --> (special_symbol | number), seq.

letter_seq --> letter.
letter_seq --> letter, letter_seq.
letter --> [X], {char_type(X, alpha)}.

seq1 --> letter | number | special_symbol.
seq1 --> (letter | number | special_symbol), seq1.

% Keyword

keyword --> [:], id_letter.

% Atom

atom --> number | string | id | keyword.

% S-expression

s_expression --> atom. % first rule
s_expression --> ['('], s_seq, [')']. % seccond rule
s_expression --> ['['], s_seq, [']']. % third rule
s_expression --> ['{','}'] | (['{'], s_seq_double, ['}']). % fourth rule

% Non-terminal symbols using in second and third rules.

s_seq --> s_expression.
s_seq --> s_expression, separator_seq, s_seq.
separator_seq --> separator.
separator_seq --> separator, separator_seq.

% Non-terminal symbols using in fourth rule.

s_seq_double --> s_expression, separator_seq, s_expression.
s_seq_double --> s_expression, separator_seq, s_expression, s_seq_double_s.
s_seq_double_s --> separator_seq, s_expression, separator_seq, s_expression.
s_seq_double_s --> separator_seq, s_expression, separator_seq, s_expression, s_seq_double_s.


% Predicate that generates phrases that are gramatically correct
% according to grammar.

gen(G, Word) :- call(G, Split, []), atomics_to_string(Split, '', Word).

% Predicate that checks whether the string is grammatically correct.

rec(G, Word) :- string_chars(Word, Chars), call(G, Chars, []),!.

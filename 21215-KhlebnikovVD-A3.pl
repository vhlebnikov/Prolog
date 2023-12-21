:- use_module(library(clpfd)).

:- dynamic derived/1.
:- dynamic asked/1.
:- dynamic rejected/1.

:- op(800, xfx, <==).
:- op(800, fx, if).
:- op(700, xfx, then).
:- op(300, xfy, or).
:- op(200, xfy, and).
:- op(100, xfx, be).

:-
    retractall(derived(_)),
    retractall(rejected(_)),
    retractall(asked(_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         KNOWLEDGE BASE                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Properties we can ask about. Each provided with its' domain. If domain consists of
% a single value then property is a binary one, otherwise we should show a menu.

askable(pain_type, [in_chest, in_head, in_stomach, in_lower_abdomen]).                                  % Type of pain
askable(throat, [hurting]).                                                                             % Does throat hurt?
askable(legs, [hurting]).                                                                               % Do legs hurt?
askable(cough, [existing]).                                                                             % Is there a cough
askable(pain, [abdominal]).                                                                             % Is there abdominal pain
askable(cough_type, [dry, wet]).                                                                        % Cough type
askable(temperature, [elevated, high]).                                                                 % Temperature type
askable(nasal_dischare, [runny_nose, sputum]).                                                          % Type of nasal discharge
askable(skin_inflammation, [rash]).                                                                     % Is there skin rash
askable(abdominal_discomfort, [nausea, vomiting]).                                                      % Type of abdominal discomfort
askable(flatulence, [existing]).                                                                        % Is there flatulence
askable(defecation_problem, [blood_in_the_stool, alternation_of_diarrhea_with_constipation, diarrhea]). % Type of defecation problem
askable(problem_with_defication, [existing]).                                                           % Is there defication problem
askable(appetite, [poor]).                                                                              % Is there poor appetite
askable(dizziness, [existing]).                                                                         % Is there dizziness
askable(dizziness_type, [rare, frequent]).                                                              % Type of dizziness frequency
askable(heartbeat, [irregular, fast]).                                                                  % Type of heartbeat
askable(vessels, [enlarged]).                                                                           % Are there vessels enlarged
askable(breathlessness, [existing]).                                                                    % Is there breathlessness


% Production rules

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Infectious diseases
if
    pain_type be in_head and cough be existing
then
    group be infectious_diseases.

% Gastrointestinal diseases
if
    pain_type be in_stomach or pain_type be in_lower_abdomen

then
    group be gastrointestinal_diseases.

% Cardiovascular diseases
if
    (pain_type be in_head or pain_type be in_chest) and dizziness be existing
then
    group be cardiovascular_diseases.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Virus infections
if
    group be infectious_diseases and cough_type be dry and nasal_dischare be runny_nose and throat be hurting
then
    type be virus_infections.

% Bacterial infections

if
    group be infectious_diseases and cough_type be wet and nasal_dischare be sputum and temperature be elevated and throat be heartnig
then
    type be bacterial_infections.

% Fungal infections

if
    group be infectious_diseases and cough_type be dry and temperature be elevated and nasal_dischare be sputum
then
    type be fungal_infections.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Stomach diseases

if
    group be gastrointestinal_diseases and pain_type be in_stomach and flatulence be existing
then
    type be stomach_diseases.

% Bowel diseases

if
    group be gastrointestinal_diseases and pain_type be in_lower_abdomen and problem_with_defication be existing
then
    type be bowel_diseases.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Heart defects

if
    group be cardiovascular_diseases and breathlessness be existing
then
    type be heart_defects.

% Vascular damage

if
    group be cardiovascular_diseases and vessels be enlarged
then
    type be vascular_damage.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ORVI
if
    type be virus_infections and temperature be elevated
then
    disease be orvi.

% Pneumonia
if
    type be virus_infections and temperature be high and abdominal_discomfort be nausea
then
    disease be pneumonia.

% Tuberculosis

if
    type be bacterial_infections and appetite be poor
then
    disease be tuberculosis.

% Cholera

if
    type be bacterial_infections and abdominal_discomfort be vomiting and defecation_problem be diarrhea
then
    disease be cholera.


% Candidiasis

if
    type be fungal_infections and defecation_problem be blood_in_the_stool and skin_inflammation be rash
then
    disease be candidiasis.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Gastritis

if
    type be stomach_diseases and abdominal_discomfort be nausea and appetite be poor
then
    disease be gastritis.

% Ulcer

if
    type be stomach_diseases and abdominal_discomfort be vomiting and defecation_problem be blood_in_the_stool
then
    disease be ucler.

% Colitis

if
    type be bowel_diseases and defecation_problem be diarrhea
then
    disease be colitis.

% Celiac disease

if
    type be bowel_diseases and skin_inflammation be rash and defecation_problem be alternation_of_diarrhea_with_constipation
then
    disease be celiac_disease.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Pulmonary artery stenosis

if
    type be heart_defects and dizziness_type be rare and heartbeat be fast
then
    disease be pulmonary_artery_stenosis.

% Stenosis of the aortic mouth

if
    type be heart_defects and dizziness_type be frequent and heartbeat be irregular
then
    disease be stenosis_of_the_aortic_mouth.

% Varicose veins

if
     type be vascular_damage and legs be hurting
then
    disease be varicose_veins.

% Aneurysm

if
     type be vascular_damage and dizziness_type be rare and abdominal_discomfort be nausea
then
    disease be aneurysm.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         KNOWLEDGE BASE                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

true(Statement, Proof) :-
                            retractall(derived(_)),
                            retractall(asked(_)),
                            retractall(rejected(_)),
                            true(Statement, Proof, []).

true(Statement, Statement, _) :- derived(Statement).
true(S1 and S2, P1 and P2, Trace) :-
                                    true(S1, P1, Trace),
                                    true(S2, P2, Trace).
true(S1 or S2, P, Trace) :-
                                    true(S1, P, Trace) ;
                                    true(S2, P, Trace).
true(Conclusion, Conclusion <== ConditionProof, Trace) :-
                                                        if Condition then Conclusion,
                                                        true(Condition, ConditionProof, [if Condition then Conclusion | Trace]).
true(Statement, Proof, Trace) :-
                                    Statement = Subject be _,
                                    askable(Subject, Menu),
                                    \+ derived(Statement),
                                    \+ asked(Subject),
                                    ask(Statement, Subject, Proof, Trace, Menu).

ask(Statement, Subject, Proof, Trace, [Val]) :-
                                format('\nIs it true that ~w is ~w ? Please answer \'yes\', \'no\' or \'why\'.\n',[Subject, Val]),
                                read_string(user_input, "\n", "\r\t", _, Answer),
                                process_yes_no(Answer, Statement, Subject, Proof, Trace, [Val]).

ask(Statement, Subject, Proof, Trace, [V,V1|Vs]) :-
                                Menu = [V,V1|Vs],
                                format('\nOf what type ~w is? Type an integer or \'why\'.\n', [Subject]),
                                show_menu(1, Menu),
                                read_string(user_input, "\n", "\r\t", _, Answer),
                                process(Answer, Statement, Subject, Proof, Trace, Menu).

show_menu(_, []).
show_menu(Counter, [V|Vs]) :-
                            format('~d: ~w\n', [Counter, V]),
                            Next #= Counter + 1,
                            show_menu(Next, Vs).

process("why", St, S, Proof, Trace, Menu) :- !,
                                        show_reasoning_chain(Trace, 0), nl,
                                        ask(St, S, Proof, Trace, Menu).

process(StrInd, St, S, Proof <== is_stated, _, Menu) :-
                                            atom_number(StrInd, Index),
                                            nth1(Index, Menu, Answer),
                                            !,
                                            Proposition = S be Answer,
                                            Proof = Proposition,
                                            asserta(derived(Proposition)),
                                            asserta(asked(S)),
                                            St == Proposition.

process(_, Statement, Subject, Proof, Trace, Menu) :-
                                            write('Incorrect answer! Try again, please\n'),
                                            ask(Statement, Subject, Proof, Trace, Menu).

process_yes_no("yes", S, Subj, S <== is_stated, _, _) :- !,
                                        asserta(derived(S)),
                                        asserta(asked(Subj)).
process_yes_no("no", _, S, _, _, _) :-   !,
                            asserta(asked(S)),
                            fail.
process_yes_no("why", Statement, Subject, Proof, Trace, Menu) :-  !,
                                            show_reasoning_chain(Trace, 0), nl,
                                            ask(Statement, Subject, Proof, Trace, Menu).
process_yes_no(_, Statement, Subject, Proof, Trace, Menu) :-
                        write('Please answer \'yes\', \'no\' or \'why\'!\n'),
                        read_string(user_input, "\n", "\r\t", _, Answer),
                        process_yes_no(Answer, Statement, Subject, Proof, Trace, Menu).


show_reasoning_chain([], _).
show_reasoning_chain([if Cond then Concl | Rules], _) :-
                                                        format('\n   To infer ~w, using rule\n   (if ~w then ~w)', [Concl, Cond, Concl]),
                                                        show_reasoning_chain(Rules, _).

derivable(CondPart, Concl, Concl <== How) :-
                                if Cond then Concl,
                                contains_term(CondPart, Cond),
                                \+ derived(Concl),
                                \+ rejected(Concl),
                                (true(Concl, How, []) -> !, asserta(derived(Concl)); asserta(rejected(Concl)), fail).

infer(Cond, Concl, Prev, Expl) :-
                            derivable(Cond, Concl1, Trace),
                            !,
                            infer(Concl1, Concl, Trace, Expl);
                            Expl = Prev,
                            Concl = Cond.


infer(Conclusion, Proof) :-
                            infer(_, Conclusion, 'Can\'t infer something from the information provided.' , Proof), !.

clear :- retractall(derived(_)), retractall(rejected(_)), retractall(asked(_)).

start :-
            clear,
            writeln('Welcome to the expert system for determining the disease!'),
            nl,
            writeln('THE RESULTS DO NOT GUARANTEE THE PRESENCE OF THIS DISEASE.'),
            writeln('ONLY A DOCTOR CAN MAKE A DIAGNOSIS!'),
            infer(Conclusion, How),
            format('Conclusion: ~w\nExplanation: ~w', [Conclusion, How]).

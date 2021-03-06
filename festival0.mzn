include "count.mzn";
include "disjunctive_opt.mzn";
include "globals.mzn";

int: nPeces;
int: nMusics;
int: nInstruments;
int: nPrecs;
int: pressupost;

set of int: Peces = 1..nPeces;
set of int: Musics = 1..nMusics;
set of int: Instruments = 1..nInstruments;
set of int: Precs = 1..nPrecs;

array [Peces] of int: durada;
array [Peces, Instruments] of int: requereix;
array [Musics, Instruments] of bool: saptocar;
array [Musics] of int: salari;

array[Precs] of int: pred;
array[Precs] of int: succ;

int: maxMinuts = sum(durada) div 5;
set of int: MaxMinuts = 0..maxMinuts;

%VARIABLES

array[Peces, Musics] of var 0..nInstruments: instrumentsXmusics; %0 significa que no en toca cap. Per tant, no toca en aquesta peça
array[Peces] of var MaxMinuts: minutInici;
array[Musics] of var 0..sum(durada): minutsTocatsXmusic :: is_defined_var; %variable auxiliar per a saber quants minuts toca cada Músic
array[Peces] of var 0..sum(durada): quanAcaba :: is_defined_var; %variable auxiliar per a saber quan acaba cada Peça


%1 constraint per fer q un music no toqui més de 1 instrument a la vegada |||||||-->  NO CAL, ÉS IMPLICITA AL VIEWPOINT. Per cada Peça un music nomes pot tocar un instrument!!!

%2 constraint per a que un music només toqui elements que sap tocar
constraint forall( p in Peces, m in Musics)(
    saptocar[m, instrumentsXmusics[p,m]] \/ (instrumentsXmusics[p,m] == 0)
);

%3 constraint per tal que tots els insturments necessaris siguin tocats
constraint forall(p in Peces, i in Instruments)(
    count([instrumentsXmusics[p,m]|m in Musics], i, requereix[p,i])
);

%4 constraint per a que un music només toqui un cop a la vegada
constraint forall (p in Peces, m in Musics where instrumentsXmusics[p,m] != 0 )(
    forall (p2 in p+1..nPeces where instrumentsXmusics[p2,m] != 0 )(
        ( ( (minutInici[p]*5+durada[p]) <= minutInici[p2]*5) \/ ((minutInici[p2]*5 + durada[p2]) <= minutInici[p]*5) ) 
        /\ ( ( (minutInici[p2]*5) > minutInici[p]*5) \/ ((minutInici[p2]*5) < minutInici[p]*5) ) 
));

%5 constraint per a estar dins del pressupost
constraint forall(m in Musics)(
    minutsTocatsXmusic[m] = sum(p in Peces where instrumentsXmusics[p,m]!=0)(durada[p]) :: defines_var(minutsTocatsXmusic[m])
);
constraint int_lin_le(salari,minutsTocatsXmusic, pressupost*5) :: domain;

%6 constraint per a complir les precedencies requerides
constraint forall(pr in Precs)(
    minutInici[pred[pr]]*5 + durada[pred[pr]] <= minutInici[succ[pr]]*5
);

constraint forall(p in Peces)(
    quanAcaba[p] = minutInici[p]*5 + durada[p] :: defines_var(quanAcaba[p])
);

%IMPLICADES

constraint max(quanAcaba) >= max(durada) :: bounds;
constraint min(quanAcaba) >= min(durada) :: bounds;

% solve minimize max(quanAcaba);
solve :: int_search(minutInici, anti_first_fail, indomain, complete) minimize max(quanAcaba);
% solve :: int_search(minutInici, most_constrained, indomain_split, complete) minimize max(quanAcaba);
% solve satisfy;
output ["Hora d’inici del festival: ",show(min(minutInici)*5 div 60+10),"h ",show(min(minutInici)*5 mod 60),"min",
"\n","Hora d’acabada del festival: ",show(max([(durada[p] div 5)+minutInici[p]|p in Peces])*5 div 60+10),"h ",show((max([(durada[p] div 5)+minutInici[p]|p in Peces])*5) mod 60),"min",
"\n","Cost de contractacio: ",show(sum([minutsTocatsXmusic[m] div 5 *salari[m]|m in Musics]))," euros\n"
]
++
["Peca " ++ show(p) ++ ":\n" ++ "    Inici: " ++ show(minutInici[p]*5 div 60+10)++"h "++show(minutInici[p]*5 mod 60)++"min"++"\n"++
"    Final: " ++ show((((durada[p] div 5)+(minutInici[p]*5)) div 60+10))++"h "++show(((durada[p] div 5) + minutInici[p])*5 mod 60 )++"min"++"\n"++
join("",["    Music " ++ show(m) ++ " toca instrument " ++ show(instrumentsXmusics[p,m]) ++  "\n"|m in Musics where fix(instrumentsXmusics[p,m]!=0)])
| p in Peces]
%output ["final = \(10 + max(quanAcaba) div 60) : \(max(quanAcaba) mod 60)\nminutInici = \(minutInici) \n"]

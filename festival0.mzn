include "count.mzn";
include "disjunctive_opt.mzn";
%1: matriu[peces, musics] of int sales, matriu[peces] of int minuts// falta algo per controlar quin instrument toca?
%%%%%% sales es de 1..nPeces i els minuts es 0..sumaDuracioMusiques
%%%%%% necessitarem una variable amb la duració final, que es el que hem de minimitzar

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


int: maxMinuts = sum(durada);
set of int: MaxMinuts = 0..maxMinuts;

%model

%he canviat per instruments, perque no ens importen les sales. No nhem de saber res. En canvi hem de controlar insturments, cosa q no feiem en el viewpoint anterior.
array[Peces, Musics] of var 0..nInstruments: instrumentsXmusics; %0 significa que no en toca cap. Per tant, no toca en aquesta peça
array[Peces] of var MaxMinuts: minutInici;
array[Musics] of var MaxMinuts: minutsTocatsXmusic;
array[Peces] of var MaxMinuts: quanAcaba; %variable auxiliar per a saber quan acaba cada Peça



%%q sha de fer?

%1 constraint per fer q un music no toqui més de 1 instrument a la vegada |||||||-->  NO CAL, ÉS IMPLICITA AL VIEW POINT. Per cada Peça un music nomes pot tocar un instrument!!!

%2 constraint per a que un music només toqui elements que sap tocar
%%%%%%% cal mirar per cada per cada Peça que cada music sapiga tocar el que li diuen que toqui.
constraint forall( p in Peces, m in Musics)(
    (saptocar[m, instrumentsXmusics[p,m]] = true) xor instrumentsXmusics[p,m] = 0 
);

%3 constraint per tal que tots els insturments necessaris siguin tocats 

constraint forall(p in Peces, i in Instruments)(
    count([instrumentsXmusics[p,m]|m in Musics], i, requereix[p,i])
);


%4 constraint per a que un music només toqui un cop a la vegada
%%%%%%% cal mirar que les peces q es solapen no tinguin musics comuns.

constraint forall (p in Peces, m in Musics where instrumentsXmusics[p,m] != 0 )(
    forall (p2 in Peces where instrumentsXmusics[p2,m] != 0 /\ p2!=p)(
        minutInici[p] > minutInici[p2]+durada[p2] \/ minutInici[p] + durada[p] > minutInici[p2]
    )
);

%5 constraint per a estar dins del pressupost
%%%%%%% mirar quants minuts toquen, multiplicar pel salari de cada music. Comprovar q el resultat no és mes gran q el pressupost.
constraint forall(m in Musics)(
    minutsTocatsXmusic[m] = sum(p in Peces where instrumentsXmusics[p,m]!=0)(durada[p])
);
constraint pressupost >= sum(m in Musics)((minutsTocatsXmusic[m]/5)*salari[m]);

%6 constraint per a complir les precedencies requerides
%%%% quan comença el successor el precesedor ha de haber acabat.

constraint forall(pr in Precs)(
    minutInici[pred[pr]] + durada[pred[pr]] > minutInici[succ[pr]]
);



constraint forall(p in Peces)(
    quanAcaba[p] = minutInici[p] + durada[p]
);



solve minimize max(quanAcaba);
% solve satisfy;
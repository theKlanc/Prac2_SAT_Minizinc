array[Peces, Musics] of var 0..nInstruments: instrumentsXmusics;
array[Peces] of var MaxMinuts: minutInici;
array[Musics] of var 0..sum(durada): minutsTocatsXmusic :: is_defined_var;
array[Peces] of var 0..sum(durada): quanAcaba :: is_defined_var;

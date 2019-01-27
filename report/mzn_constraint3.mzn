constraint forall (p in Peces, m in Musics where instrumentsXmusics[p,m] != 0 )(
    forall (p2 in p+1..nPeces where instrumentsXmusics[p2,m] != 0 )(
        ( ( (minutInici[p]*5+durada[p]) <= minutInici[p2]*5) \/ ((minutInici[p2]*5 + durada[p2]) <= minutInici[p]*5) ) 
        /\ ( ( (minutInici[p2]*5) > minutInici[p]*5) \/ ((minutInici[p2]*5) < minutInici[p]*5) ) 
)) :: domain;
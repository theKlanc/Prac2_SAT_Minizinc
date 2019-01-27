constraint forall( p in Peces, m in Musics)(
    saptocar[m, instrumentsXmusics[p,m]] \/ (instrumentsXmusics[p,m] == 0)
);
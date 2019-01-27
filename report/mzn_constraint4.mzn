constraint forall(m in Musics)(
    minutsTocatsXmusic[m] = sum(p in Peces where instrumentsXmusics[p,m]!=0)(durada[p]) :: defines_var(minutsTocatsXmusic[m])
);
constraint int_lin_le(salari,minutsTocatsXmusic, pressupost*5);
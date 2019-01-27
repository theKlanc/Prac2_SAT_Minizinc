constraint forall(p in Peces)(
    quanAcaba[p] = minutInici[p]*5 + durada[p] :: defines_var(quanAcaba[p])
);
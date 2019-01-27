constraint forall(pr in Precs)(
    minutInici[pred[pr]]*5 + durada[pred[pr]] <= minutInici[succ[pr]]*5
);

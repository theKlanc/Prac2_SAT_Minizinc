object Reines3 {
  def main(args: Array[String]): Unit ={
    var e = new ScalAT
    e.setWorkingPath("./")
    e.setSolverPath("src/main/scala/")
    val N = 120

    //La variable a graella(i,j) ens indica si a la posició i,j hi ha una reina o no.
    var graella = e.newVar2DArray(N,N)

    for(i <- 0 until N) {
      var l: List[Int] = List()
      for (j <- 0 until N) {
        l ::= graella(j)(i)
      }
      e.addEOLog(l)
    }

    //una reina per columna
    for(i <- 0 until N) {
      var l: List[Int] = List()
      for (j <- 0 until N) {
        l ::= graella(i)(j)
      }
      e.addEOLog(l)
    }

    //una reina per diagonal
    for(i<-0 until N) {
      var l1: List[Int] = List()
      var l2: List[Int] = List()
      for (j <- 0 until N) {
        if(j+i<N)l1 ::= graella(j+i)(j)
        if(i!=0 && j-i>=0)l2 ::= graella(j-i)(j)
      }
      e.addAMOLog(l1)
      e.addAMOLog(l2)
    }
    //una reina per diagonal inversa
    for(i<-0 until N) {
      var l1: List[Int] = List()
      var l2: List[Int] = List()
      for (j <- 0 to N) {
        if(j+i<N && N-j-1>=0)l1 ::= graella(j+i)(N-j-1)
        if(i!=0 && j-i>=0 && N-j-1>=0)l2 ::= graella(j-i)(N-j-1)
      }
      e.addAMOLog(l1)
      e.addAMOLog(l2)
    }


    //Condició implicada per a que només hi hagi N reines al tauler
    var llista:List[Int] = List()

    for(i<-0 until N){
      llista = llista ::: graella(i).toList
    }
    e.addEK(llista, N)

    //Solucionem
    if (e.solve()) {
      for(i <- 0 until N){
        for(j<- 0 until N) {
          if (e.getValue(graella(i)(j))) print(1)
          else print(0)
          print(" ")
        }
        println()
      }

    }

    //Printem estadistiques de solucio
    println("Solving time: " + e.getTime.toString + " seconds")
    println("N vars: " + e.getNVars)
    println("N clauses: " +  e.getNClauses)
    println("N decisions: " + e.getDecisions)
    println("N propagations: " + e.getPropagations)
    println("N conflicts: " + e.getConflicts)
  }
}

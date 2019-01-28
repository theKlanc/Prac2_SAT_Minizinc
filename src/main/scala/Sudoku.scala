object Sudoku {
  def main(args: Array[String]){
    var e = new ScalAT
    e.setWorkingPath("./")
    e.setSolverPath("src/main/scala/")

    //graella(i)(j)(k) es cert sii a la casella (i)(j) hi ha el valor k
    //Agafem valors de 0 a 8 enlloc de 1 a 9
    var graella = e.newVar3DArray(9,9,9)

    //Cada casella te exactament un valor
    for(i <- 0 to 8;j <- 0 to 8)
        e.addEOQuad(graella(i)(j).toList)

    //A la columna 'j' hi ha com a molt un 'k'
    for(j <- 0 to 8;k <- 0 to 8) {
      var l: List[Int] = List()
      for (i <- 0 to 8) {
        l ::= graella(i)(j)(k)
      }
      e.addAMOLog(l)
    }

    //A la fila 'i' hi ha com a molt un 'k'
    for(i <- 0 to 8; k <- 0 to 8) {
      var l: List[Int] = List()
      for (j <- 0 to 8) {
        l ::= graella(i)(j)(k)
      }
      e.addAMOLog(l)
    }

    //Al cuadrat (i,j) hi ha com a molt un 'k'
    for(i <- List(0,3,6);j <- List(0,3,6);k <- 0 to 8){
      var l: List[Int] = List()
      for(i2 <- List(0,1,2); j2<-List(0,1,2)){
        l::=graella(i+i2)(j+j2)(k)
      }
      e.addAMOLog(l)
    }



    //Instancia concreta (diapositives de teoria)
    e.addClause(graella(0)(0)(4) :: List())
    e.addClause(graella(0)(1)(6) :: List())
    e.addClause(graella(0)(3)(5) :: List())
    e.addClause(graella(0)(6)(2) :: List())
    e.addClause(graella(0)(8)(8) :: List())
    e.addClause(graella(1)(1)(1) :: List())
    e.addClause(graella(1)(3)(2) :: List())
    e.addClause(graella(1)(5)(8) :: List())
    e.addClause(graella(1)(7)(6) :: List())
    e.addClause(graella(1)(8)(0) :: List())
    e.addClause(graella(2)(0)(0) :: List())
    e.addClause(graella(2)(4)(7) :: List())
    e.addClause(graella(3)(1)(4) :: List())
    e.addClause(graella(3)(3)(6) :: List())
    e.addClause(graella(3)(5)(2) :: List())
    e.addClause(graella(3)(7)(7) :: List())
    e.addClause(graella(3)(8)(5) :: List())
    e.addClause(graella(4)(2)(5) :: List())
    e.addClause(graella(4)(6)(3) :: List())
    e.addClause(graella(5)(0)(3) :: List())
    e.addClause(graella(5)(1)(0) :: List())
    e.addClause(graella(5)(3)(7) :: List())
    e.addClause(graella(5)(5)(5) :: List())
    e.addClause(graella(5)(7)(4) :: List())
    e.addClause(graella(6)(4)(5) :: List())
    e.addClause(graella(6)(8)(1) :: List())
    e.addClause(graella(7)(0)(7) :: List())
    e.addClause(graella(7)(1)(8) :: List())
    e.addClause(graella(7)(3)(4) :: List())
    e.addClause(graella(7)(5)(1) :: List())
    e.addClause(graella(7)(7)(5) :: List())
    e.addClause(graella(8)(0)(1) :: List())
    e.addClause(graella(8)(2)(2) :: List())
    e.addClause(graella(8)(5)(3) :: List())
    e.addClause(graella(8)(7)(0) :: List())
    e.addClause(graella(8)(8)(7) :: List())



    //Solucionem
    if (e.solve()) {

      for(i <- 0 to 8){
        for(j<- 0 to 8) {
          for (k <- 0 to 8)
            if (e.getValue(graella(i)(j)(k))) print(k+1+" ")
          if(j%3==2) print(" ")
        }
        println
        if(i%3==2) println
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

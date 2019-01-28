object TestSorter {
  def main(args: Array[String]){
    var e = new ScalAT;
    e.setWorkingPath("/home/klanc/Projects/Uni/4t/ProgDec/Prac2_SAT_Minizinc/")
    e.setSolverPath("/home/klanc/Projects/Uni/4t/ProgDec/Prac2_SAT_Minizinc/src/main/scala/")


    //Comprovem el funcionament d'un sorter


    var n = 30;
    var x = e.newVarArray(n)
    var y = e.newVarArray(n)

    //Creem una llista desordenada 'x'
    for(i <- 0 to n-1) {
      if(i%3==0)
        e.addClause(x(i) :: List())
      else
        e.addClause(-x(i) :: List())
    }

    //Ordenem x
    e.addSorter(x.toList,y.toList)

    //Solucionem
    if (e.solve()) {
      print("x: ")
      for(v <- x){
          print((if(e.getValue(v)) "1 " else "0 "))
      }
      println()
      print("y: ")
      for(v <- y){
        print((if(e.getValue(v)) "1 " else "0 "))
      }
    }

    println()
    println("Solving time: " + e.getTime.toString + " seconds")
    println("N vars: " + e.getNVars)
    println("N clauses: " +  e.getNClauses)
    println("N decisions: " + e.getDecisions)
    println("N propagations: " + e.getPropagations)
    println("N conflicts: " + e.getConflicts)

  }
}

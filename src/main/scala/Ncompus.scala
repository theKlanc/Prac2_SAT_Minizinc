object Ncompus {
  def main(args: Array[String]) {
    var e = new ScalAT
    e.setWorkingPath("./")
    e.setSolverPath("src/main/scala/")

    val M1 = 10
    val N1 = 6
    val R1 = 3
    val U1 = 8
    val conSat1 = Array.ofDim[Boolean](N1, N1)
    conSat1(0)(0) = false
    conSat1(0)(1) = true
    conSat1(0)(2) = true
    conSat1(0)(3) = true
    conSat1(0)(4) = false
    conSat1(0)(5) = false
    conSat1(1)(0) = true
    conSat1(1)(1) = false
    conSat1(1)(2) = false
    conSat1(1)(3) = false
    conSat1(1)(4) = false
    conSat1(1)(5) = false
    conSat1(2)(0) = true
    conSat1(2)(1) = false
    conSat1(2)(2) = false
    conSat1(2)(3) = true
    conSat1(2)(4) = false
    conSat1(2)(5) = true
    conSat1(3)(0) = true
    conSat1(3)(1) = false
    conSat1(3)(2) = true
    conSat1(3)(3) = false
    conSat1(3)(4) = false
    conSat1(3)(5) = false
    conSat1(4)(0) = false
    conSat1(4)(1) = false
    conSat1(4)(2) = false
    conSat1(4)(3) = false
    conSat1(4)(4) = false
    conSat1(4)(5) = true
    conSat1(5)(0) = false
    conSat1(5)(1) = false
    conSat1(5)(2) = true
    conSat1(5)(3) = false
    conSat1(5)(4) = true
    conSat1(5)(5) = false

    val M2 = 10
    val N2 = 7
    val R2 = 3
    val U2 = 11
    val conSat2 = Array.ofDim[Boolean](N2, N2)
    conSat2(0)(0)=false
    conSat2(0)(1)=true
    conSat2(0)(2)=true
    conSat2(0)(3)=true
    conSat2(0)(4)=false
    conSat2(0)(5)=false
    conSat2(0)(6)=false
    conSat2(1)(0)=true
    conSat2(1)(1)=false
    conSat2(1)(2)=false
    conSat2(1)(3)=false
    conSat2(1)(4)=false
    conSat2(1)(5)=false
    conSat2(1)(6)=true
    conSat2(2)(0)=true
    conSat2(2)(1)=false
    conSat2(2)(2)=false
    conSat2(2)(3)=true
    conSat2(2)(4)=false
    conSat2(2)(5)=true
    conSat2(2)(6)=false
    conSat2(3)(0)=true
    conSat2(3)(1)=false
    conSat2(3)(2)=true
    conSat2(3)(3)=false
    conSat2(3)(4)=false
    conSat2(3)(5)=false
    conSat2(3)(6)=false
    conSat2(4)(0)=false
    conSat2(4)(1)=false
    conSat2(4)(2)=false
    conSat2(4)(3)=false
    conSat2(4)(4)=false
    conSat2(4)(5)=true
    conSat2(4)(6)=true
    conSat2(5)(0)=false
    conSat2(5)(1)=false
    conSat2(5)(2)=true
    conSat2(5)(3)=false
    conSat2(5)(4)=true
    conSat2(5)(5)=false
    conSat2(5)(6)=true
    conSat2(6)(0)=false
    conSat2(6)(1)=true
    conSat2(6)(2)=false
    conSat2(6)(3)=false
    conSat2(6)(4)=true
    conSat2(6)(5)=true
    conSat2(6)(6)=true

    val M         = M2
    val N         = N2
    val R         = R2
    val U         = U2
    val con =  conSat2


    ///graella(i)(j) es true si i esta a la xarxa j
    var graella = e.newVar2DArray(N, M)

    //U és el maxim de connexions, per tant...
    var l: List[Int] = List()
    for(i <- 0 until N;j <- 0 until M) {
        l ::= graella(i)(j)
    }
    e.addAMK(l,U)
    //R és el maxim de connexions per xarxa, ergo...
    for(i <- 0 until M) {
      var k: List[Int] = List()
      for(j <- 0 until N){
        k::=graella(j)(i)
      }
      e.addAMK(k,R)
    }

    //restricció de connexions necessaries
    for(con1 <- 0 until N){
      for(con2 <- 0 until N){
        if(con(con1)(con2)){
          for(m1<-0 until Math.pow(M,2).toInt){
            var k: List[Int] = List()
            var m3: Int = m1
            for(m2<-0 until M){
              k::=graella(
                if // volem saber si en binari, el digit de la posicio m2 de m1 és 0 o 1
                (m3%2==0)
                  con1
                else
                  con2
              )(m2)
              m3 = m3/2
            }
            e.addClause(k)
          }
        }
      }
    }


    //Solucionem i mostrem
    if(e.solve()){
      for(i<-0 until N) {
        print(i)
        print(":")
        for (j <- 0 until M) {
          if (e.getValue(graella(i)(j))) {
            print("1")
          }
          else print("0")
        }
        println()
      }
    }


    //Printem estadistiques de solucio
    println("Solving time: " + e.getTime.toString + " seconds")
    println("N vars: " + e.getNVars)
    println("N clauses: " + e.getNClauses)
    println("N decisions: " + e.getDecisions)
    println("N propagations: " + e.getPropagations)
    println("N conflicts: " + e.getConflicts)
  }

}
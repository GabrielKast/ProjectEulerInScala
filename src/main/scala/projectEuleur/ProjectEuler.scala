package projecteuler


object Euler{
  import utils._
  def problem1(x :Int) = List.range(1, x).filter(x => x % 3==0 || x % 5 ==0).sum
  def problem2(lim :Int) = utils.fibs takeWhile( _ < lim) filter (_ % 2 ==0) sum

  //   3. What is the largest prime factor of the number 317584931803?
  def problem3(n :Long) :Long= {
    def findValid(x :Long) :Long= if (x==1) 1 
			    else if (n % x ==0 && isPrime(x)) x
			    else findValid(x-1)
    findValid(math.sqrt(n).toLong)
  }

  def problem4() :Long= {
    val range = List.range(1000l, 100l, -1)
    range.foldLeft(0l)((max :Long, y) => (max :: (range.map ( _ * y ).filter( x => (x >= max && isPalindrom(x)) ))).sortWith((a, b) => (a > b)).head)
  }

  def problem6(n :Int)= {
    val range :List[Int] = List.range(1, n+1)
    val sumOfSquare :Long = range.map ( x => x * x).sum
    val squareOfSum :Long = range.sum match {case n => n*n }
    math.abs(sumOfSquare - squareOfSum)
  }

  def problem7(nth :Int)= {
    def nthPrime(nth :Int, n :Long) :Long= {
      if (nth==1) n
      else {
	val newN = n + 2
	isPrime(newN) match {
	  case true =>  nthPrime(nth-1, newN)
	  case false => nthPrime(nth, newN)
	}
      }
    }
    nthPrime(nth -1 , 3)
  }

}


object utils {
  // lazy val fibs: Stream[Int] = 0 #:: 1 #:: ((fibs zip fibs.tail) map { case (c1,c2) => c1 + c2 })
  lazy val fibs: Stream[Int] = Stream.cons(0, Stream.cons(1, fibs.zip(fibs.tail) map { case (c1,c2) => c1 + c2 } ))

  // lazy naturals :Strean[Int] = 2 #:: 3:: 
  def isPrime(n :Long) :Boolean = { 
    val nSquare = math.sqrt(n)
    def isDividedBy(n :Long, m :Long): Boolean = 
      if (m > nSquare) false
      else if (n % m == 0) true
      else isDividedBy(n, m+2)
    if (n <= 3) true
    else if (n % 2 ==0) false 
    else ! isDividedBy(n, 3)
  }

  def isPalindrom(n :Long)=n.toString.reverse == n.toString

}

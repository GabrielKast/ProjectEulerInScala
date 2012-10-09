package projecteuler

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import projecteuler.Euler._

@RunWith(classOf[JUnitRunner])
class ProjectEulerTestSuite extends FunSuite {
  test("1. Find the sum of all the multiples of 3 or 5 below 1000."){
    assert(problem1(10) === 23)
    assert(problem1(1000) === 233168)
  }

  test("2. By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.") {
    assert(problem2(90) ===  2 + 8 + 34)
    assert(problem2(4000000) ===  4613732)
  }

  test("3. What is the largest prime factor of the number 317584931803?") {
    assert(problem3(600851475143l) ===  6857)
  }

  test("4.  Find the largest palindrome made from the product of two 3-digit numbers."){
    assert(problem4()===906609)
  }

  test("6.  Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum."){
    assert(problem6(10)===2640)
    assert(problem6(100)===25164150)
  }

  /*
   * problem7

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
What is the 10 001st prime number?
*/
  test("7. 10 001st prime number"){
    assert(problem7(6)===13)
    assert(problem7(10001)===104743)
  }

  /*
   *
   * problem8
   Find the greatest product of five consecutive digits in the 1000-digit number.
   */
  test("8. Find the greatest product of five consecutive digits in the 1000-digit number.") {
    assert(problem8("123456543") === 4*5*6*5*4)

    assert(problem8("""
		    12345
		    6543
		    """) === 4*5*6*5*4)

    val digits= """
    73167176531330624919225119674426574742355349194934
    96983520312774506326239578318016984801869478851843
    85861560789112949495459501737958331952853208805511
    12540698747158523863050715693290963295227443043557
    66896648950445244523161731856403098711121722383113
    62229893423380308135336276614282806444486645238749
    30358907296290491560440772390713810515859307960866
    70172427121883998797908792274921901699720888093776
    65727333001053367881220235421809751254540594752243
    52584907711670556013604839586446706324415722155397
    53697817977846174064955149290862569321978468622482
    83972241375657056057490261407972968652414535100474
    82166370484403199890008895243450658541227588666881
    16427171479924442928230863465674813919123162824586
    17866458359124566529476545682848912883142607690042
    24219022671055626321111109370544217506941658960408
    07198403850962455444362981230987879927244284909188
    84580156166097919133875499200524063689912560717606
    05886116467109405077541002256983155200055935729725
    71636269561882670428252483600823257530420752963450
    """
    assert(problem8(digits) === 40824)
  }
/*
 *
 * 

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
 */
  test("Find the only Pythagorean triplet, {a, b, c}, for which a^2 + b^2 + c^2 = 1000.") {
    assert(problem9() === 31875000) // 200,375,425
  }


 /* Problem10
  * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
  * Find the sum of all the primes below two million.
*/
  test("10. Find the sum of all the primes below two million.") {
    assert(problem10(10)===17)
    assert(problem10(2000000) === 142913828922l)
  }

  test("11. Find the greatest product of four adjacent numbers in any direction") {

    val grid = 
      Array(
	Array(8,2,22,97,38,15,0,40,0,75,4,5,7,78,52,12,50,77,91,8),
	Array(49,49,99,40,17,81,18,57,60,87,17,40,98,43,69,48,4,56,62,0),
	Array(81,49,31,73,55,79,14,29,93,71,40,67,53,88,30,3,49,13,36,65),
	Array(52,70,95,23,4,60,11,42,69,24,68,56,1,32,56,71,37,2,36,91),
	Array(22,31,16,71,51,67,63,89,41,92,36,54,22,40,40,28,66,33,13,80),
	Array(24,47,32,60,99,3,45,2,44,75,33,53,78,36,84,20,35,17,12,50),
	Array(32,98,81,28,64,23,67,10,26,38,40,67,59,54,70,66,18,38,64,70),
	Array(67,26,20,68,2,62,12,20,95,63,94,39,63,8,40,91,66,49,94,21),
	Array(24,55,58,5,66,73,99,26,97,17,78,78,96,83,14,88,34,89,63,72),
	Array(21,36,23,9,75,0,76,44,20,45,35,14,0,61,33,97,34,31,33,95),
	Array(78,17,53,28,22,75,31,67,15,94,3,80,4,62,16,14,9,53,56,92),
	Array(16,39,5,42,96,35,31,47,55,58,88,24,0,17,54,24,36,29,85,57),
	Array(86,56,0,48,35,71,89,7,5,44,44,37,44,60,21,58,51,54,17,58),
	Array(19,80,81,68,5,94,47,69,28,73,92,13,86,52,17,77,4,89,55,40),
	Array(4,52,8,83,97,35,99,16,7,97,57,32,16,26,26,79,33,27,98,66),
	Array(88,36,68,87,57,62,20,72,3,46,33,67,46,55,12,32,63,93,53,69),
	Array(4,42,16,73,38,25,39,11,24,94,72,18,8,46,29,32,40,62,76,36),
	Array(20,69,36,41,72,30,23,88,34,62,99,69,82,67,59,85,74,4,36,16),
	Array(20,73,35,29,78,31,90,1,74,31,49,71,48,86,81,16,23,57,5,54),
	Array(1,70,54,71,83,51,54,69,16,92,33,48,61,43,52,1,89,19,67,48))
    assert(problem11(grid)===70600674)
  }


  // test("12. What is the value of the first triangle number to have over five hundred divisors?") {
  //   assert(problem12(5) === 28)
  //   assert(problem12(500) === 28)
  // }

  test("Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.") {
    assert(problem27(42) === -1*41)
    assert(problem27(1001) === -61 * 971)
  }
}



@RunWith(classOf[JUnitRunner])
class UtilsTestSuite extends FunSuite {
  import utils._
  test("IsPrime"){
    assert(isPrime(987654323) === true)
    assert(isPrime(13) === true)  
    assert(isPrime(3) === true)  
    assert(isPrime(2) === true)  
    assert(isPrime(4) === false)  
    assert(isPrime(35) === false)  
  }

  test("isPalindrom") {
    assert(isPalindrom(919)===true)
    assert(isPalindrom(918)===false)
    assert(isPalindrom(777)===true)
    assert(isPalindrom(707)===true)
    assert(isPalindrom(7)===true)
  }
}
 

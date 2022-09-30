package chapter02

object MyCalcFunctions:

    def abs(n: Int): Int = {
        if (n < 0) -n
        else n
    }

    def factorial(n: Int): Int = {
        @annotation.tailrec
        def go(n: Int, acc: Int): Int =
            if (n <= 0) acc
            else go(n - 1, acc * n)
        go(n, 1)
    }

    //    Exercise 2.1
    def fib(n: Int): Int = {
        @annotation.tailrec
        def go(n: Int, fib1: Int, fib2: Int): Int =
            if (n <= 1) fib1
            else go(n - 1, fib2, fib1 + fib2)
        go(n, 0, 1)
    }

object FormatCalcFunctions:

    import MyCalcFunctions.*

    def formatAbs(x: Int): String = {
        val msg = "The absolute value of %d is %d"
        msg.format(x, abs(x))
    }

    def formatFactorial(x: Int): String = {
        val msg = "The factorial of %d is %d"
        msg.format(x, factorial(x))
    }

    def formatFib(x: Int): String = {
        val msg = "The %dth Fibonacci number is %d"
        msg.format(x, fib(x))
    }

    def formatResult(name: String, n: Int, f: Int => Int) = {
        val msg = "The %s of %d is %d."
        msg.format(name, n, f(n))
    }

object PrintCalcFunctions:

    import MyCalcFunctions.*
    import FormatCalcFunctions.*

    @main def testCalcFunctions: Unit =
        println(formatAbs(-42))
        println(formatFactorial(7))
        println(formatFib(8))

        println(formatResult("absolute value", -42, abs))
        println(formatResult("factorial", 7, factorial))
        println(formatResult("Fibonacci value in position", 8, fib))

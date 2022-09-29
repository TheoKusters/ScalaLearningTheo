object MyModule {
    // absolute value
    def abs(n: Int) : Int = {
        if (n < 0) -n
        else n
    }

    private def formatAbs(x: Int): String = {
        val msg = "The absolute value of %d is %d"
        msg.format(x, abs(x))
    }

    def factorial(n: Int): Int = {
        @annotation.tailrec 
        def go(n: Int, acc: Int): Int =
            if (n <= 0) acc 
            else go(n - 1, acc * n)
        go(n, 1)
    }

    def main(args: Array[String]): Unit = {
        println(formatAbs(-42))
    }
}
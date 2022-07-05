package tutorial.introduction

private class OuterNested {
    private val bar: Int = 1
    class Nested {
        fun foo() = 2
    }
}

private val demo1 = OuterNested.Nested().foo() // == 2

private class OuterInner {
    private val bar: Int = 1
    inner class Inner {
        fun foo() = bar
    }
}

private val demo2 = OuterInner().Inner().foo() // == 1

fun main() {
    println("out nested: $demo1")
    println("out nested: $demo2")
}
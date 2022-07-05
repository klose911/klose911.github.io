package tutorial.`object`

import tutorial.basic.getLongestString

//fun MutableList<Int>.swap(index1: Int, index2: Int) {
//    val tmp = this[index1] // “this”对应该列表
//    this[index1] = this[index2]
//    this[index2] = tmp
//}
fun <T> MutableList<T>.swap(index1: Int, index2: Int) {
    val tmp = this[index1] // “this”对应该列表
    this[index1] = this[index2]
    this[index2] = tmp
}

fun Any?.toString(): String {
    if (this == null) return "null"
    // 空检测之后，“this”会自动转换为非空类型，所以下面的 toString()
    // 解析为 Any 类的成员函数
    return toString()
}

val <T> List<T>.lastIndex: Int
    get() = size - 1

class MyClass {
    companion object {}  // 将被称为 "Companion"
}

fun MyClass.Companion.printCompanion() {
    println("companion")
}

class Host(val hostname: String) {
    fun printHostname() {
        print(hostname)
    }
}

class Connection(val host: Host, val port: Int) {
    fun printPort() {
        print(port)
    }

    fun Host.printConnectionString() {
        printHostname()   // 调用 Host.printHostname()
        print(":")
        printPort()   // 调用 Connection.printPort()
        println()
    }

    fun Host.getConnectionString() {
        toString()         // 调用 Host.toString()
        this@Connection.toString()  // 调用 Connection.toString()
    }

    fun connect() {
        /*……*/
        host.printConnectionString()   // 调用扩展函数
    }
}

fun main() {
    val list = mutableListOf(1, 2, 3)
    list.forEach { println(it) }
    list.swap(0, 2) // “swap()”内部的“this”会保存“list”的值
    println("after swap ");
    list.forEach { println(it) }

    open class Shape
    class Rectangle : Shape()

    fun Shape.getName() = "Shape"
    fun Rectangle.getName() = "Rectangle"

    fun printClassName(s: Shape) {
        println(s.getName()) //  这里绑定的是 Shape.getName()
    }
    printClassName(Rectangle())

    class Example {
        fun printFunctionType() {
            println("Class method")
        }
    }

    fun Example.printFunctionType() {
        println("Extension function")
    }

    fun Example.printFunctionType(i: Int) {
        println("Extension function")
    }

    Example().printFunctionType() // Class method
    Example().printFunctionType(1) // Extension function

    MyClass.printCompanion() // companion

    val list2 = listOf("red", "green", "blue")
    list2.getLongestString()

    Connection(Host("kotl.in"), 443).connect()
    //Host("kotl.in").printConnectionString(443)  // 错误，该扩展函数在 Connection 外不可用

}
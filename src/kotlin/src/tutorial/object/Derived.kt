package tutorial.`object`

import kotlin.properties.Delegates
import kotlin.reflect.KProperty

interface BaseForDerived1 {
    fun printMessage()
    fun printMessageLine()
}

class BaseForDerivedImpl1(val x: Int) : BaseForDerived1 {
    override fun printMessage() {
        print(x)
    }

    override fun printMessageLine() {
        println(x)
    }
}

class DerivedForBase1(b: BaseForDerived1) : BaseForDerived1 by b {
    override fun printMessage() {
        print("abc")
    }
}

interface BaseForDerived2 {
    val message: String
    fun print()
}

class BaseForDerivedImpl2(val x: Int) : BaseForDerived2 {
    override val message = "BaseImpl: x = $x"
    override fun print() {
        println(message)
    }
}

class DerivedForBase2(b: BaseForDerived2) : BaseForDerived2 by b {
    // 在 b 的 `print` 实现中不会访问到这个属性
    override val message = "Message of Derived"
}

class Example {
    var p: String by Delegate()
}


class Delegate {
    operator fun getValue(thisRef: Any?, property: KProperty<*>): String {
        return "$thisRef, thank you for delegating '${property.name}' to me!"
    }

    operator fun setValue(thisRef: Any?, property: KProperty<*>, value: String) {
        println("$value has been assigned to '${property.name}' in $thisRef.")
    }
}

val lazyValue: String by lazy {
    println("computed!")
    "Hello"
}

class ObservableUser {
    var name: String by Delegates.observable("<no name>") { prop, old, new ->
        println("$old -> $new")
    }
}

class MapUser(val map: Map<String, Any?>) {
    val name: String by map
    val age: Int by map
}

class MutableUser(val map: MutableMap<String, Any?>) {
    var name: String by map
    var age: Int     by map
}

fun main() {
    val b1 = BaseForDerivedImpl1(10)
    DerivedForBase1(b1).printMessage() // abc
    DerivedForBase1(b1).printMessageLine() // 10

    val b2 = BaseForDerivedImpl2(10)
    val derived2 = DerivedForBase2(b2)
    derived2.print() // BaseImpl: x = 10
    println(derived2.message) //Message of Derived

    val e = Example()
    println(e.p)
    e.p = "NEW"

    println(lazyValue) // hello
    println(lazyValue) // hello

    val user = ObservableUser()
    user.name = "first" // <no name> -> first
    user.name = "second" // first -> second

    val mapUser = MapUser(
        mapOf(
            "name" to "John Doe",
            "age" to 25
        )
    )
    println(mapUser.name) // Prints "John Doe"
    println(mapUser.age)  // Prints 25

}


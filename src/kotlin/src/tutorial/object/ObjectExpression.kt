package tutorial.`object`

import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import javax.swing.JComponent

open class A(x: Int) {
    public open val y: Int = x
}

interface B { /*……*/ }

val ab: A = object : A(1), B {
    override val y = 15
}

fun foo() {
    val adHoc = object {
        var x: Int = 0
        var y: Int = 0
    }
    print(adHoc.x + adHoc.y)
}

class C {
    // 私有函数，所以其返回类型是匿名对象类型
    private fun foo() = object {
        val x: String = "x"
    }

    // 公有函数，所以其返回类型是 Any
    fun publicFoo() = object {
        val x: String = "x"
    }

    fun bar() {
        val x1 = foo().x        // 没问题
//        val x2 = publicFoo().x  // 错误：未能解析的引用“x”
    }
}

fun countClicks(window: JComponent) {
    var clickCount = 0
    var enterCount = 0

    window.addMouseListener(object : MouseAdapter() {
        override fun mouseClicked(e: MouseEvent) {
            clickCount++
        }

        override fun mouseEntered(e: MouseEvent) {
            enterCount++
        }
    })
    // ……
}

object DefaultListener : MouseAdapter() {
    override fun mouseClicked(e: MouseEvent) { /*……*/
    }

    override fun mouseEntered(e: MouseEvent) { /*……*/
    }
}

class MyClass1 {
    companion object Named {}
}

val x = MyClass1

class MyClass2 {
    companion object {}
}

val y = MyClass2

interface Factory<T> {
    fun create(): T
}

class MyClass3 {
    companion object : Factory<MyClass3> {
        override fun create(): MyClass3 = MyClass3()
    }
}

val f: Factory<MyClass3> = MyClass3


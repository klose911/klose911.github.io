package tutorial.`object`

open class Base1 {}

class Derived1 : Base1() {}

open class Base1Caller {
    open fun Base1.printFunctionInfo() {
        println("Base extension function in BaseCaller")
    }

    open fun Derived1.printFunctionInfo() {
        println("Derived extension function in BaseCaller")
    }

    fun call(b: Base1) {
        b.printFunctionInfo()   // 调用扩展函数
    }
}

class Derived1Caller : Base1Caller() {
    override fun Base1.printFunctionInfo() {
        println("Base extension function in DerivedCaller")
    }

    override fun Derived1.printFunctionInfo() {
        println("Derived extension function in DerivedCaller")
    }
}

fun main() {
    Base1Caller().call(Base1())   // “Base extension function in BaseCaller”
    Base1Caller().call(Derived1())   // “Base extension function in BaseCaller”
    Derived1Caller().call(Base1())  // “Base extension function in DerivedCaller”——分发接收者虚拟解析
    Derived1Caller().call(Derived1())  // “Base extension function in DerivedCaller”——扩展接收者静态解析
}
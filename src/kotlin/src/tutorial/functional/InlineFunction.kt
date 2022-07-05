package tutorial.functional

import javax.swing.tree.TreeNode

//inline fun <T> lock(lock: Lock, body: () -> T): T { /*……*/ }

fun ordinaryFunction(block: () -> Unit) {
    println("hi!")
}

//fun foo() {
//    ordinaryFunction {
//        return // 错误：不能使 `foo` 在此处返回
//    }
//}

inline fun inlined(block: () -> Unit) { println("hi!") }

fun foo() {
    inlined {
        return // OK：该 lambda 表达式是内联的
    }
}


inline fun <reified T> TreeNode.findParentOfType(): T? {
    var p = parent
    while (p != null && p !is T) {
        p = p.parent
    }
    return p as T?
}

inline fun <reified T> membersOf() = T::class.members

fun main(s: Array<String>) {
    foo()
    println(membersOf<StringBuilder>().joinToString("\n"))
}

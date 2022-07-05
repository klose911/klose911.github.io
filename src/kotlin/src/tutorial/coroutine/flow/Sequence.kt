package tutorial.coroutine.flow

fun foo1(): Sequence<Int> = sequence { // 序列构建器
    for (i in 1..3) {
        Thread.sleep(100) // 假装我们正在计算
        yield(i) // 产生下一个值
    }
}

fun main() {
    foo1().forEach { value -> println(value) }
}
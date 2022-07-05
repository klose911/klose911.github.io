package tutorial.coroutine.basic

import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.Test

//fun main() = runBlocking { // 开始执行主协程
//    GlobalScope.launch { // 在后台启动一个新的协程并继续
//        delay(1000L)
//        println("World!")
//    }
//    println("Hello,") // 主协程在这里会立即执行
//    delay(2000L)
//}

fun main() = runBlocking { // this: CoroutineScope
    launch { // 在 runBlocking 作用域中启动一个新协程
        delay(1000L)
        println("World!")
    }
    println("Hello,")
}

class MyTest {
    @Test
    fun testMySuspendingFunction() = runBlocking {
        //...
    }
}
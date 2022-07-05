package tutorial.coroutine.select

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.cancelChildren
import kotlinx.coroutines.channels.ReceiveChannel
import kotlinx.coroutines.channels.produce
import kotlinx.coroutines.delay
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.selects.select

fun CoroutineScope.fizz() = produce {
    while (true) {
        // 每 300 毫秒发送一个 "Fizz"
        delay(300)
        send("Fizz")
    }
}

fun CoroutineScope.buzz() = produce {
    while (true) {
        // 每 300 毫秒发送一个 "Fizz"
        delay(500)
        send("Buzz")
    }
}

suspend fun selectFizzBuzz(fizz: ReceiveChannel<String>, buzz: ReceiveChannel<String>) {
    select<Unit> { // 意味着该 select 表达式不返回任何结果
        fizz.onReceive { value -> // 这是第一个 select 子句
            println("fizz -> '$value'")
        }
        buzz.onReceive { value -> // 这是第二个 select 子句
            println("buzz -> '$value'")
        }
    }
}

fun main() = runBlocking {
    val fizz = fizz()
    val buzz = buzz()
    repeat(7) {
        selectFizzBuzz(fizz, buzz)
    }
    coroutineContext.cancelChildren() // 取消 fizz 和 buzz 协程
} 

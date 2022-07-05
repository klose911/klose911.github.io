package tutorial.coroutine.channel

import kotlinx.coroutines.cancelChildren
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking

data class Ball(var hits: Int)

fun main() = runBlocking {
    val table = Channel<Ball>() // 一个共享的 table（桌子）
    launch {
        player("ping", table)
    }

    launch {
        player("pong", table)
    }

    table.send(Ball(0)) // 乒乓球
    delay(1000) // 延迟 1 秒钟
    coroutineContext.cancelChildren() // 游戏结束，取消它们
}

suspend fun player(name: String, table: Channel<Ball>) {
    for (ball in table) {
        // 在循环中接收球
        ball.hits++
        println("$name $ball")
        delay(300) // 等待一段时间
        table.send(ball) // 将球发送回去
    }
}

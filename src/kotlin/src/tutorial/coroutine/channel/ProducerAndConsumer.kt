package tutorial.coroutine.channel

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.channels.ReceiveChannel
import kotlinx.coroutines.channels.consumeEach
import kotlinx.coroutines.channels.produce
import kotlinx.coroutines.runBlocking

fun CoroutineScope.produceSquares(): ReceiveChannel<Int> = produce {
    for (x in 1..5)
        send(x * x)
}

fun main() = runBlocking {
    val squares = produceSquares()
    squares.consumeEach {
        println(it)
    }
    println("Done!")
} 

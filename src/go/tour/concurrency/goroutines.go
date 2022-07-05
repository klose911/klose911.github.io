package main

import (
	"fmt"
	"time"
)

func say(s string) {
	for i := 0; i < 5; i++ {
		time.Sleep(100 * time.Millisecond)
		fmt.Println(s)
	}
}

// hello
// world
// world
// hello
// hello
// world
// world
// hello
// hello
func main() {
	go say("world")
	say("hello")
}

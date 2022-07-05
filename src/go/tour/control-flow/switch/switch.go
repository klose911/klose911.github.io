package main

import (
	"fmt"
	"runtime"
)

func main() {
	//Go runs on windows.
	fmt.Print("Go runs on ")
	switch os := runtime.GOOS; os {
	//不需要手动用break语句
	case "darwin":
		fmt.Println("OS X.")
	case "linux":
		fmt.Println("Linux.")
	default:
		// freebsd, openbsd,
		// plan9, windows...
		fmt.Printf("%s.", os)
	}
}

package main

import "fmt"

const Pi = 3.14

func main() {
	const World = "世界"
	//Hello 世界
	fmt.Println("Hello", World)
	//Happy 3.14 Day
	fmt.Println("Happy", Pi, "Day")
	const Truth = true
	//Go rule? true
	fmt.Println("Go rules?", Truth)
}

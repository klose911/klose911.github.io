package main

import "fmt"

func main() {
	m := make(map[string]int)
	m["Answer"] = 42
	//The value: 42
	fmt.Println("The value:", m["Answer"])

	m["Answer"] = 48
	//The value: 48
	fmt.Println("The value:", m["Answer"])

	delete(m, "Answer")
	//The value: 0
	fmt.Println("The value:", m["Answer"])

	v, ok := m["Answer"]
	//The value: 0 Present? false
	fmt.Println("The value:", v, "Present?", ok)
}

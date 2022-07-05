package main

import (
	"fmt"
	"math"
)

func pow(x, n, lim float64) float64 {
	if v := math.Pow(x, n); v < lim {
		return v
	}
	return lim
}

func main() {
	fmt.Println(
		//9
		pow(3, 2, 10),
		//20
		pow(3, 3, 20),
	)
}

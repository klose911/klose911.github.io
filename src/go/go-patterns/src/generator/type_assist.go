package main

import "fmt"

//Container is a generic container, accepting anything.
type Container []interface{}

//Put adds an element to the container.
func (c *Container) Put(elem interface{}) {
	*c = append(*c, elem)
}

//Get gets an element from the container.
func (c *Container) Get() interface{} {
	elem := (*c)[0]
	*c = (*c)[1:]
	return elem
}

func main() {
	intContainer := &Container{}
	intContainer.Put(7)
	intContainer.Put(42)

	// assert that the actual type is int
	elem, ok := intContainer.Get().(int)
	if !ok {
		fmt.Println("Unable to read an int from intContainer")
	}

	fmt.Printf("assertExample: %d (%T)\n", elem, elem)
}

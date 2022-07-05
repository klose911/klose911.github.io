#![allow(unused_variables)]
fn main() {
    let v = vec![100, 32, 57];
    // immutable iteration 
    for i in &v {
	println!("{}", i);
    }

    let mut v = vec![100, 32, 57];
    // mutable iteration 
    for i in &mut v {
	*i += 50;
    }
    for i in &v {
        println!("{}", i);
    }
    
}

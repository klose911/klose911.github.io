// can not directly return closure 
// fn returns_closure() -> Fn(i32) -> i32 {
//     |x| x + 1
// }

// we need to wrap closure in a Box<dyn> trait 
fn returns_closure() -> Box<dyn Fn(i32) -> i32> {
    Box::new(|x| x + 1)
}

fn main() {
    println!("call return closure: {}", returns_closure()(5));
}

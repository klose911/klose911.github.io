// 错误使用
// fn main() {
//     let mut s = String::from("hello");

//     let r1 = &s; // 没问题
//     let r2 = &s; // 没问题
//     let r3 = &mut s; // 大问题

//     println!("{}, {}, and {}", r1, r2, r3);
//}


// 正确使用
fn main() {
    let mut s = String::from("hello");

    let r1 = &s; // 没问题
    let r2 = &s; // 没问题
    println!("{} and {}", r1, r2);
    // 此位置之后 r1 和 r2 不再使用

    let r3 = &mut s; // 没问题
    println!("{}", r3);

}

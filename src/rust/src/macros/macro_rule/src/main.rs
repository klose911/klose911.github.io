#[macro_export]
macro_rules! myvec {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($x);
            )*
		temp_vec
        }
    };
}

fn main() {
    // let mut temp_vec = Vec::new();
    // temp_vec.push(1);
    // temp_vec.push(2);
    // temp_vec.push(3);
    // temp_vec

    let v: Vec<u32> = myvec![1, 2, 3];
}

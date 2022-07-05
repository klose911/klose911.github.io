fn main() {
    let mut v = vec![1, 2, 3, 4, 5, 6];

    let r = &mut v[..];

    //let (a, b) = r.split_at_mut(3);
    let (a, b) = my_split_at_mut(r, 3);

    assert_eq!(a, &mut [1, 2, 3]);
    assert_eq!(b, &mut [4, 5, 6]);

    // let address = 0x01234usize;
    // let r = address as *mut i32;

    // let slice: &[i32] = unsafe {
    // 	slice::from_raw_parts_mut(r, 10000)
    // };
}

// fn my_split_at_mut(slice: &mut [i32], mid: usize) -> (&mut [i32], &mut [i32]) {
//     let len = slice.len();

//     assert!(mid <= len);

//     (&mut slice[..mid],
//      &mut slice[mid..])
// }

use std::slice;

fn my_split_at_mut(slice: &mut [i32], mid: usize) -> (&mut [i32], &mut [i32]) {
    let len = slice.len();
    let ptr = slice.as_mut_ptr();

    assert!(mid <= len);

    unsafe {
        (slice::from_raw_parts_mut(ptr, mid),
         slice::from_raw_parts_mut(ptr.offset(mid as isize), len - mid))
    }
}


// NB: -Z and --pretty only work on the nightly unstable compiler :/
// so no macro help w/the release rust toolchain?
//   rustc -Z unstable-options --pretty expanded recurrence.rs

/*

// 'a is a lifetime?  could be 'mut', but what else?  IndexOffset
// needs to borrow the slice so ownership isn't _moved_, we
// parameterize that lifetime out with 'a (how long it's allowed to
// hold the reference)
struct IndexOffset<'a> {
slice: &'a [u64, 2],
offset: usize,
}

impl<'a> Index<usize> for IndexOffset<'a> {
     type Output = u64;

     #[inline(always)]
     fn index<'b>(&'b self, index: usize) -> &'b u64 {
         use std::num::Wrapping;

         let index = Wrapping(index);
         let offset = Wrapping(self.offset);
         let window = Wrapping(2);

         let real_index = index - offset + window;
         &self.slice[real_index.0]
     }
 }

 */

 /*
 macro_rules! recurrence {
     // no type specified, default to u64
     ( a[n] = $($inits:expr),+ , ... , $recur:expr ) => { /* ... */ };
     // user specified type
     ( a[n]: $sty:ty = $($inits:expr),+ , ... , $recur:expr ) => { /* ... */ };
 }
  */

// this version doesn't work :(
//  macro_rules! recurrence {
 //      // ( a[n]: $sty:ty = $($inits:expr),+  ...  $recur:expr ) => {
 //      ( a[n]: $sty:ty = $($inits:expr),+ ... $recur:expr ) => {
 //          {
 //              /*
 //              What follows here is *literally* the code from before,
 //              cut and pasted into a new position.  No other changes
 //              have been made.
 //               */

 //             use std::ops::Index;

 //             struct Recurrence {
 //                 mem: [u64; 2],
 //                 pos: usize,
 //             }

 //             struct IndexOffset<'a> {
 //                 slice: &'a [u64; 2],
 //                 offset: usize,
 //             }

 //             impl<'a> Index<usize> for IndexOffset<'a> {
 //                 type Output = u64;

 //                 #[inline(always)]
 //                 fn index<'b>(&'b self, index: usize) -> &'b u64 {
 //                 use std::num::Wrapping;

 //                 let index = Wrapping(index);
 //                 let offset = Wrapping(self.offset);
 //                 let window = Wrapping(2);

 //                 let real_index = index - offset + window;
 //                 &self.slice[real_index.0]
 //             }
 //         }

 //         impl Iterator for Recurrence {
 //             type Item = u64;

 //             #[inline]
 //             fn next(&mut self) -> Option<u64> {
 //             if self.pos < 2 {
 //                 let next_val = self.mem[self.pos];
 //                 self.pos += 1;
 //                 Some(next_val)
 //             } else {
 //                 let next_val = {
 //                     let n = self.pos;
 //                     let a = IndexOffset { slice: &self.mem, offset: n };
 //                     (a[n-1] + a[n-2])
 //                 };

 //                 {
 //                     use std::mem::swap;

 //                     let mut swap_tmp = next_val;
 //                     for i in (0..2).rev() {
 //                         swap(&mut swap_tmp, &mut self.mem[i]);
 //                     }
 //                 }

 //                 self.pos += 1;
 //                 Some(next_val)
 //             }
 //             }
 //         }

 //         Recurrence { mem: [0, 1], pos: 0 }
 //     }
 // };
 // }

 fn done_manually() {
     /*
     let fib = recurrence![a[n]: u64 = 0, 1, ..., a[n-1] + a[n-2]];

     for e in fib.take(10) { println!("{}", e) }
      */
     let fib = {
         use std::ops::Index;

         struct Recurrence {
             mem: [u64; 2],
             pos: usize,
         }

         struct IndexOffset<'a> {
             slice: &'a [u64; 2],
             offset: usize,
         }

         impl<'a> Index<usize> for IndexOffset<'a> {
             type Output = u64;

             #[inline(always)]
             fn index<'b>(&'b self, index: usize) -> &'b u64 {
                 use std::num::Wrapping;

                 let index = Wrapping(index);
                 let offset = Wrapping(self.offset);
                 let window = Wrapping(2);

                 let real_index = index - offset + window;
                 &self.slice[real_index.0]
             }
         }

         impl Iterator for Recurrence {
             type Item = u64;

             #[inline]
             fn next(&mut self) -> Option<u64> {
                 if self.pos < 2 {
                     let next_val = self.mem[self.pos];
                     self.pos += 1;
                     Some(next_val)
                 } else {
                     let next_val = {
                         let n = self.pos;
                         let a = IndexOffset {
                             slice: &self.mem,
                             offset: n,
                         };
                         (a[n - 1] + a[n - 2])
                     };

                     {
                         use std::mem::swap;

                         let mut swap_tmp = next_val;
                         for i in (0..2).rev() {
                             swap(&mut swap_tmp, &mut self.mem[i]);
                         }
                     }

                     self.pos += 1;
                     Some(next_val)
                 }
             }
         }

         Recurrence {
             mem: [0, 1],
             pos: 0,
         }
     };

     for e in fib.take(10) {
         println!("{}", e)
     }
 }

macro_rules! count_exprs {
    () => (0);
    ($head:expr) => (1);
    ($head:expr, $($tail:expr),*) => (1 + count_exprs!($($tail),*));
}

macro_rules! recurrence {
     ( $seq:ident [ $ind:ident ]: $sty:ty = $($inits:expr),+ ... $recur:expr ) => {
     //    ^~~~~~~~~~   ^~~~~~~~~~ changed
     {
         use std::ops::Index;

         const MEM_SIZE: usize = count_exprs!($($inits),+);

         struct Recurrence {
             mem: [$sty; MEM_SIZE],
             pos: usize,
         }

         struct IndexOffset<'a> {
             slice: &'a [$sty; MEM_SIZE],
             offset: usize,
         }

         impl<'a> Index<usize> for IndexOffset<'a> {
             type Output = $sty;

             #[inline(always)]
             fn index<'b>(&'b self, index: usize) -> &'b $sty {
             use std::num::Wrapping;

             let index = Wrapping(index);
             let offset = Wrapping(self.offset);
             let window = Wrapping(MEM_SIZE);

             let real_index = index - offset + window;
             &self.slice[real_index.0]
         }
     }

         impl Iterator for Recurrence {
             type Item = $sty;

             #[inline]
             fn next(&mut self) -> Option<$sty> {
             if self.pos < MEM_SIZE {
                 let next_val = self.mem[self.pos];
                 self.pos += 1;
                 Some(next_val)
             } else {
                 let next_val = {
                     let $ind = self.pos;
                     //                              ^~~~ changed
                     let $seq = IndexOffset { slice: &self.mem, offset: $ind };
                     //                              ^~~~ changed
                     $recur
                 };

                 {
                     use std::mem::swap;

                     let mut swap_tmp = next_val;
                     for i in (0..MEM_SIZE).rev() {
                         swap(&mut swap_tmp, &mut self.mem[i]);
                     }
                 }

                 self.pos += 1;
                 Some(next_val)
             }
             }
         }

         Recurrence { mem: [$($inits),+], pos: 0 }
     }
 };
}

fn main() {
    // done_manually();

    // let fib = recurrence![a[n]: u64 = 0, 1 ... a[n-1] + a[n-2]];
    let fib = recurrence![a[n]: u64 = 0, 1 ... a[n-1] + a[n-2]];
    for elt in fib.take(10) {
        println!("{}", elt);
    }


    // TODO: an example using big-int/nums
}

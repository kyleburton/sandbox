extern crate hellolib;
// extern crate hellolib;
// use self::hellolib::hellolib::gcd;

// #[cfg(test)]
// mod hello_tests {

    #[test]
    #[should_panic]
    fn test_gcd_panics_when_firstarg_zero () {
        assert_eq!(u64::max_value(), hellolib::gcd(0, 1));
    }

    #[test]
    #[should_panic]
    fn test_gcd_panics_when_secondarg_zero () {
        assert_eq!(u64::max_value(), hellolib::gcd(1, 0));
    }

    #[test]
    fn test_gcd_primes () {
        assert_eq!( 1,  hellolib::gcd(  1,    1));

        assert_eq!( 1,  hellolib::gcd(  2,    3));
        assert_eq!( 1,  hellolib::gcd(  2,    5));
        assert_eq!( 1,  hellolib::gcd(  2,    7));

        assert_eq!( 1,  hellolib::gcd(  4,    3));
        assert_eq!( 1,  hellolib::gcd(  4,    5));
        assert_eq!( 1,  hellolib::gcd(  4,    7));
        assert_eq!( 1,  hellolib::gcd(  4,   11));
        assert_eq!( 1,  hellolib::gcd(  4,   13));

        assert_eq!( 3,  hellolib::gcd(  3,    9));
        assert_eq!( 3,  hellolib::gcd(  9,   24));

        assert_eq!( 2,  hellolib::gcd( 22,   24));
        assert_eq!( 4,  hellolib::gcd(  4,   24));
        assert_eq!( 4,  hellolib::gcd( 20,   24));
        assert_eq!( 6,  hellolib::gcd(  6,   24));
        assert_eq!(12,  hellolib::gcd( 12,   24));
        assert_eq!( 8,  hellolib::gcd(  8,   24));

        assert_eq!( 9,  hellolib::gcd(  9,   27));



        assert_eq!(
            hellolib::gcd(  2 *  5 * 11 * 17,
                            3 *  7 * 13 * 19),
            1
            );

        assert_eq!(
            hellolib::gcd(  2 *  3 *  5 * 11 * 17,
                            3 *  7 * 11 * 13 * 19),
            3 * 11
            );
    }
// }

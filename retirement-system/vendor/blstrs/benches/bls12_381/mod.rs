mod ec;
#[cfg(feature = "__private_bench")]
mod fp;
#[cfg(feature = "__private_bench")]
mod fp12;
#[cfg(feature = "__private_bench")]
mod fp2;
mod scalar;

use rand_core::SeedableRng;
use rand_xorshift::XorShiftRng;

use blstrs::*;
use group::{Curve, Group};
use pairing_lib::{Engine, MillerLoopResult, MultiMillerLoop};

#[bench]
fn bench_pairing_g2_preparation(b: &mut ::test::Bencher) {
    const SAMPLES: usize = 1000;

    let mut rng = XorShiftRng::from_seed([
        0x59, 0x62, 0xbe, 0x5d, 0x76, 0x3d, 0x31, 0x8d, 0x17, 0xdb, 0x37, 0x32, 0x54, 0x06, 0xbc,
        0xe5,
    ]);

    let v: Vec<G2Projective> = (0..SAMPLES)
        .map(|_| G2Projective::random(&mut rng))
        .collect();

    let mut count = 0;
    b.iter(|| {
        let tmp = G2Prepared::from(v[count].to_affine());
        count = (count + 1) % SAMPLES;
        tmp
    });
}

#[bench]
fn bench_pairing_miller_loop(b: &mut ::test::Bencher) {
    const SAMPLES: usize = 1000;

    let mut rng = XorShiftRng::from_seed([
        0x59, 0x62, 0xbe, 0x5d, 0x76, 0x3d, 0x31, 0x8d, 0x17, 0xdb, 0x37, 0x32, 0x54, 0x06, 0xbc,
        0xe5,
    ]);

    let v: Vec<(G1Affine, G2Prepared)> = (0..SAMPLES)
        .map(|_| {
            (
                G1Affine::from(G1Projective::random(&mut rng)),
                G2Prepared::from(G2Affine::from(G2Projective::random(&mut rng))),
            )
        })
        .collect();

    let mut count = 0;
    b.iter(|| {
        let tmp = Bls12::multi_miller_loop(&[(&v[count].0, &v[count].1)]);
        count = (count + 1) % SAMPLES;
        tmp
    });
}

#[bench]
fn bench_pairing_final_exponentiation(b: &mut ::test::Bencher) {
    const SAMPLES: usize = 1000;

    let mut rng = XorShiftRng::from_seed([
        0x59, 0x62, 0xbe, 0x5d, 0x76, 0x3d, 0x31, 0x8d, 0x17, 0xdb, 0x37, 0x32, 0x54, 0x06, 0xbc,
        0xe5,
    ]);

    let v: Vec<_> = (0..SAMPLES)
        .map(|_| {
            (
                G1Affine::from(G1Projective::random(&mut rng)),
                G2Prepared::from(G2Affine::from(G2Projective::random(&mut rng))),
            )
        })
        .map(|(ref p, ref q)| Bls12::multi_miller_loop(&[(p, q)]))
        .collect();

    let mut count = 0;
    b.iter(|| {
        let tmp = v[count].final_exponentiation();
        count = (count + 1) % SAMPLES;
        tmp
    });
}

#[bench]
fn bench_pairing_full(b: &mut ::test::Bencher) {
    const SAMPLES: usize = 1000;

    let mut rng = XorShiftRng::from_seed([
        0x59, 0x62, 0xbe, 0x5d, 0x76, 0x3d, 0x31, 0x8d, 0x17, 0xdb, 0x37, 0x32, 0x54, 0x06, 0xbc,
        0xe5,
    ]);

    let v: Vec<(G1Affine, G2Affine)> = (0..SAMPLES)
        .map(|_| {
            (
                G1Projective::random(&mut rng).to_affine(),
                G2Projective::random(&mut rng).to_affine(),
            )
        })
        .collect();

    let mut count = 0;
    b.iter(|| {
        let tmp = Bls12::pairing(&v[count].0, &v[count].1);
        count = (count + 1) % SAMPLES;
        tmp
    });
}

#[bench]
fn bench_g1_multi_exp_naive(b: &mut ::test::Bencher) {
    use ff::Field;
    const SIZE: usize = 256;

    let mut rng = XorShiftRng::from_seed([
        0x59, 0x62, 0xbe, 0x5d, 0x76, 0x3d, 0x31, 0x8d, 0x17, 0xdb, 0x37, 0x32, 0x54, 0x06, 0xbc,
        0xe5,
    ]);

    let points: Vec<G1Projective> = (0..SIZE).map(|_| G1Projective::random(&mut rng)).collect();
    let scalars: Vec<Scalar> = (0..SIZE).map(|_| Scalar::random(&mut rng)).collect();

    b.iter(|| {
        let mut acc = points[0] * scalars[0];
        for i in 1..SIZE {
            acc += points[i] * scalars[i];
        }
    });
}

#[bench]
fn bench_g1_multi_exp(b: &mut ::test::Bencher) {
    use ff::Field;
    const SIZE: usize = 256;

    let mut rng = XorShiftRng::from_seed([
        0x59, 0x62, 0xbe, 0x5d, 0x76, 0x3d, 0x31, 0x8d, 0x17, 0xdb, 0x37, 0x32, 0x54, 0x06, 0xbc,
        0xe5,
    ]);

    let points: Vec<G1Projective> = (0..SIZE).map(|_| G1Projective::random(&mut rng)).collect();
    let scalars: Vec<Scalar> = (0..SIZE).map(|_| Scalar::random(&mut rng)).collect();

    b.iter(|| G1Projective::multi_exp(points.as_slice(), scalars.as_slice()));
}

#[bench]
fn bench_g2_multi_exp_naive(b: &mut ::test::Bencher) {
    use ff::Field;
    const SIZE: usize = 256;

    let mut rng = XorShiftRng::from_seed([
        0x59, 0x62, 0xbe, 0x5d, 0x76, 0x3d, 0x31, 0x8d, 0x17, 0xdb, 0x37, 0x32, 0x54, 0x06, 0xbc,
        0xe5,
    ]);

    let points: Vec<G2Projective> = (0..SIZE).map(|_| G2Projective::random(&mut rng)).collect();
    let scalars: Vec<Scalar> = (0..SIZE).map(|_| Scalar::random(&mut rng)).collect();

    b.iter(|| {
        let mut acc = points[0] * scalars[0];
        for i in 1..SIZE {
            acc += points[i] * scalars[i];
        }
    });
}

#[bench]
fn bench_g2_multi_exp(b: &mut ::test::Bencher) {
    use ff::Field;
    const SIZE: usize = 256;

    let mut rng = XorShiftRng::from_seed([
        0x59, 0x62, 0xbe, 0x5d, 0x76, 0x3d, 0x31, 0x8d, 0x17, 0xdb, 0x37, 0x32, 0x54, 0x06, 0xbc,
        0xe5,
    ]);

    let points: Vec<G2Projective> = (0..SIZE).map(|_| G2Projective::random(&mut rng)).collect();
    let scalars: Vec<Scalar> = (0..SIZE).map(|_| Scalar::random(&mut rng)).collect();

    b.iter(|| G2Projective::multi_exp(points.as_slice(), scalars.as_slice()));
}

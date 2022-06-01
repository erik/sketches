use std::fmt::Debug;

pub trait Coordinate {
    fn dist_to(&self, other: &Self) -> f32;
}

// Implemented using a Vantage Point tree
// TODO: Fix this - sometimes get nearest(x) != x
#[derive(Debug)]
pub struct SpatialIndex<Coord, Ix> {
    tree: Vec<(Coord, Ix)>,
}

const MAX_POINTS_PER_LEAF_NODE: usize = 8;

impl<Coord, Ix> SpatialIndex<Coord, Ix>
where
    Ix: Copy + Debug,
    Coord: Coordinate + Copy + Debug,
{
    pub fn build(points: Vec<(Coord, Ix)>) -> SpatialIndex<Coord, Ix> {
        assert!(points.len() > 0, "empty data");

        let initial_vp = &points[0].0.clone();

        let mut points_with_dist: Vec<_> = points
            .into_iter()
            .map(|(pt, ix)| (pt.dist_to(initial_vp), (pt, ix)))
            .collect();

        Self::build_inner(&mut points_with_dist[..]);

        let tree = points_with_dist.into_iter().map(|(_, p)| p).collect();

        SpatialIndex { tree }
    }

    fn build_inner(data: &mut [(f32, (Coord, Ix))]) {
        if data.len() <= MAX_POINTS_PER_LEAF_NODE {
            return;
        }

        let pivot_idx = data.len() / 2;

        // `select_nth_unstable_by` partially sorts the slice, so that
        // `data[pivot_idx]` is left in the correct sorted position.
        //
        // The rest of the data is partitioned into buckets of "less
        // than pivot" and "greater than pivot"
        //
        // TODO: Why pivot_idx - 1?
        let _ = data[1..]
            .select_nth_unstable_by(pivot_idx - 1, |(a, _), (b, _)| a.partial_cmp(b).unwrap());

        // TODO: avoid clone
        let new_vp = &data[pivot_idx].1.clone();
        for (dist, (point, _ix)) in &mut data[pivot_idx + 1..] {
            *dist = point.dist_to(&new_vp.0);
        }

        // Left children: inside current vantage point
        Self::build_inner(&mut data[..pivot_idx]);
        // Right children: outside vantage point
        Self::build_inner(&mut data[pivot_idx..]);
    }

    pub fn find_nearest_within(&self, point: &Coord, radius: f32) -> Option<Ix> {
        let mut nearest_so_far = (radius, None);
        Self::find_nearest_inner(&self.tree, point, &mut nearest_so_far);
        return nearest_so_far.1.map(|index| index);
    }

    fn find_nearest_inner(
        tree: &[(Coord, Ix)],
        query_point: &Coord,
        nearest_so_far: &mut (f32, Option<Ix>),
    ) {
        if tree.len() <= MAX_POINTS_PER_LEAF_NODE {
            for &(point, index) in &tree[..] {
                let dist = query_point.dist_to(&point);

                if dist < nearest_so_far.0 {
                    *nearest_so_far = (dist, Some(index));
                }
            }
        } else {
            let (vantage_point, _ix) = &tree[0];

            let pivot_idx = tree.len() / 2;

            let dist_to_query = vantage_point.dist_to(query_point);
            let dist_to_boundary = vantage_point.dist_to(&tree[pivot_idx].0);

            if dist_to_query >= dist_to_boundary {
                Self::find_nearest_inner(&tree[pivot_idx..], query_point, nearest_so_far);

                if dist_to_query - dist_to_boundary < nearest_so_far.0 {
                    Self::find_nearest_inner(&tree[..pivot_idx], query_point, nearest_so_far);
                }
            } else {
                Self::find_nearest_inner(&tree[..pivot_idx], query_point, nearest_so_far);

                if dist_to_boundary - dist_to_query < nearest_so_far.0 {
                    Self::find_nearest_inner(&tree[pivot_idx..], query_point, nearest_so_far);
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use rand::{prelude::*, rngs::mock::StepRng, Rng};

    impl Coordinate for i32 {
        fn dist_to(&self, other: &Self) -> f32 {
            (self - other).abs() as f32
        }
    }

    impl Coordinate for (i32, i32) {
        fn dist_to(&self, other: &Self) -> f32 {
            let a = self.0 - other.0;
            let b = self.1 - other.1;

            ((a * a + b * b) as f32).sqrt()
        }
    }

    #[test]
    fn construct_1d_index() {
        let mut rng = StepRng::new(0, 3);

        let mut input: [i32; 30] = rng.gen();
        input.shuffle(&mut rng);
        for i in &mut input {
            *i *= rng.gen::<i32>();
        }

        let with_index = input.iter().enumerate().map(|(ix, x)| (*x, ix)).collect();
        let index = SpatialIndex::build(with_index);

        println!("index = {:?}", index);

        for x in &input {
            let neighbor = index.find_nearest_within(x, 10.0);
            assert_eq!(neighbor.map(|i| input[i]), Some(*x));
        }
    }

    #[test]
    fn construct_2d_index() {
        let mut rng = StepRng::new(0, 111);

        let mut input: [(i32, i32); 31] = rng.gen();
        input.shuffle(&mut rng);

        println!("input {:?}", input);

        let index = SpatialIndex::build(input.iter().enumerate().map(|(ix, x)| (*x, ix)).collect());

        for x in &input[..] {
            println!("find nearest to {:?}", x);
            let nearest_ix = index.find_nearest_within(x, 1000.0);

            assert_eq!(nearest_ix.map(|ix| input[ix]).unwrap(), *x);
        }
    }
}

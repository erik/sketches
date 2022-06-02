use std::fmt::Debug;

pub trait Euclidean<Dist> {
    fn distance(self, other: Self) -> Dist;
}

pub type Point1D = f32;
pub type Point2D = (f32, f32);

impl Euclidean<f32> for Point1D {
    fn distance(self, other: Self) -> f32 {
        (self - other).abs()
    }
}

impl Euclidean<f32> for Point2D {
    fn distance(self, other: Self) -> f32 {
        let dx = self.0 - other.0;
        let dy = self.1 - other.1;

        (dx * dx + dy * dy).sqrt()
    }
}

struct IndexedCoordinate<C, I> {
    index: I,
    coord: C,
}

// Implemented using a Vantage Point tree
// TODO: Fix this - sometimes get nearest(x) != x
#[derive(Debug)]
pub struct SpatialIndex<C, I> {
    tree: Vec<(C, I)>,
}

const MAX_POINTS_PER_LEAF_NODE: usize = 8;

impl<C, I> SpatialIndex<C, I>
where
    I: Copy + Debug,
    C: Euclidean<f32> + Copy + Debug,
{
    pub fn build<T>(points: Vec<(T, I)>) -> SpatialIndex<C, I>
    where
        T: Into<C> + Copy,
    {
        assert!(points.len() > 0, "empty data");

        // For simplicity: use the first element of the list as our
        // first vantage point.
        let vp: C = points[0].0.into();
        let mut points_with_dist: Vec<_> = points
            .into_iter()
            .map(|(pt, ix)| {
                let coord = pt.into();
                (vp.distance(coord), (coord, ix))
            })
            .collect();

        Self::build_inner(&mut points_with_dist[..]);

        let tree = points_with_dist.into_iter().map(|(_, p)| p).collect();

        SpatialIndex { tree }
    }

    fn build_inner(data: &mut [(f32, (C, I))]) {
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

        let new_vp = data[pivot_idx].1;
        for (dist, (point, _ix)) in &mut data[pivot_idx + 1..] {
            *dist = point.distance(new_vp.0);
        }

        // Left children: inside current vantage point
        Self::build_inner(&mut data[..pivot_idx]);
        // Right children: outside vantage point
        Self::build_inner(&mut data[pivot_idx..]);
    }

    pub fn find_nearest_within(&self, point: &C, radius: f32) -> Option<I> {
        let mut nearest_so_far = (radius, None);
        Self::find_nearest_inner(&self.tree, *point, &mut nearest_so_far);
        return nearest_so_far.1.map(|index| index);
    }

    fn find_nearest_inner(tree: &[(C, I)], query_point: C, nearest_so_far: &mut (f32, Option<I>)) {
        if tree.len() <= MAX_POINTS_PER_LEAF_NODE {
            for &(point, index) in &tree[..] {
                let dist = query_point.distance(point);

                if dist < nearest_so_far.0 {
                    *nearest_so_far = (dist, Some(index));
                }
            }
        } else {
            let (vantage_point, _ix) = &tree[0];

            let pivot_idx = tree.len() / 2;

            let dist_to_query = vantage_point.distance(query_point);
            let dist_to_boundary = vantage_point.distance(tree[pivot_idx].0);

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

    #[test]
    fn construct_1d_index() {
        let mut rng = StepRng::new(0, 3);

        let mut input: [f32; 30] = rng.gen();
        input.shuffle(&mut rng);
        for i in &mut input {
            *i *= rng.gen::<i32>() as f32;
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

        let mut input: [(f32, f32); 31] = rng.gen();
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

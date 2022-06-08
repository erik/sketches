use std::fmt::Debug;

pub type Point1D = f32;
pub type Point2D = (f32, f32);

#[derive(Debug, Clone, Copy)]
pub struct IndexedCoordinate<I, C> {
    index: I,
    coord: C,
}

// Implemented using a Vantage Point tree
// TODO: Fix this - sometimes get nearest(x) != x
#[derive(Debug)]
pub struct SpatialIndex<I, C> {
    tree: Vec<IndexedCoordinate<I, C>>,
}

pub trait Euclidean {
    fn distance(self, other: Self) -> f32;
}

impl Euclidean for Point1D {
    fn distance(self, other: Self) -> f32 {
        (self - other).abs()
    }
}

impl Euclidean for Point2D {
    fn distance(self, other: Self) -> f32 {
        let dx = self.0 - other.0;
        let dy = self.1 - other.1;

        dx.hypot(dy)
    }
}

impl<I, C: Euclidean> Euclidean for IndexedCoordinate<I, C> {
    fn distance(self, other: Self) -> f32 {
        self.coord.distance(other.coord)
    }
}

impl<I, C> IndexedCoordinate<I, C> {
    pub fn new(index: I, coord: C) -> Self {
        IndexedCoordinate { index, coord }
    }
}

const MAX_POINTS_PER_LEAF_NODE: usize = 8;

impl<I, C> SpatialIndex<I, C>
where
    I: Copy + Debug,
    C: Euclidean + Copy + Debug,
{
    pub fn build(points: &[IndexedCoordinate<I, C>]) -> SpatialIndex<I, C> {
        assert!(!points.is_empty(), "empty data");

        // For simplicity: use the first element of the list as our
        // first vantage point, and calculate the distance from it to
        // every other point.
        let vp = points[0];
        let mut dist_from_vp: Vec<_> = points.iter().map(|&pt| (vp.distance(pt), pt)).collect();

        Self::build_inner(&mut dist_from_vp);

        let tree = dist_from_vp.into_iter().map(|(_dist, pt)| pt).collect();

        SpatialIndex { tree }
    }

    fn build_inner(data: &mut [(f32, IndexedCoordinate<I, C>)]) {
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

        let (_, vp) = data[pivot_idx];
        for (dist, coord) in &mut data[pivot_idx + 1..] {
            *dist = vp.distance(*coord);
        }

        // Left children: inside current vantage point
        Self::build_inner(&mut data[..pivot_idx]);
        // Right children: outside vantage point
        Self::build_inner(&mut data[pivot_idx..]);
    }

    pub fn find_nearest_within(&self, point: C, radius: f32) -> Option<I> {
        let mut nearest_so_far = (radius, None);
        Self::find_nearest_inner(&self.tree, point, &mut nearest_so_far);

        nearest_so_far.1
    }

    fn find_nearest_inner(
        tree: &[IndexedCoordinate<I, C>],
        query_point: C,
        nearest_so_far: &mut (f32, Option<I>),
    ) {
        if tree.len() <= MAX_POINTS_PER_LEAF_NODE {
            for &pt in &tree[..] {
                let dist = query_point.distance(pt.coord);

                if dist < nearest_so_far.0 {
                    *nearest_so_far = (dist, Some(pt.index));
                }
            }
        } else {
            let vantage_point = tree[0];

            let pivot_idx = tree.len() / 2;

            let dist_to_query = vantage_point.coord.distance(query_point);
            let dist_to_boundary = vantage_point.distance(tree[pivot_idx]);

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
        println!("input {:?}", input);

        let mut with_index: Vec<_> = input
            .iter()
            .enumerate()
            .map(|(i, x)| IndexedCoordinate::new(i, *x))
            .collect();
        let index = SpatialIndex::build(&mut with_index);

        println!("index = {:?}", index);

        for &x in &input {
            let neighbor = index.find_nearest_within(x, 10.0);
            assert_eq!(neighbor.map(|i| input[i]), Some(x));
        }
    }

    #[test]
    fn construct_2d_index() {
        let mut rng = StepRng::new(0, 111);

        let mut input: [(f32, f32); 31] = rng.gen();
        input.shuffle(&mut rng);

        println!("input {:?}", input);

        let mut with_index: Vec<_> = input
            .iter()
            .enumerate()
            .map(|(i, x)| IndexedCoordinate::new(i, *x))
            .collect();
        let index = SpatialIndex::build(&mut with_index);

        for &x in &input[..] {
            println!("find nearest to {:?}", x);
            let nearest_ix = index.find_nearest_within(x, 10.0);

            assert_eq!(nearest_ix.map(|ix| input[ix]).unwrap(), x);
        }
    }
}

pub trait Coordinate {
    fn dist_to(&self, other: &Self) -> u32;
}

// Implemented using a Vantage Point tree
#[derive(Debug)]
pub struct SpatialIndex<Coord, Ix> {
    tree: Vec<(Coord, Ix)>,
}

const MAX_POINTS_PER_LEAF_NODE: usize = 8;

impl<Coord, Ix> SpatialIndex<Coord, Ix>
where
    Ix: Copy + std::fmt::Debug,
    Coord: Coordinate + Copy + std::fmt::Debug,
{
    pub fn build(points: Vec<(Coord, Ix)>) -> SpatialIndex<Coord, Ix> {
        assert!(points.len() > 0, "empty data");

        let initial_pivot = &points[0].0.clone();

        let mut points_with_dist: Vec<_> = points
            .into_iter()
            .map(|(pt, ix)| (pt.dist_to(initial_pivot), (pt, ix)))
            .collect();

        Self::build_inner(&mut points_with_dist[1..]);

        let tree = points_with_dist.into_iter().map(|(_, p)| p).collect();

        SpatialIndex { tree }
    }

    fn build_inner(data: &mut [(u32, (Coord, Ix))]) {
        if data.len() <= MAX_POINTS_PER_LEAF_NODE {
            return;
        }

        let pivot_idx = data.len() / 2;

        // `select_nth_unstable_by` partially sorts the slice, so that
        // `pivot_idx` is left in the correct sorted position.
        //
        // The rest of the data is partitioned into buckets of "less
        // than pivot" and "greater than pivot"
        let (left, pivot, right) =
            data.select_nth_unstable_by(pivot_idx, |(a, _), (b, _)| a.cmp(b));

        // Inside
        Self::build_inner(left);

        for (dist, (point, _ix)) in &mut right[..] {
            *dist = point.dist_to(&pivot.1 .0);
        }

        // Outside
        Self::build_inner(right);
    }

    pub fn find_nearest(&self, point: &Coord) -> Option<Ix> {
        let mut nearest_so_far = (u32::MAX, None);
        Self::find_nearest_inner(&self.tree, point, &mut nearest_so_far);

        // TODO: Check dist within bounds
        return nearest_so_far.1.map(|index| index);
    }

    fn find_nearest_inner(
        tree: &[(Coord, Ix)],
        query_point: &Coord,
        nearest_so_far: &mut (u32, Option<Ix>),
    ) {
        if tree.len() <= MAX_POINTS_PER_LEAF_NODE {
            println!("leaf: {:?}", tree);
            for &(point, index) in &tree[..] {
                let dist = query_point.dist_to(&point);

                if dist < nearest_so_far.0 {
                    *nearest_so_far = (dist, Some(index));
                }
            }
        } else {
            let (pivot, _ix) = &tree[0];

            let mid_point = tree.len() / 2;

            let dist_to_query = pivot.dist_to(query_point);
            let dist_to_boundary = pivot.dist_to(&tree[mid_point].0);

            if dist_to_query >= dist_to_boundary {
                Self::find_nearest_inner(&tree[mid_point..], query_point, nearest_so_far);

                if dist_to_query - dist_to_boundary < nearest_so_far.0 {
                    Self::find_nearest_inner(&tree[..mid_point], query_point, nearest_so_far);
                }
            } else {
                Self::find_nearest_inner(&tree[..mid_point], query_point, nearest_so_far);

                if dist_to_boundary - dist_to_query < nearest_so_far.0 {
                    Self::find_nearest_inner(&tree[mid_point..], query_point, nearest_so_far);
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    impl Coordinate for f32 {
        fn dist_to(&self, other: &Self) -> u32 {
            (self - other).abs() as u32
        }
    }

    impl Coordinate for (f32, f32) {
        fn dist_to(&self, other: &Self) -> u32 {
            let a = self.0 - other.0;
            let b = self.1 - other.1;

            (a * a + b * b).sqrt() as u32
        }
    }

    // #[test]
    // fn construct_tree() {
    //     let input = vec![1.0, 5.0, 2.0, 3.0, 9.0, 100.0, 2.0, 50.0]
    //         .iter()
    //         .enumerate()
    //         .map(|(ix, x)| (*x, ix))
    //         .collect();
    //     let index = SpatialIndex::build(input);

    //     println!("index = {:?}", index);

    //     let neighbor = index.find_nearest(&76.0);
    //     assert_eq!(neighbor.unwrap(), 0);

    //     let neighbor = index.find_nearest(&6.0);
    //     assert_eq!(neighbor.unwrap(), 0);
    // }

    #[test]
    fn construct_2d_index() {
        let input = vec![
            (0.0, 100.0),
            (0.0, 0.0),
            (1000.0, 1000.0),
            (1000.0, 200.0),
            (1000.0, 500.0),
            (50.0, 50.0),
            (1000.0, 900.0),
            (1000.0, 700.0),
            (1000.0, 800.0),
            (80.0, 80.0),
            (1000.0, 600.0),
            (1000.0, 100.0),
            (1000.0, 300.0),
            (100.0, 100.0),
            (1000.0, 400.0),
        ];

        let index = SpatialIndex::build(input.iter().enumerate().map(|(ix, x)| (*x, ix)).collect());

        for x in &input[..] {
            println!("find nearest to {:?}", x);
            let nearest_ix = index.find_nearest(x);

            assert_eq!(nearest_ix.map(|ix| input[ix]).unwrap(), *x);
            println!("\n\n\n\n\n");
        }
        for x in &input[..] {
            println!("find nearest to {:?}", x);
            let nearest_ix = index.find_nearest(&(x.0 - 10.0, x.1 + 10.0));

            assert_eq!(nearest_ix.map(|ix| input[ix]).unwrap(), *x);
            println!("\n\n\n\n\n");
        }
    }
}
